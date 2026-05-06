use anyhow::{Context, Result};
use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use flate2::read::GzDecoder;
use rand::{RngExt, SeedableRng};
use std::{
    env, fs,
    hint::black_box,
    path::{Path, PathBuf},
    thread,
};
use tar::Archive;

use aetherus_events::prelude::*;
use aetherus_events::read::read_ledger;

fn get_benches_dir() -> PathBuf {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    PathBuf::from(manifest_dir).join("benches")
}

fn decompress_ledger(benches_dir: &Path) -> Result<()> {
    let data_dir = benches_dir.join("data");
    let tar_gz_path = data_dir.join("simulation_ledger.tar.gz");
    let output_path = data_dir.join("simulation_ledger.json");

    if output_path.exists() {
        return Ok(());
    }

    let tar_gz = fs::File::open(tar_gz_path).context("Failed to open simulation_ledger.tar.gz")?;
    let tar = GzDecoder::new(tar_gz);
    let mut archive = Archive::new(tar);
    archive
        .unpack(&data_dir)
        .context("Failed to extract simulation_ledger.tar.gz")?;
    Ok(())
}

fn criterion_benchmark(c: &mut Criterion) {
    let benches_dir = get_benches_dir();

    // Decompress ledger once before benchmarking
    decompress_ledger(&benches_dir).expect("Failed to decompress ledger");

    let data_dir = benches_dir.join("data");
    let ledger_path = data_dir.join("simulation_ledger.json");

    // Benchmark 1: Deserialize JSON file into Ledger Tree
    c.bench_function("deserialize", |b| {
        b.iter(|| {
            let ledger = read_ledger(&ledger_path).expect("Failed to read ledger file");
            black_box(ledger);
        })
    });

    // Read ledger
    let ledger = read_ledger(&ledger_path).expect("Failed to read ledger file");
    let ledger_tree: LedgerTree = ledger.into();

    // Top 20 most frequent UIDs in the ledger (these would be determined by analyzing the ledger beforehand)
    let uids = vec![
        Uid::new(4014, 0x05000000_u32), // 64
        Uid::new(2573, 0x05000000_u32), // 68
        Uid::new(5521, 0x05000000_u32), // 77
        Uid::new(4174, 0x05000000_u32), // 83
        Uid::new(7977, 0x05000000_u32), // 83
        Uid::new(5520, 0x05000000_u32), // 107
        Uid::new(3928, 0x05000000_u32), // 109
        Uid::new(5459, 0x05000000_u32), // 118
        Uid::new(1851, 0x05000000_u32), // 125
        Uid::new(4088, 0x05000000_u32), // 129
        Uid::new(3127, 0x05000000_u32), // 136
        Uid::new(595,  0x05000000_u32), // 156
        Uid::new(2249, 0x05000000_u32), // 189
        Uid::new(426,  0x05000000_u32), // 200
        Uid::new(514,  0x05000000_u32), // 223
        Uid::new(1289, 0x05000000_u32), // 230
        Uid::new(659,  0x05000000_u32), // 239
        Uid::new(22,   0x05000000_u32), // 252
        Uid::new(1315, 0x05000000_u32), // 277
        Uid::new(494,  0x05000000_u32), // 5273
    ];

    // Benchmark 2: Build reconstruct chains for top 20 most frequent UIDs
    c.bench_function("get_chains", |b| {
        b.iter(|| {
            let chains: Vec<_> = uids
                .iter()
                .map(|uid| ledger_tree.get_chain(black_box(uid)))
                .collect();
            black_box(chains);
        })
    });

    let events: Vec<u32> = vec![1, 42, 73, 105, 156, 209, 312, 415, 523, 634, 789, 890];
    // Use a random algorithm that is deterministic with a given seed
    let mut rng = rand::rngs::StdRng::from_seed([42u8; 32]);

    // Benchmark 3: Build a new ledger with random addition of events to existing events
    let indices: Vec<usize> = (0..100_000)
        .map(|_| rng.random_range(1..events.len()))
        .collect();
    let weights: Vec<f64> = (0..100_000)
        .map(|_| rng.random::<f64>())
        .collect();
    c.bench_function("allocate", |b| {
        b.iter_batched(|| (indices.clone(), weights.clone()), |(indices, weights)| {
                let ledger = LedgerTree::new();
                let start_node = ledger.root().insert(events[0]);
                let mut prev_node = start_node.clone();
                let mut weight: f64 = 1.0;
                let min_weight = 0.01;

                for idx in indices {
                    let event = events[idx];
                    prev_node = prev_node.insert(event);

                    weight *= weights[idx];
                    if weight < min_weight {
                        // Restart a new chain from the start UID
                        weight = 1.0;
                        prev_node = start_node.clone();
                    }
                }
                let _ = black_box(ledger);
            },
            BatchSize::SmallInput
        )
    });

    // Benchmark 4: Allocate ledger multi-threaded
    // This benchmark simulates multiple threads concurrently adding events to the ledger,
    // which is the access pattern from Aetherus MCRT simulation
    let num_threads = 8;
    let events_per_thread = 50_000;
    let mut indices_per_thread = Vec::with_capacity(num_threads);
    let mut weights_per_thread = Vec::with_capacity(num_threads);

    for thread_id in 0..num_threads {
        let mut rng = rand::rngs::StdRng::from_seed([42u8 + (thread_id as u8); 32]);
        let indices: Vec<usize> = (0..events_per_thread)
            .map(|_| rng.random_range(1..events.len()))
            .collect();
        let weights: Vec<f64> = (0..events_per_thread)
            .map(|_| rng.random::<f64>())
            .collect();
        indices_per_thread.push(indices);
        weights_per_thread.push(weights);
    }
    c.bench_function("allocate_multithreaded", |b| {
        b.iter_batched(
            || (indices_per_thread.clone(), weights_per_thread.clone()),
            |(indices_per_thread, weights_per_thread)| {
                let ledger = LedgerTree::new();
                let mut handles = Vec::new();
                let start_node = ledger.root().insert(events[0]);

                for ((indices, weights), _thread_id) in indices_per_thread
                    .into_iter()
                    .zip(weights_per_thread.into_iter())
                    .zip(0..num_threads)
                {
                    let events_clone = events.clone();
                    let start_node = start_node.clone();

                    let handle = thread::spawn(move || {
                        let mut prev_node = start_node.clone();
                        let mut weight: f64 = 1.0;
                        let min_weight = 0.01;

                        for (idx, w) in indices.into_iter().zip(weights.into_iter()) {
                            let event = events_clone[idx];
                            prev_node = prev_node.insert(event);

                            weight *= w;
                            if weight < min_weight {
                                weight = 1.0;
                                prev_node = start_node.clone();
                            }
                        }
                    });
                    handles.push(handle);
                }

                for handle in handles {
                    handle.join().unwrap();
                }

                let _ = black_box(ledger);
            },
            BatchSize::SmallInput
        )
    });

    let ledger = {
        let ledger = LedgerTree::new();
        let start_node = ledger.root().insert(events[0]);
        let mut prev_node = start_node.clone();
        let mut weight: f64 = 1.0;
        let min_weight = 0.01;

        for _ in 0..400_000 {
            let event = events[rng.random_range(1..events.len())];
            prev_node = prev_node.insert(event);

            weight *= rng.random::<f64>();
            if weight < min_weight {
                // Restart a new chain from the start UID
                weight = 1.0;
                prev_node = start_node.clone();
            }
        }
        ledger
    };

    c.bench_function("resolve", |b| {
        b.iter_batched(|| ledger.clone(), // not timed
            |mut ledger| {
                ledger.resolve(); // timed
                black_box(ledger);
            },
            criterion::BatchSize::SmallInput,
        );
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
