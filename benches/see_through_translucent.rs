use anyhow::{Context, Result};
use criterion::{Criterion, criterion_group, criterion_main};
use flate2::read::GzDecoder;
use rand::{RngExt, SeedableRng};
use std::{
    env, fs,
    hint::black_box,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
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

    // Top 20 most frequent UIDs in the ledger (these would be determined by analyzing the ledger beforehand)
    let uids = vec![
        Uid::new(138, 0x05000000),
        Uid::new(464, 0x05000000),
        Uid::new(547, 0x05000000),
        Uid::new(403, 0x05000000),
        Uid::new(1311, 0x05000000),
        Uid::new(504, 0x05000000),
        Uid::new(804, 0x05000000),
        Uid::new(289, 0x05000000),
        Uid::new(967, 0x05000000),
        Uid::new(180, 0x05000000),
        Uid::new(860, 0x05000000),
        Uid::new(1142, 0x05000000),
        Uid::new(1353, 0x05000000),
        Uid::new(1768, 0x05000000),
        Uid::new(935, 0x05000000),
        Uid::new(1169, 0x05000000),
        Uid::new(1027, 0x05000000),
        Uid::new(5102, 0x05000000),
        Uid::new(2176, 0x05000000),
        Uid::new(10629, 0x05000000),
    ];

    // Benchmark 2: Build reconstruct chains for top 20 most frequent UIDs
    c.bench_function("get_chains", |b| {
        b.iter(|| {
            let chains: Vec<_> = uids
                .iter()
                .map(|uid| ledger.get_chain(black_box(*uid)))
                .collect();
            black_box(chains);
        })
    });

    let events: Vec<u32> = vec![1, 42, 73, 105, 156, 209, 312, 415, 523, 634, 789, 890];
    // Use a random algorithm that is deterministic with a given seed
    let mut rng = rand::rngs::StdRng::from_seed([42u8; 32]);

    // Benchmark 3: Build a new ledger with random addition of events to existing events
    c.bench_function("allocate", |b| {
        b.iter(|| {
            let mut ledger = Ledger::new();
            let start_uid = ledger.insert_start(events[0]);
            let mut prev_uid = start_uid;
            let mut weight: f64 = 1.0;
            let min_weight = 0.01;

            for _ in 0..100_000 {
                let event = events[rng.random_range(0..events.len())];
                prev_uid = ledger.insert(prev_uid, event);

                weight *= rng.random::<f64>();
                if weight < min_weight {
                    // Restart a new chain from the start UID
                    weight = 1.0;
                    prev_uid = start_uid;
                }
            }
            let _ = black_box(ledger);
        })
    });

    // Benchmark 4: Allocate ledger multi-threaded
    // This benchmark simulates multiple threads concurrently adding events to the ledger,
    // which is the access pattern from Aetherus MCRT simulation
    c.bench_function("allocate_multithreaded", |b| {
        b.iter(|| {
            let ledger = Arc::new(Mutex::new(Ledger::new()));
            let num_threads = 8;
            let events_per_thread = 50_000;
            let mut handles = Vec::new();
            let start_uid = {
                let mut ledger_guard = ledger.lock().unwrap();
                ledger_guard.insert_start(events[0])
            };

            for thread_id in 0..num_threads {
                let ledger_clone = Arc::clone(&ledger);
                let events_clone = events.clone();
                let mut rng = rand::rngs::StdRng::from_seed([42u8 + thread_id; 32]);

                let handle = thread::spawn(move || {
                    let mut prev_uid = start_uid;
                    let mut weight: f64 = 1.0;
                    let min_weight = 0.01;

                    for _ in 0..events_per_thread {
                        let event = events_clone[rng.random_range(0..events_clone.len())];
                        prev_uid = {
                            let mut ledger = ledger_clone.lock().unwrap();
                            ledger.insert(prev_uid, event)
                        };

                        weight *= rng.random::<f64>();
                        if weight < min_weight {
                            weight = 1.0;
                            prev_uid = start_uid;
                        }
                    }
                });
                handles.push(handle);
            }

            for handle in handles {
                handle.join().unwrap();
            }

            let ledger = Arc::try_unwrap(ledger)
                .unwrap_or_else(|_| panic!("Error extracting final value of the Ledger"));
            let _ = black_box(ledger.into_inner().unwrap());
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
