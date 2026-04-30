use anyhow::{Context, Result};
use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use flate2::read::GzDecoder;
use rand::{RngExt, SeedableRng};
use std::{
    collections::HashSet, env, fs, hint::black_box, path::{Path, PathBuf}
};
use tar::Archive;

use aetherus_events::{Ledger, ledger::Uid, reader::read_ledger};

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

    // Read ledger
    let ledger =
        read_ledger(&ledger_path).expect("Failed to read ledger file");

    // Top 20 most frequent UIDs in the ledger (these would be determined by analyzing the ledger beforehand)
    let uids = vec![
        Uid::new(138,   0x05000000),
        Uid::new(464,   0x05000000),
        Uid::new(547,   0x05000000),
        Uid::new(403,   0x05000000),
        Uid::new(1311,  0x05000000),
        Uid::new(504,   0x05000000),
        Uid::new(804,   0x05000000),
        Uid::new(289,   0x05000000),
        Uid::new(967,   0x05000000),
        Uid::new(180,   0x05000000),
        Uid::new(860,   0x05000000),
        Uid::new(1142,  0x05000000),
        Uid::new(1353,  0x05000000),
        Uid::new(1768,  0x05000000),
        Uid::new(935,   0x05000000),
        Uid::new(1169,  0x05000000),
        Uid::new(1027,  0x05000000),
        Uid::new(5102,  0x05000000),
        Uid::new(2176,  0x05000000),
        Uid::new(10629, 0x05000000),
    ];

    // Benchmark 1: Build reconstruct chains for top 20 most frequent UIDs
    c.bench_function("get_chains", |b| {
        b.iter(|| {
            let _chains: Vec<_>= uids.iter().map(|uid| ledger.get_chain(black_box(*uid))).collect();
        })
    });

    let events: Vec<u32> = vec![
        1, 42, 73, 105, 156, 209, 312, 415, 523, 634, 789, 890, 934, 1021, 1156, 1234, 1345, 1456, 1567, 1678,
    ];
    // Use a random algorithm that is deterministic with a given seed
    let mut rng = rand::rngs::StdRng::from_seed([42u8; 32]);

    // Benchmark 2: Build a new ledger with random addition of events to existing events
    c.bench_function("allocate_to_ledger", |b| {
        b.iter(|| {
            let mut uids = HashSet::new();
            let mut ledger = Ledger::new();
            let prev_uid = ledger.insert_start(events[0]);
            uids.insert(prev_uid);

            for _ in 0..50_000 {
                let prev_uid = *uids.iter().nth(rng.random_range(0..uids.len())).unwrap();
                let event = events[rng.random_range(0..events.len())];
                let uid = ledger.insert(prev_uid, event);
                uids.insert(uid);
            }
            let _ = black_box(ledger);
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
