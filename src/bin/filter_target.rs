use std::path::PathBuf;

use aetherus_events::reader::CsvRecord;
use aetherus_events::reader::read_csv;
use aetherus_events::reader::read_ledger;

use aetherus_events::pattern;
use aetherus_events::SrcId;
use aetherus_events::filter::{find_forward_uid_seq, BitsProperty};


fn main() {
    let args: Vec<String> = std::env::args().collect();
    let ledger_path = args[1].parse::<PathBuf>().unwrap();

    let ledger = read_ledger(&ledger_path).expect("Unable to read ledger file");

    let csv_path = args.get(2).map(|s| s.parse::<PathBuf>().unwrap());
    let phot_records = if let Some(ref csv_path) = csv_path {
        read_csv(&csv_path).expect("Unable to read CSV file")
    } else {
        Vec::new()
    };

    let filter_seq = vec![
        pattern!(MCRT,      Interface, Refraction,                    SrcId::Surf(0xFFFF)),
        pattern!(MCRT,      Material, Elastic, HenyeyGreenstein, Any, SrcId::Mat(0xFFFF)),
        pattern!(Detection,                                           SrcId::None),
    ].into_iter().map(|bits_match| BitsProperty::Match(bits_match)).collect();

    println!("Filter seq: {:?}", filter_seq);

    let uids = find_forward_uid_seq(&ledger, filter_seq);
    for uid in uids.clone() {
        println!("Found UID: {}", uid);
    }

    let hex_uids = uids.iter()
        .map(|uid| uid.encode())
        .collect::<Vec<u64>>();

    let phot_filtered = phot_records.iter()
    .filter(|record| {
        hex_uids.contains(&record.uid)
    }).collect::<Vec<&CsvRecord>>();

    println!("Filtered photon records: len={} from {}", phot_filtered.len(), phot_records.len());

    let csv_dirpath = csv_path.map(|p| p.parent().unwrap().to_path_buf());
    let csv_outpath = if let Some(dirpath) = csv_dirpath {
        dirpath.join("filtered_photons.csv")
    } else {
        PathBuf::from("filtered_photons.csv")
    };

    let mut csv_writer = csv::Writer::from_path(csv_outpath)
        .expect("Unable to create output CSV file");
    for filtered_record in phot_filtered {
        csv_writer.serialize(&filtered_record)
        .expect("Unable to write filtered CSV file");
    }
}
