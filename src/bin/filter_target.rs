use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::error::Error;

use serde::{Deserialize, Serialize};

use aetherus_events::{filter_seq, ledger::Ledger};
use aetherus_events::SrcId;
use aetherus_events::filter::{find_forward_uid_seq, BitsProperty};

#[derive(Deserialize, Serialize)]
struct CsvRecord {
    pos_x: f64,
    pos_y: f64,
    pos_z: f64,
    dir_x: f64,
    dir_y: f64,
    dir_z: f64,
    wavelength: f64,
    power: f64,
    weight: f64,
    tof: f64,
    #[serde(serialize_with = "array_bytes::ser_hexify", deserialize_with = "array_bytes::de_dehexify")]
    uid: u64,
}

fn read_csv(file_path: &str) -> Result<Vec<CsvRecord>, Box<dyn Error>> {
    let file = File::open(file_path)?;
    let mut rdr = csv::Reader::from_reader(file);
    let mut records = Vec::new();

    for result in rdr.deserialize() {
        let record: CsvRecord = result?;
        records.push(record);
    }

    Ok(records)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let ledger_path = args[1].parse::<PathBuf>().unwrap();

    let file = File::open(ledger_path).expect("Unable to create file");
    let json_data = {
        let mut buf_reader = std::io::BufReader::new(file);
        let mut contents = String::new();
        buf_reader
            .read_to_string(&mut contents)
            .expect("Unable to read ledger file");
        contents
    };
    let ledger: Ledger = serde_json::from_str(&json_data).expect("Unable to parse ledger file");

    let filter_seq = vec![
        filter_seq!(MCRT, Interface, Refraction, SrcId::Surf(0xFFFF)),
        filter_seq!(MCRT, Material, Elastic, HenyeyGreenstein, Any, SrcId::Mat(0xFFFF)),
        filter_seq!(Detection, SrcId::None),
    ].into_iter().map(|bits_match| BitsProperty::Match(bits_match)).collect();

    println!("Filter seq: {:?}", filter_seq);

    let uids = find_forward_uid_seq(&ledger, filter_seq);
    for uid in uids.clone() {
        println!("Found UID: {}", uid);
    }

    let csv_path = args.get(2).map(|s| s.parse::<PathBuf>().unwrap());
    let phot_records = if let Some(csv_path) = csv_path.clone() {
        read_csv(csv_path.to_str().unwrap()).expect("Unable to read CSV file")
    } else {
        Vec::new()
    };

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
