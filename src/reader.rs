use std::{error::Error, io::Read};
use std::fs::File;

use serde::{Deserialize, Serialize};

use crate::Ledger;

pub fn read_ledger(path: &std::path::Path) -> std::io::Result<Ledger> {
    let file = File::open(path)?;
    let json_data = {
        let mut buf_reader = std::io::BufReader::new(file);
        let mut contents = String::new();
        buf_reader.read_to_string(&mut contents)?;
        contents
    };
    let ledger: Ledger = serde_json::from_str(&json_data).expect("Unable to parse ledger file");
    Ok(ledger)
}

#[derive(Deserialize, Serialize)]
pub struct CsvRecord {
    pub pos_x: f64,
    pub pos_y: f64,
    pub pos_z: f64,
    pub dir_x: f64,
    pub dir_y: f64,
    pub dir_z: f64,
    pub wavelength: f64,
    pub power: f64,
    pub weight: f64,
    pub tof: f64,
    #[serde(serialize_with = "array_bytes::ser_hexify", deserialize_with = "array_bytes::de_dehexify")]
    pub uid: u64,
}

pub fn read_csv(path: &std::path::Path) -> std::io::Result<Vec<CsvRecord>> {
    let file = File::open(path)?;
    let mut reader = csv::Reader::from_reader(file);
    let mut records = Vec::new();

    for result in reader.deserialize() {
        let record: CsvRecord = result?;
        records.push(record);
    }

    Ok(records)
}
