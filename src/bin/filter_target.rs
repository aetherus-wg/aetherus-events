use std::path::PathBuf;

use aetherus_events::read::CsvRecord;
use aetherus_events::read::read_csv;
use aetherus_events::read::read_ledger;

use aetherus_events::prelude::*;
use aetherus_events::filter::{BitsProperty, find_forward_uid_seq};
use aetherus_events::pattern;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let ledger_path = args[1].parse::<PathBuf>().unwrap();

    let ledger = read_ledger(&ledger_path).expect("Unable to read ledger file");

    let csv_path = args.get(2).map(|s| s.parse::<PathBuf>().unwrap());
    let phot_records = if let Some(csv_path) = &csv_path {
        read_csv(csv_path).expect("Unable to read CSV file")
    } else {
        Vec::new()
    };

    //let filter_seq = vec![
    //    pattern!(MCRT,      Interface, Refraction,                    SrcId::Surf(0xFFFF)),
    //    pattern!(MCRT,      Material, Elastic, HenyeyGreenstein, Unknown, SrcId::Mat(0xFFFF)),
    //    pattern!(Detection,                                           SrcId::None),
    //].into_iter().map(|bits_match| BitsProperty::Match(bits_match)).collect();
    //
    // Seawater scatter
    //let filter_seq = vec![
    //    pattern!(MCRT,      Material, Elastic, HenyeyGreenstein, Unknown, SrcId::Mat(3)),
    //    //pattern!(MCRT,      Material, Elastic, HenyeyGreenstein, Unknown, SrcId::MatSurf(65535)),
    //    //pattern!(MCRT,      Material, Elastic, HenyeyGreenstein, Unknown, SrcId::MatSurf(65533)),
    //    //pattern!(MCRT, Reflector, Specular, SrcId::Surf(0)),
    //    pattern!(Detection,                                           SrcId::None),
    //].into_iter().map(|bits_match| BitsProperty::Match(bits_match)).collect();

    // Targets
    let filter_seq = vec![
        pattern!(Emission, SrcId::Light(0)),
        pattern!(MCRT, Reflector, Specular, SrcId::Surf(0)), // TargetTube
        //pattern!(MCRT, Reflector, Specular, SrcId::Surf(1)), // TargetToy
        pattern!(Detection, SrcId::None),
    ]
    .into_iter()
    .map(BitsProperty::Match)
    .collect();

    // Glass specular
    //let filter_seq = vec![
    //    pattern!(MCRT, Interface, Reflection, SrcId::MatSurf(65534)), // External interface
    //    //pattern!(MCRT, Interface, Reflection, SrcId::MatSurf(65535)), // Interface with water
    //    pattern!(Detection,                                           SrcId::None),
    //].into_iter().map(|bits_match| BitsProperty::Match(bits_match)).collect();

    // Glass only
    //let filter_seq = vec![
    //    //BitsProperty::Match(pattern!(MCRT,      Interface, Refraction,                    SrcId::Surf(0xFFFF))),
    //    BitsProperty::Match(pattern!(MCRT,      Material, Elastic, HenyeyGreenstein, Unknown, SrcId::Surf(0xFFFF))),
    //    BitsProperty::NoMatch(pattern!(MCRT,      Reflector, Specular,                    SrcId::Surf(0))),
    //    BitsProperty::NoMatch(pattern!(MCRT,      Reflector, Specular,                    SrcId::Surf(1))),
    //    BitsProperty::Match(pattern!(Detection,                                           SrcId::None)),
    //];

    println!("Filter seq: {:?}", filter_seq);

    let uids = find_forward_uid_seq(&ledger, filter_seq);
    println!("Found {} matching UID sequences", uids.len());
    for uid in uids.clone() {
        println!("Found UID: {}", uid);
    }

    let hex_uids = uids.iter().map(|uid| uid.encode()).collect::<Vec<u64>>();

    let phot_filtered = phot_records
        .iter()
        .filter(|record| hex_uids.contains(&record.uid))
        .collect::<Vec<&CsvRecord>>();

    println!(
        "Filtered photon records: len={} from {}",
        phot_filtered.len(),
        phot_records.len()
    );

    let csv_dirpath = csv_path.map(|p| p.parent().unwrap().to_path_buf());
    let csv_outpath = if let Some(dirpath) = csv_dirpath {
        dirpath.join("filtered_photons.csv")
    } else {
        PathBuf::from("filtered_photons.csv")
    };

    let mut csv_writer =
        csv::Writer::from_path(csv_outpath).expect("Unable to create output CSV file");
    for filtered_record in phot_filtered {
        csv_writer
            .serialize(filtered_record)
            .expect("Unable to write filtered CSV file");
    }
}
