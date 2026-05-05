//! Event ledger for tracking photon event chains.
//!
//! The `Ledger` maintains a record of photon event sequences,
//! enabling forward and backward traversal through the event tree.
//!
//! ## Structure
//!
//! Each event is identified by a `Uid` (unique identifier) containing:
//! - `seq_id`: Sequence number in the chain
//! - `event`: 32-bit encoded event type
//!
//! ## Usage
//!
//! ```rust
//! use aetherus_events::prelude::*;
//! use aetherus_events::mcrt_event;
//! use aetherus_events::events::Emission;
//!
//! let mut ledger = Ledger::new();
//!
//! // Insert start event (photon emission)
//! let start = ledger.insert_start(EventId::new(
//!     EventType::Emission(Emission::PointSource),
//!     SrcId::Light(1)
//! ));
//!
//! // Add subsequent events
//! let next = ledger.insert(start, EventId::new_mcrt(
//!     mcrt_event!(Material, Elastic, Mie, Forward),
//!     SrcId::Mat(1)
//! ));
//!
//! // Get the chain
//! let chain = ledger.get_chain(next);
//! ```

use log::warn;
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeAs, SerializeAs};
use serde_with::{DisplayFromStr, serde_as};
use std::collections::{HashMap, HashSet};

use crate::Decode;
use crate::EventId;
use crate::src::SrcId;
use crate::uid::Uid;

use serde_json;
use std::fs::File;

use std::hash::Hash;

use thousands::Separable;

/// Named source identifier for humans.
///
/// Associates a name with a source ID for readable output.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Hash, Eq)]
pub enum SrcName {
    Light(String),
    Surf(String),
    MatSurf(String),
    Mat(String),
    Detector(String),
}

// ----------------------------------------------------
// Definition of Ledger struct and methods
// ----------------------------------------------------
// - write ledger to JSON file
// - Ledger methods:
//   - Initialise sources: Materials, Surfaces, Lights, etc
//   - Group sources for batch ID
//   - insert events and build the event chain
//   - query events, using the next/prev maps as a doubled linked list

pub fn write_ledger_to_json<P>(ledger: &Ledger, file_path: P) -> Result<(), serde_json::Error>
where
    P: AsRef<std::path::Path>,
{
    // Write the JSON string to a file
    let file = File::create(file_path).expect("Unable to create file");
    serde_json::to_writer_pretty(file, ledger)
}

/// Event ledger for tracking photon event chains.
///
/// Maintains a record of all photon events with:
/// - Source mappings (src_map): Named sources mapped to source IDs
/// - Event chains (next/prev): Linked lists of event sequences
/// - Start events (start_events): Root events, which have no previous event cause
#[serde_as]
#[derive(Serialize, Deserialize, Default)]
pub struct Ledger {
    grps:         HashMap<String, SrcId>, // Key: Group name
    #[serde_as(as = "HashMap<DisplayFromStr, _>")]
    src_map:      HashMap<SrcId, Vec<SrcName>>, // Value: Material name, object name, light name.
    start_events: Vec<Uid>,

    next_mat_id:     u16,
    next_surf_id:    u16,
    next_matsurf_id: u16,
    next_light_id:   u16,

    // Use a nested map: (seq_id -> (uid -> next_seq_id)) instead of (seq_id, uid) -> next_seq_id in order to
    // retrieve be able to do a depth search based on seq_id
    #[serde_as(as = "HashMap<_, HexInnerMap>")]
    next:        HashMap<u32, HashMap<u32, u32>>,
    #[serde_as(as = "HashMap<_, DisplayFromStr>")]
    prev:        HashMap<u32, Uid>,
    next_seq_id: u32,
}

impl Ledger {
    pub fn new() -> Self {
        Self {
            grps:            HashMap::new(),
            src_map:         HashMap::new(),
            start_events:    Vec::new(),
            next_mat_id:     0,
            next_surf_id:    0,
            next_matsurf_id: u16::MAX,
            next_light_id:   0,
            next:            HashMap::new(),
            prev:            HashMap::new(),
            next_seq_id:     0,
        }
    }

    pub fn with_light(&mut self, light_name: String) -> SrcId {
        let light_id = SrcId::Light(self.next_light_id);
        self.next_light_id += 1;
        match self.src_map.get_mut(&light_id) {
            Some(_value) => {
                panic!("Light ID {} already exists in src_map", *light_id);
                //value.push(SrcName::Light(light_name))
            }
            None => {
                self.src_map
                    .insert(light_id, vec![SrcName::Light(light_name)]);
            }
        };
        light_id
    }

    pub fn with_surf(&mut self, obj_name: String, grp: Option<String>) -> SrcId {
        let src_id = if let Some(grp_name) = grp {
            let src_id = match self.grps.get(&grp_name) {
                Some(src_id) => *src_id,
                None => {
                    // Create new SurfId
                    let surf_id = SrcId::Surf(self.next_surf_id);
                    self.next_surf_id += 1;
                    self.grps.insert(grp_name.clone(), surf_id);
                    surf_id
                }
            };

            match src_id {
                SrcId::Surf(_) => src_id,
                SrcId::MatSurf(_) => src_id,
                SrcId::Mat(_) => {
                    let matsurf_id = self.next_matsurf_id;
                    self.next_matsurf_id -= 1;

                    warn!(
                        "Discarding {:?} and allocate MatSurf({}), moving Map({:?}) to Map(Mat({}))",
                        src_id, matsurf_id, src_id, matsurf_id
                    );
                    if let Some(mat_names) = self.src_map.remove(&SrcId::Mat(*src_id)) {
                        self.src_map.insert(SrcId::Mat(matsurf_id), mat_names);
                    } else {
                        panic!("Material ID {} not found in src_map", *src_id);
                    }

                    SrcId::MatSurf(matsurf_id)
                }
                SrcId::Light(_) => {
                    panic!("Group name {} already used for a light source", grp_name);
                }
                SrcId::Detector(_) => {
                    panic!("Group name {} already used for a detector sink", grp_name);
                }
                SrcId::SrcId(_) => {
                    panic!("SrcId reserved as an abstract super type");
                }
                SrcId::None => {
                    panic!("Group name {} registered an invalid None source", grp_name);
                }
            }
        } else {
            let surf_id = SrcId::Surf(self.next_surf_id);
            self.next_surf_id += 1;
            surf_id
        };

        match self.src_map.get_mut(&src_id) {
            Some(value) => value.push(SrcName::Surf(obj_name)),
            None => {
                self.src_map.insert(src_id, vec![SrcName::Surf(obj_name)]);
            }
        };

        self.check_ids();

        src_id
    }

    // NOTE: Materials are not grouped, only objects are
    // FIXME: Is `with_mat` necessary? Materials are always paird with surfaces, apart from
    // boundary, which can also be considered a special case of a surface
    pub fn with_mat(&mut self, mat_name: String) -> SrcId {
        let mat_id = SrcId::Mat(self.next_mat_id);
        self.next_mat_id += 1;

        match self.src_map.get_mut(&mat_id) {
            Some(value) => value.push(SrcName::Mat(mat_name)),
            None => {
                self.src_map.insert(mat_id, vec![SrcName::Mat(mat_name)]);
            }
        };

        self.check_ids();

        mat_id
    }

    pub fn with_matsurf(
        &mut self,
        obj_name: String,
        mat_name: String,
        grp: Option<String>,
    ) -> SrcId {
        let src_id = if let Some(grp_name) = grp {
            let src_id = match self.grps.get(&grp_name) {
                Some(src_id) => *src_id,
                None => {
                    // Create new MatId
                    let surf_id = SrcId::MatSurf(self.next_matsurf_id);
                    self.next_matsurf_id -= 1;
                    self.grps.insert(grp_name.clone(), surf_id);
                    surf_id
                }
            };

            match src_id {
                SrcId::MatSurf(_) => src_id,
                SrcId::Surf(_) | SrcId::Mat(_) => {
                    let matsurf_id = self.next_matsurf_id;
                    self.next_matsurf_id -= 1;

                    match src_id {
                        SrcId::Surf(_) => {
                            warn!(
                                "Discarding {:?} and allocate MatSurf({}), moving Map({:?}) to Map(Surf({}))",
                                src_id, matsurf_id, src_id, matsurf_id
                            );
                            if let Some(surf_names) = self.src_map.remove(&src_id) {
                                self.src_map.insert(SrcId::Surf(matsurf_id), surf_names);
                            } else {
                                panic!("Surface ID {} not found in src_map", *src_id);
                            }
                        }
                        SrcId::Mat(_) => {
                            warn!(
                                "Discarding {:?} and allocate MatSurf({}), moving Map({:?}) to Map(Mat({}))",
                                src_id, matsurf_id, src_id, matsurf_id
                            );
                            if let Some(surf_names) = self.src_map.remove(&src_id) {
                                self.src_map.insert(SrcId::Mat(matsurf_id), surf_names);
                            } else {
                                panic!("Surface ID {} not found in src_map", *src_id);
                            }
                        }
                        _ => {}
                    };

                    SrcId::MatSurf(matsurf_id)
                }
                SrcId::Light(_) => {
                    panic!("Group name {} already used for a light source", grp_name);
                }
                SrcId::Detector(_) => {
                    panic!("Group name {} already used for a detector sink", grp_name);
                }
                SrcId::SrcId(_) => {
                    panic!("SrcId reserved as an abstract super type");
                }
                SrcId::None => {
                    panic!("Group name {} registered an invalid None source", grp_name);
                }
            }
        } else {
            let surf_id = SrcId::MatSurf(self.next_matsurf_id);
            self.next_matsurf_id -= 1;
            surf_id
        };

        let matsurf_name = format!("{}:{}", obj_name, mat_name);
        match self.src_map.get_mut(&src_id) {
            Some(value) => value.push(SrcName::MatSurf(matsurf_name)),
            None => {
                self.src_map
                    .insert(src_id, vec![SrcName::MatSurf(matsurf_name)]);
            }
        };

        self.check_ids();

        src_id
    }

    pub fn insert_start(&mut self, start_event: impl Into<u32>) -> Uid {
        let uid = Uid::new(0, start_event.into());

        if self.insert_entry(uid, 1) {
            self.start_events.push(uid);
        }

        if self.next_seq_id == 0 {
            self.next_seq_id = 2;
        }

        uid
    }

    // WARN: next_seq_id increment overflows silently in release mode, however that is unlikely to
    // happen unless the simulation scene is extremely complex
    pub fn insert(&mut self, prev_event: Uid, event: impl Into<u32>) -> Uid {
        // Push a new entry in next with the new_event UID if it doesn't exist already and
        //    set count to 1
        // Obs: seq_id=0 is reserved for root identification, hence all new events with no
        // previous cause start with seq_id=0
        let next_seq_id = self
            .get_next_seq_id(&prev_event)
            .ok_or("Previous event not found in ledger")
            .unwrap();

        let uid = Uid::new(next_seq_id, event.into());

        // FIXME: This is the only portion of the Ledger that needs to be accessed concurently.
        // Then we should encapsulate this section to run it atomically, then the Ledger can
        // implement Send + Sync traits safely without Arc<Mutex>
        if self.insert_entry(uid, self.next_seq_id) {
            self.next_seq_id += 1;
        }

        uid
    }

    /// WARN: This is meant to be called only with dangling UIDs
    pub fn prune(&mut self, uid: &Uid) {
        let mut bifurcate = false;
        let mut current_uid = *uid;

        while !bifurcate {
            self.next
                .get_mut(&current_uid.seq_id)
                .map(|map| map.remove(&current_uid.event));

            if let Some(prev_uid) = self.prev.get(&current_uid.seq_id).cloned() {
                bifurcate = match self.next.get(&current_uid.seq_id) {
                    Some(next_map) => !next_map.is_empty(),
                    None => {
                        panic!(
                            "Inconsistent Ledger state: missing next entry for seq_id {}",
                            current_uid.seq_id
                        );
                    }
                };

                if !bifurcate {
                    self.prev.remove(&current_uid.seq_id);
                    self.next.remove(&current_uid.seq_id);
                }

                current_uid = prev_uid;
            } else {
                panic!(
                    "Inconsistent Ledger state: missing prev entry for seq_id {}",
                    current_uid.seq_id
                );
            }
        }
    }

    pub fn get_dangling_uids(&self) -> Vec<Uid> {
        let mut dangling_uids = Vec::new();
        for (seq_id, map) in &self.next {
            if map.is_empty() && let Some(uid) = self.prev.get(seq_id)
            {
                dangling_uids.push(*uid);
            }
        }
        dangling_uids
    }

    fn insert_entry(&mut self, uid: Uid, next_seq_id: u32) -> bool {
        if self.get_next_seq_id(&uid).is_none() {
            self.next.entry(uid.seq_id).or_default();
            self.next
                .get_mut(&uid.seq_id)
                .unwrap()
                .insert(uid.event, next_seq_id);
            self.prev.insert(next_seq_id, uid);
            // Prepare the next seq_id entry
            self.next.insert(next_seq_id, HashMap::new());
            true
        } else {
            false
        }
    }

    pub fn get_start_events(&self) -> &Vec<Uid> {
        &self.start_events
    }

    pub fn get_next_seq_id(&self, uid: &Uid) -> Option<u32> {
        match self.next.get(&uid.seq_id) {
            None => None,
            Some(map) => map.get(&uid.event).cloned(),
        }
    }
    pub fn get_next(&self, uid: &Uid) -> Vec<Uid> {
        let mut next_uids = Vec::new();
        if let Some(next_seq_id) = self.get_next_seq_id(uid)
            && let Some(map) = self.next.get(&next_seq_id)
        {
            for next_event in map.keys() {
                let next_uid = Uid::new(next_seq_id, *next_event);
                next_uids.push(next_uid);
            }
        }
        next_uids
    }

    pub fn get_prev(&self, seq_id: u32) -> Option<Uid> {
        self.prev.get(&seq_id).cloned()
    }

    pub fn get_chain(&self, last_uid: Uid) -> Vec<Uid> {
        let mut chain = Vec::new();
        chain.push(last_uid);
        let mut seq_id = last_uid.seq_id;
        while let Some(uid) = self.get_prev(seq_id) {
            chain.push(uid);
            seq_id = uid.seq_id;
        }
        chain.reverse();
        chain
    }

    fn check_ids(&self) {
        if self.next_mat_id >= self.next_matsurf_id {
            warn!("Material ID and Material-Surface ID ranges are overlapping");
        }
        if self.next_surf_id >= self.next_matsurf_id {
            warn!("Surface ID and Material-Surface ID ranges are overlapping");
        }
    }

    pub fn get_src_dict(&self) -> HashMap<SrcName, SrcId> {
        let mut src_dict = HashMap::new();
        for (src_id, src_names) in &self.src_map {
            for src_name in src_names {
                src_dict.insert(src_name.clone(), *src_id);
            }
        }
        src_dict
    }

    pub fn emit_dot<'a, I>(&self, uids: I) -> String
    where
        I: IntoIterator<Item = &'a Uid>,
    {
        self.emit_dot_with_freq(uids, &HashMap::new())
    }

    pub fn emit_dot_with_freq<'a, I>(&self, uids: I, freq_dict: &HashMap<Uid, usize>) -> String
    where
        I: IntoIterator<Item = &'a Uid>,
    {
        let mut dot = String::from("digraph Ledger {\n  rankdir=LR;\n  node [shape=record];\n");
        let mut nodes = HashSet::new();
        let mut pairs = HashSet::new();

        let with_cnt = !freq_dict.is_empty();

        for uid in uids {
            let chain_uids = self.get_chain(*uid);
            for chain_uid in chain_uids.iter() {
                let event = EventId::decode(chain_uid.event);
                if !nodes.contains(chain_uid) {
                    let event_str = format!("{}", event).replace("|", "\\|");
                    if with_cnt && let Some(cnt) = freq_dict.get(chain_uid) {
                        dot.push_str(&format!(
                            "  n{:016X} [label=\"{{<l>{}|{}|<r>cnt({})}}\"];\n",
                            chain_uid.encode(),
                            chain_uid.seq_id,
                            event_str,
                            cnt.separate_with_commas(),
                        ));
                    } else {
                        dot.push_str(&format!(
                            "  n{:016X} [label=\"{{<l>{}|<r>{}}}\"];\n",
                            chain_uid.encode(),
                            chain_uid.seq_id,
                            event_str,
                        ));
                    }
                    nodes.insert(*chain_uid);
                }
            }
            for (chain_uid, chain_uid_next) in chain_uids.windows(2).map(|w| (w[0], w[1])) {
                if !pairs.contains(&(chain_uid, chain_uid_next)) {
                    dot.push_str(&format!(
                        "  n{:016X}:r -> n{:016X}:l;\n",
                        chain_uid.encode(),
                        chain_uid_next.encode()
                    ));
                    pairs.insert((chain_uid, chain_uid_next));
                }
            }
        }
        dot.push_str("}\n");
        dot
    }
}

// ----------------------------------------------------
// Helper methods and structs
// ----------------------------------------------------
// - Custom serializer/deserializer for BTreeMap<u32, u32> with hex keys

pub struct HexInnerMap;

impl SerializeAs<HashMap<u32, u32>> for HexInnerMap {
    fn serialize_as<S>(value: &HashMap<u32, u32>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        let mut map = serializer.serialize_map(Some(value.len()))?;
        for (k, v) in value {
            let key = format!("0x{:08X}", k); // hex key
            map.serialize_entry(&key, v)?;
        }
        map.end()
    }
}

impl<'de> DeserializeAs<'de, HashMap<u32, u32>> for HexInnerMap {
    fn deserialize_as<D>(deserializer: D) -> Result<HashMap<u32, u32>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{Error as DeError, MapAccess, Visitor};
        use std::collections::HashMap as StdHashMap;
        use std::fmt;

        struct HexInnerVisitor;

        impl<'de> Visitor<'de> for HexInnerVisitor {
            type Value = HashMap<u32, u32>;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("map with hex-encoded u32 keys")
            }

            fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut out = StdHashMap::new();
                while let Some((k, v)) = access.next_entry::<String, u32>()? {
                    let key = u32::from_str_radix(&k[2..10], 16)
                        .map_err(|e| A::Error::custom(format!("invalid hex key {k}: {e}")))?;
                    out.insert(key, v);
                }
                Ok(out)
            }
        }

        deserializer.deserialize_map(HexInnerVisitor)
    }
}

#[cfg(test)]
mod tests {
    use crate::RawEvent;
    use crate::events::Emission;
    use crate::events::EventType;
    use crate::filter::BitsProperty;
    use crate::mcrt_event;
    use crate::pattern;

    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn produce_src_id() {
        let surfs = vec![
            "surf1".to_string(),
            "surf2".to_string(),
            "surf3".to_string(),
        ];
        let mats = vec!["mat1".to_string(), "mat2".to_string()];

        let objects = vec![
            ("obj1".to_string(), "mat1".to_string()),
            ("obj2".to_string(), "mat2".to_string()),
            ("obj3".to_string(), "mat1".to_string()),
        ];

        let mut ledger = Ledger::new();

        for mat in mats {
            let src_id = ledger.with_mat(mat.clone());
            assert!(ledger.src_map.contains_key(&src_id));
            assert_eq!(
                ledger
                    .src_map
                    .get(&src_id)
                    .unwrap()
                    .to_vec(),
                vec![SrcName::Mat(mat.clone())]
            );
        }

        for surf in surfs {
            let src_id = ledger.with_surf(surf.clone(), None);
            assert!(ledger.src_map.contains_key(&src_id));
            assert_eq!(
                ledger
                    .src_map
                    .get(&src_id)
                    .unwrap()
                    .to_vec(),
                vec![SrcName::Surf(surf.clone())]
            );
        }

        for (obj, mat) in objects {
            let src_id = ledger.with_matsurf(obj.clone(), mat.clone(), None);
            assert!(ledger.src_map.contains_key(&src_id));
            let expected_name = format!("{}:{}", obj.clone(), mat.clone());
            assert_eq!(
                ledger
                    .src_map
                    .get(&src_id)
                    .unwrap()
                    .to_vec(),
                vec![SrcName::MatSurf(expected_name)]
            );
        }

        // Inspect the ledger
        println!("Ledger src_map: {:?}", ledger.src_map);
    }

    #[test]
    fn insert_events() {
        let mut ledger = Ledger::new();
        let emission_event = EventId {
            event_type: EventType::Emission(Emission::PointSource),
            src_id:     SrcId::Light(2),
        };
        let uid1 = ledger.insert_start(emission_event);
        assert_eq!(uid1.seq_id, 0);
        let mcrt_event = EventId {
            event_type: EventType::MCRT(mcrt_event!(Material, Elastic, HenyeyGreenstein, Forward)),
            src_id:     SrcId::Mat(2),
        };
        let uid2 = ledger.insert(uid1, mcrt_event);
        assert_eq!(uid2.seq_id, 1);
        let mcrt_event = EventId {
            event_type: EventType::MCRT(mcrt_event!(Material, Elastic, Mie, Forward)),
            src_id:     SrcId::Mat(2),
        };
        let uid3 = ledger.insert(uid2, mcrt_event);
        assert_eq!(uid3.seq_id, 2);
        // Check the chain
        let chain = ledger.get_chain(uid3);
        println!("Chain: {:?}", chain);
        println!(
            "Chain: {:?}",
            chain
                .iter()
                .map(|uid| format!(
                    "Uid(seq_id: {}, event: {:?})",
                    uid.seq_id,
                    uid.event.decode().event_type
                ))
                .collect::<Vec<String>>()
        );
        assert_eq!(chain.len(), 3);
        assert_eq!(chain[0], uid1);
        assert_eq!(chain[1], uid2);
        assert_eq!(chain[2], uid3);
    }

    #[test]
    fn write_ledger_json() {
        let mut ledger = Ledger::new();
        let surf_src_id = ledger.with_surf("surface1".to_string(), Some("group1".to_string()));
        let mat_src_id = ledger.with_mat("material1".to_string());
        // TODO: Complete the entire implementation to test the json writer
        let emission_event = EventId {
            event_type: EventType::Emission(Emission::PointSource),
            src_id:     SrcId::Light(1),
        };
        let uid1 = ledger.insert_start(emission_event);

        let mcrt_event = EventId {
            event_type: EventType::MCRT(mcrt_event!(Interface, Refraction)),
            src_id:     surf_src_id,
        };
        let uid2 = ledger.insert(uid1, mcrt_event);

        assert_eq!(uid2.seq_id, 1);
        let mcrt_event = EventId {
            event_type: EventType::MCRT(mcrt_event!(Material, Elastic, Mie, Forward)),
            src_id:     mat_src_id,
        };
        let uid3 = ledger.insert(uid2, mcrt_event);

        let chain = ledger.get_chain(uid3);
        println!(
            "Chain: {:?}",
            chain
                .iter()
                .map(|uid| format!(
                    "Uid(seq_id: {}, event: {:?})",
                    uid.seq_id,
                    uid.event.decode().event_type
                ))
                .collect::<Vec<String>>()
        );

        // Create a temporary directory
        let temp_dir = tempdir().expect("Failed to create temporary directory");
        let temp_file_path = temp_dir.path().join("test_ledger.json");
        println!("Temporary file path: {:?}", temp_file_path);
        write_ledger_to_json(&ledger, temp_file_path.to_str().unwrap())
            .expect("Failed to save ledger to JSON.");

        // Keep the temporary directory for inspection
        let _persisted_dir = temp_dir.keep();
        println!(
            "Temporary directory persisted at: {}",
            _persisted_dir.display()
        );

        let stored_ledger: Ledger = {
            let contents: String =
                fs::read_to_string(temp_file_path).expect("Unabel to read json file");
            serde_json::from_str(&contents).expect("Unable to parse ledger file")
            // FIXME: Directly reading from file causes conflict with `de_dexify` which expects a
            // borrowed value
            // let file = File::open(temp_file_path).expect("Unable to open file");
            // serde_json::from_reader(file).expect("Unable to parse ledger file")
        };

        assert_eq!(ledger.grps, stored_ledger.grps);
        assert_eq!(ledger.src_map, stored_ledger.src_map);
        assert_eq!(ledger.start_events, stored_ledger.start_events);
        assert_eq!(ledger.next, stored_ledger.next);
        assert_eq!(ledger.prev, stored_ledger.prev);
    }

    #[test]
    fn test_prune_dangling_uids() {
        let mut ledger = Ledger::new();

        // Populate ledger with non-dangling UIDs
        let event = ledger.insert_start(EventId::new(EventType::Detection, SrcId::None));
        let event = ledger.insert(event, EventId::new(EventType::Detection, SrcId::None));
        let _event = ledger.insert(event, EventId::new(EventType::Detection, SrcId::None));

        let result = ledger.get_dangling_uids();
        assert_eq!(result.len(), 1,
            "Expected exactly one dangling UID in a simple chain"
        );
    }

    #[test]
    fn test_prune_until_bifurcation() {
        let mut ledger = Ledger::new();

        // Populate ledger with non-dangling UIDs
        let uid_0 =
            ledger.insert_start(EventId::new_emission(Emission::PencilBeam, SrcId::Light(0)));
        let uid_1 = ledger.insert(
            uid_0,
            EventId::new_mcrt(mcrt_event!(Material, Elastic, Mie, Unknown), SrcId::Mat(0)),
        );

        let uid_21 = ledger.insert(
            uid_1,
            EventId::new_mcrt(mcrt_event!(Material, Elastic, Mie, Unknown), SrcId::Mat(0)),
        );
        let uid_22 = ledger.insert(
            uid_21,
            EventId::new_mcrt(mcrt_event!(Material, Elastic, Mie, Unknown), SrcId::Mat(0)),
        );
        let _uid_231 = ledger.insert(
            uid_22,
            EventId::new_mcrt(mcrt_event!(Interface, Boundary), SrcId::Surf(0)),
        );
        let uid_232 = ledger.insert(
            uid_22,
            EventId::new_mcrt(mcrt_event!(Material, Elastic, Mie, Unknown), SrcId::Mat(0)),
        );
        let _uid_2321 = ledger.insert(
            uid_232,
            EventId::new_mcrt(mcrt_event!(Interface, Boundary), SrcId::Surf(0)),
        );

        let uid_31 = ledger.insert(
            uid_1,
            EventId::new_mcrt(mcrt_event!(Material, Elastic, Mie, Unknown), SrcId::Mat(0)),
        );
        let _uid_32 = ledger.insert(uid_31, EventId::new(EventType::Detection, SrcId::Surf(1)));

        let dangling_uids = ledger.get_dangling_uids();
        assert_eq!(dangling_uids.len(), 3, "Expected dangling UIDs");
        let dangling_lost_uids = dangling_uids
            .into_iter()
            .filter(|uid| {
                BitsProperty::NoMatch(pattern!(Detection, SrcId::Surf(1))).matches(uid.event)
            })
            .collect::<Vec<_>>();

        assert_eq!(
            dangling_lost_uids.len(),
            2,
            "Expected dangling UID to be pruned"
        );

        //println!("{:?}", ledger);

        for uid in dangling_lost_uids {
            println!("Pruning UID: {:?}", uid);
            ledger.prune(&uid);
        }
    }
}
