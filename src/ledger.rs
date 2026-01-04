use log::warn;
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeAs, SerializeAs};
use serde_with::{DisplayFromStr, serde_as};
use std::collections::HashMap;
use std::str::FromStr;

use crate::SrcId;
use crate::{Encode, EventId, RawEvent, SrcName};
use serde_json;
use std::fs::File;

use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};

// UID combines sequence number and event type [file:1].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Uid {
    pub seq_id: u32,
    pub event: u32, // u32 Event
}

impl Hash for Uid {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.encode().hash(state);
    }
}

impl std::fmt::Debug for Uid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Uid(seq_id: {}, event: 0x{:08X})",
            self.seq_id, self.event
        )
    }
}

impl std::fmt::Display for Uid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:08X}_{:08X}", self.seq_id, self.event)
    }
}

impl FromStr for Uid {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim_start_matches("0x");
        if s.len() != 17 {
            return Err(format!("Invalid Uid string length: {}", s.len()));
        }
        let seq_id = u32::from_str_radix(&s[0..8], 16)
            .map_err(|e| format!("Failed to parse seq_id: {}", e))?;
        assert_eq!(
            &s[8..9],
            "_",
            "Invalid Uid format, expected '_' at position 8"
        );
        let event = u32::from_str_radix(&s[9..17], 16)
            .map_err(|e| format!("Failed to parse event: {}", e))?;
        Ok(Uid { seq_id, event })
    }
}

impl FromStr for SrcId {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if s == "None" {
            return Ok(SrcId::None);
        }
        let parts: Vec<&str> = s.split('(').collect();
        if parts.len() != 2 || !parts[1].ends_with(')') {
            return Err(format!("Invalid SrcId format: {}", s));
        }
        let id_type = parts[0];
        let id_value_str = &parts[1][..parts[1].len() - 1];
        let id_value = id_value_str
            .parse::<u16>()
            .map_err(|e| format!("Failed to parse SrcId value: {}", e))?;
        match id_type {
            "Mat" => Ok(SrcId::Mat(id_value)),
            "Surf" => Ok(SrcId::Surf(id_value)),
            "MatSurf" => Ok(SrcId::MatSurf(id_value)),
            "Light" => Ok(SrcId::Light(id_value)),
            _ => Err(format!("Unknown SrcId type: {}", id_type)),
        }
    }
}

impl Uid {
    pub fn new(seq_id: u32, event: u32) -> Self {
        Self { seq_id, event }
    }

    pub fn encode(&self) -> u64 {
        ((self.seq_id as u64) << 32) | (self.event.raw() as u64)
    }

    pub fn decode(encoded: u64) -> Self {
        let seq_id = (encoded >> 32) as u32;
        let event = (encoded & 0xFFFFFFFF) as u32;
        Self { seq_id, event }
    }
}

pub struct HexInnerMap;

impl SerializeAs<BTreeMap<u32, u32>> for HexInnerMap {
    fn serialize_as<S>(value: &BTreeMap<u32, u32>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        let mut map = serializer.serialize_map(Some(value.len()))?;
        for (k, v) in value {
            let key = format!("{:08X}", k); // hex key
            map.serialize_entry(&key, v)?;
        }
        map.end()
    }
}

impl<'de> DeserializeAs<'de, BTreeMap<u32, u32>> for HexInnerMap {
    fn deserialize_as<D>(deserializer: D) -> Result<BTreeMap<u32, u32>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{Error as DeError, MapAccess, Visitor};
        use std::collections::BTreeMap as StdBTreeMap;
        use std::fmt;

        struct HexInnerVisitor;

        impl<'de> Visitor<'de> for HexInnerVisitor {
            type Value = BTreeMap<u32, u32>;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("map with hex-encoded u32 keys")
            }

            fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut out = StdBTreeMap::new();
                while let Some((k, v)) = access.next_entry::<String, u32>()? {
                    let key = u32::from_str_radix(&k, 16)
                        .map_err(|e| A::Error::custom(format!("invalid hex key {k}: {e}")))?;
                    out.insert(key, v);
                }
                Ok(out)
            }
        }

        deserializer.deserialize_map(HexInnerVisitor)
    }
}

#[serde_as]
#[derive(Serialize, Deserialize)]
pub struct Ledger {
    grps: HashMap<String, SrcId>, // Key: Group name
    #[serde_as(as = "HashMap<DisplayFromStr, _>")]
    src_map: HashMap<SrcId, Vec<SrcName>>, // Value: Material name, object name, light name.
    start_events: Vec<Uid>,

    next_mat_id: u16,
    next_surf_id: u16,
    next_matsurf_id: u16,
    next_light_id: u16,

    // Use a nested map: (seq_id -> (uid -> next_seq_id)) instead of (seq_id, uid) -> next_seq_id in order to
    // retrieve be able to do a depth search based on seq_id
    #[serde_as(as = "BTreeMap<DisplayFromStr, HexInnerMap>")]
    next: BTreeMap<u32, BTreeMap<u32, u32>>,
    prev: BTreeMap<u32, Uid>,
    next_seq_id: u32,
}

pub fn write_ledger_to_json(ledger: &Ledger, file_path: &str) -> Result<(), serde_json::Error> {
    // Write the JSON string to a file
    let file = File::create(file_path).expect("Unable to create file");
    serde_json::to_writer_pretty(file, ledger)
}

impl Ledger {
    pub fn new() -> Self {
        Self {
            grps: HashMap::new(),
            src_map: HashMap::new(),
            start_events: Vec::new(),
            next_mat_id: 0,
            next_surf_id: 0,
            next_matsurf_id: u16::MAX,
            next_light_id: 0,
            next: BTreeMap::new(),
            prev: BTreeMap::new(),
            next_seq_id: 0,
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
                    .insert(light_id.clone(), vec![SrcName::Light(light_name)]);
            }
        };
        light_id
    }

    pub fn with_surf(&mut self, obj_name: String, grp: Option<String>) -> SrcId {
        let src_id = if let Some(grp_name) = grp {
            let src_id = match self.grps.get(&grp_name) {
                Some(src_id) => src_id.clone(),
                None => {
                    // Create new SurfId
                    let surf_id = SrcId::Surf(self.next_surf_id);
                    self.next_surf_id += 1;
                    self.grps.insert(grp_name.clone(), surf_id.clone());
                    surf_id
                }
            };

            let grp_src_id = match src_id {
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
                SrcId::None => {
                    panic!("Group name {} registered an invalid None source", grp_name);
                }
            };

            grp_src_id
        } else {
            let surf_id = SrcId::Surf(self.next_surf_id);
            self.next_surf_id += 1;
            surf_id
        };

        match self.src_map.get_mut(&src_id) {
            Some(value) => value.push(SrcName::Surf(obj_name)),
            None => {
                self.src_map
                    .insert(src_id.clone(), vec![SrcName::Surf(obj_name)]);
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
                self.src_map
                    .insert(mat_id.clone(), vec![SrcName::Mat(mat_name)]);
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
                Some(src_id) => src_id.clone(),
                None => {
                    // Create new MatId
                    let surf_id = SrcId::MatSurf(self.next_matsurf_id);
                    self.next_matsurf_id -= 1;
                    self.grps.insert(grp_name.clone(), surf_id.clone());
                    surf_id
                }
            };

            let grp_src_id = match src_id {
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
                SrcId::None => {
                    panic!("Group name {} registered an invalid None source", grp_name);
                }
            };
            grp_src_id
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
                    .insert(src_id.clone(), vec![SrcName::MatSurf(matsurf_name)]);
            }
        };

        self.check_ids();

        src_id
    }

    pub fn insert_start(&mut self, start_event: EventId) -> Uid {
        let uid = Uid::new(0, start_event.encode());
        self.start_events.push(uid.clone());

        self.insert_entry(uid.clone(), 1);
        if self.next_seq_id == 0 {
            self.next_seq_id = 2;
        }

        uid
    }

    // WARN: next_seq_id increment overflows silently in release mode, however that is unlikely to
    // happen unless the simulation scene is extremely complex
    pub fn insert(&mut self, prev_event: Uid, event: EventId) -> Uid {
        // Push a new entry in next with the new_event UID if it doesn't exist already and
        //    set count to 1
        // Obs: seq_id=0 is reserved for root identification, hence all new events with no
        // previous cause start with seq_id=0
        let next_seq_id = self
            .get_next_seq_id(&prev_event)
            .ok_or("Previous event not found in ledger")
            .unwrap();

        let uid = Uid::new(next_seq_id, event.encode());

        // FIXME: This is the only portion of the Ledger that needs to be accessed concurently.
        // Then we should encapsulate this section to run it atomically, then the Ledger can
        // implement Send + Sync traits safely without Arc<Mutex>
        if self.insert_entry(uid.clone(), self.next_seq_id) {
            self.next_seq_id += 1;
        }

        uid
    }

    fn insert_entry(&mut self, uid: Uid, next_seq_id: u32) -> bool {
        if None == self.get_next_seq_id(&uid) {
            if self.next.get(&uid.seq_id) == None {
                self.next.insert(uid.seq_id, BTreeMap::new());
            }
            self.next
                .get_mut(&uid.seq_id)
                .unwrap()
                .insert(uid.event, next_seq_id);
            self.prev.insert(next_seq_id, uid.clone());
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
        if let Some(next_seq_id) = self.get_next_seq_id(&uid) {
            if let Some(map) = self.next.get(&next_seq_id) {
                for (next_event, _) in map {
                    let next_uid = Uid::new(next_seq_id, *next_event);
                    next_uids.push(next_uid);
                }
            }
        }
        next_uids
    }

    pub fn get_prev(&self, seq_id: u32) -> Option<Uid> {
        self.prev.get(&seq_id).cloned()
    }

    pub fn get_chain(&self, last_uid: Uid) -> Vec<Uid> {
        let mut chain = Vec::new();
        chain.push(last_uid.clone());
        let mut seq_id = last_uid.seq_id;
        while let Some(uid) = self.get_prev(seq_id) {
            chain.push(uid.clone());
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

    fn get_prev_map(&self) -> &BTreeMap<u32, Uid> {
        &self.prev
    }

    fn get_src_map(&self) -> &HashMap<SrcId, Vec<SrcName>> {
        &self.src_map
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
                    .iter()
                    .map(|src| src.to_string())
                    .collect::<Vec<_>>(),
                vec![mat.clone()]
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
                    .iter()
                    .map(|src| src.to_string())
                    .collect::<Vec<String>>(),
                vec![surf.clone()]
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
                    .iter()
                    .map(|src| src.to_string())
                    .collect::<Vec<String>>(),
                vec![expected_name]
            );
        }

        // Inspect the ledger
        println!("Ledger src_map: {:?}", ledger.src_map);
    }

    #[test]
    fn insert_events() {
        let mut ledger = Ledger::new();
        let emission_event = EventId {
            event_type: crate::EventType::Emission(crate::emission::Emission::PointSource),
            src_id: 1,
        };
        let uid1 = ledger.insert_start(emission_event);
        assert_eq!(uid1.seq_id, 0);
        let mcrt_event = EventId {
            event_type: crate::EventType::MCRT(crate::mcrt_event!(
                Material,
                Elastic,
                HenyeyGreenstein,
                Forward
            )),
            src_id: 2,
        };
        let uid2 = ledger.insert(uid1.clone(), mcrt_event);
        assert_eq!(uid2.seq_id, 1);
        let mcrt_event = EventId {
            event_type: crate::EventType::MCRT(crate::mcrt_event!(Material, Elastic, Mie, Forward)),
            src_id: 2,
        };
        let uid3 = ledger.insert(uid2.clone(), mcrt_event);
        assert_eq!(uid3.seq_id, 2);
        // Check the chain
        let chain = ledger.get_chain(uid3.clone());
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
            event_type: crate::EventType::Emission(crate::emission::Emission::PointSource),
            src_id: 1,
        };
        let uid1 = ledger.insert_start(emission_event);

        let mcrt_event = EventId {
            event_type: crate::EventType::MCRT(crate::mcrt_event!(Interface, Refraction)),
            src_id: *surf_src_id,
        };
        let uid2 = ledger.insert(uid1.clone(), mcrt_event);

        assert_eq!(uid2.seq_id, 1);
        let mcrt_event = EventId {
            event_type: crate::EventType::MCRT(crate::mcrt_event!(Material, Elastic, Mie, Forward)),
            src_id: *mat_src_id,
        };
        let uid3 = ledger.insert(uid2.clone(), mcrt_event);

        let chain = ledger.get_chain(uid3.clone());
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
    }
}
