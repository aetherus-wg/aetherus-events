use std::collections::HashMap;
use log::warn;
use serde::Serialize;

use crate::mcrt::SrcId;
use crate::{EventId, RawEvent, Encode};


// UID combines sequence number and event type [file:1].
#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Uid
{
    pub seq_no: u32,
    pub event:  u32, // u32 Event
}

impl std::fmt::Debug for Uid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Uid(seq_no: {}, event: 0x{:08X})", self.seq_no, self.event)
    }
}

impl Uid
{
    pub fn new(seq_no: u32, event: u32) -> Self {
        Self { seq_no, event }
    }

    pub fn encode(&self) -> u64 {
        ((self.seq_no as u64) << 32) | (self.event.raw() as u64)
    }
}


#[derive(Serialize)]
pub struct Ledger
{
    grps:            HashMap<String, SrcId>, // Key: Group name
    src_map:         HashMap<SrcId, Vec<String>>, // Value: Material name, object name, light name.

    next_mat_id:     u16,
    next_surf_id:    u16,
    next_matsurf_id: u16,
    next_light_id:   u16,

    next:            HashMap<Uid, u32>,
    prev:            HashMap<u32, Uid>,
    next_seq_id:     u32,
}

impl Ledger
{
    pub fn new() -> Self {
        Self {
            grps:            HashMap::new(),
            src_map:         HashMap::new(),
            next_mat_id:     0,
            next_surf_id:    0,
            next_matsurf_id: u16::MAX,
            next_light_id:   0,
            next:            HashMap::new(),
            prev:            HashMap::new(),
            next_seq_id:     0,
        }
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
                SrcId::Surf(_) => { src_id },
                SrcId::MatSurf(_) => { src_id },
                SrcId::Mat(_) => {
                    let matsurf_id = self.next_matsurf_id;
                    self.next_matsurf_id -= 1;

                    warn!("Discarding {:?} and allocate MatSurf({}), moving Map({:?}) to Map(Mat({}))", src_id, matsurf_id, src_id, matsurf_id);
                    if let Some(mat_names) = self.src_map.remove(&SrcId::Mat(*src_id)) {
                        self.src_map.insert(SrcId::Mat(matsurf_id), mat_names);
                    } else {
                        panic!("Material ID {} not found in src_map", *src_id);
                    }

                    SrcId::MatSurf(matsurf_id)
                },
SrcId::Light(_) => {
                    panic!("Group name {} already used for a light source", grp_name);
                },
            };

            grp_src_id
        } else {
            let surf_id = SrcId::Surf(self.next_surf_id);
            self.next_surf_id += 1;
            surf_id
        };

        match self.src_map.get_mut(&src_id) {
            Some(value) => value.push(obj_name),
            None => {
                self.src_map.insert(src_id.clone(), vec![obj_name]);
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
            Some(value) => value.push(mat_name),
            None => {
                self.src_map.insert(mat_id.clone(), vec![mat_name]);
            }
        };

        self.check_ids();

        mat_id
    }

    pub fn with_matsurf(&mut self, obj_name: String, mat_name: String, grp: Option<String>) -> SrcId {
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
                SrcId::MatSurf(_) => { src_id },
                SrcId::Surf(_) | SrcId::Mat(_) => {
                    let matsurf_id = self.next_matsurf_id;
                    self.next_matsurf_id -= 1;

                    match src_id {
                        SrcId::Surf(_) => {
                            warn!("Discarding {:?} and allocate MatSurf({}), moving Map({:?}) to Map(Surf({}))", src_id, matsurf_id, src_id, matsurf_id);
                            if let Some(surf_names) = self.src_map.remove(&src_id) {
                                self.src_map.insert(SrcId::Surf(matsurf_id), surf_names);
                            } else {
                                panic!("Surface ID {} not found in src_map", *src_id);
                            }
                        },
                        SrcId::Mat(_) => {
                            warn!("Discarding {:?} and allocate MatSurf({}), moving Map({:?}) to Map(Mat({}))", src_id, matsurf_id, src_id, matsurf_id);
                            if let Some(surf_names) = self.src_map.remove(&src_id) {
                                self.src_map.insert(SrcId::Mat(matsurf_id), surf_names);
                            } else {
                                panic!("Surface ID {} not found in src_map", *src_id);
                            }
                        },
                        _ => {},
                    };

                    SrcId::MatSurf(matsurf_id)
                },
SrcId::Light(_) => {
                    panic!("Group name {} already used for a light source", grp_name);
                },
            };
            grp_src_id
        } else {
            let surf_id = SrcId::MatSurf(self.next_matsurf_id);
            self.next_matsurf_id -= 1;
            surf_id
        };

        let matsurf_name = format!("{}:{}", obj_name, mat_name);
        match self.src_map.get_mut(&src_id) {
            Some(value) => value.push(matsurf_name),
            None => {
                self.src_map.insert(src_id.clone(), vec![matsurf_name]);
            }
        };

        self.check_ids();

        src_id
    }

    // WARN: next_seq_id increment overflows silently in release mode, however that is unlikely to
    // happen unless the simulation scene is extremely complex
    pub fn insert(&mut self, prev_event: Option<Uid>, event: EventId) -> Uid {
        // 1. If prev_event is Some, i.e. not the first event in the pipeline, like an emission
        //    event, then we are looking for the seq_no to use
        // 2. Push a new entry in next with the new_event UID if it doesn't exist already and
        //    set count to 1
        // 2b. Otherwise, increment count
        // Obs: seq_id=0 is reserved for root identification, hence all new events with no
        // previous cause start with seq_no=0
        let new_event_seq_no =
            if let Some(prev_event) = prev_event {
                let next_seq = self.next.get(&prev_event);
                *next_seq.ok_or("Previous event not found in ledger").unwrap()
            } else {
                self.next_seq_id += 1;
                0
            };

        let uid = Uid::new(new_event_seq_no, event.encode());

        if let Some(_event_next_seq_no) = self.next.get(&uid) {
            //*self.count.get_mut(&uid).unwrap() += 1.0;
        } else {
            self.next.insert(uid.clone(), self.next_seq_id);
            //self.count.insert(uid.clone(), 1.0);
            self.prev.insert(self.next_seq_id, uid.clone());
            self.next_seq_id += 1;
        }
        uid
    }

    pub fn get_next(&self, uid: &Uid) -> Option<u32> {
        self.next.get(&uid).cloned()
    }
    pub fn get_prev(&self, seq_no: u32) -> Option<Uid> {
        self.prev.get(&seq_no).cloned()
    }

    pub fn get_chain(&self, last_uid: Uid) -> Vec<Uid> {
        let mut chain = Vec::new();
        chain.push(last_uid.clone());
        let mut seq_no = last_uid.seq_no;
        while let Some(uid) = self.get_prev(seq_no) {
            chain.push(uid.clone());
            seq_no = uid.seq_no;
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn produce_src_id() {
        let surfs = vec![
            "surf1".to_string(),
            "surf2".to_string(),
            "surf3".to_string(),
        ];
        let mats = vec![
            "mat1".to_string(),
            "mat2".to_string(),
        ];

        let objects = vec![
            ("obj1".to_string(), "mat1".to_string()),
            ("obj2".to_string(), "mat2".to_string()),
            ("obj3".to_string(), "mat1".to_string()),
        ];

        let mut ledger = Ledger::new();

        for mat in mats {
            let src_id = ledger.with_mat(mat.clone());
            assert!(ledger.src_map.contains_key(&src_id));
            assert_eq!(ledger.src_map.get(&src_id).unwrap(), &vec![mat.clone()]);
        }

        for surf in surfs {
            let src_id = ledger.with_surf(surf.clone(), None);
            assert!(ledger.src_map.contains_key(&src_id));
            assert_eq!(ledger.src_map.get(&src_id).unwrap(), &vec![surf.clone()]);
        }

        for (obj, mat) in objects {
            let src_id = ledger.with_matsurf(obj.clone(), mat.clone(), None);
            assert!(ledger.src_map.contains_key(&src_id));
            let expected_name = format!("{}:{}", obj.clone(), mat.clone());
            assert_eq!(ledger.src_map.get(&src_id).unwrap(), &vec![expected_name]);
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
        let uid1 = ledger.insert(None, emission_event);
        assert_eq!(uid1.seq_no, 0);
        let mcrt_event = EventId {
            event_type: crate::EventType::MCRT(crate::mcrt_event!(Material, Elastic, HenyeyGreenstein, Forward)),
            src_id: 2,
        };
        let uid2 = ledger.insert(Some(uid1.clone()), mcrt_event);
        assert_eq!(uid2.seq_no, 1);
        let mcrt_event = EventId {
            event_type: crate::EventType::MCRT(crate::mcrt_event!(Material, Elastic, Mie, Forward)),
            src_id: 2,
        };
        let uid3 = ledger.insert(Some(uid2.clone()), mcrt_event);
        assert_eq!(uid3.seq_no, 2);
        // Check the chain
        let chain = ledger.get_chain(uid3.clone());
        println!("Chain: {:?}", chain);
        println!("Chain: {:?}",
            chain.iter()
            .map(|uid|
                format!("Uid(seq_no: {}, event: {:?})", uid.seq_no, uid.event.decode().event_type))
            .collect::<Vec<String>>()
        );
        assert_eq!(chain.len(), 3);
        assert_eq!(chain[0], uid1);
        assert_eq!(chain[1], uid2);
        assert_eq!(chain[2], uid3);
    }
}
