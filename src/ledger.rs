use std::collections::HashMap;
use log::warn;
use serde::Serialize;
use std::ops::Deref;


// UID combines sequence number and event type [file:1].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct Uid<T>
where
    T: crate::Event,
{
    pub seq_no: u32,
    pub event: T, // u32 Event
}


impl<T> Uid<T>
where
    T: crate::Event,
{
    pub fn new(seq_no: u32, event: T) -> Self {
        Self { seq_no, event }
    }

    pub fn encode(&self) -> u64 {
        ((self.seq_no as u64) << 32) | (self.event.raw() as u64)
    }
}

#[derive(Debug, Clone)]
pub enum Src {
    Mat{ mat_name: String, grp: Option<String> },
    Surf{ obj_name: String, grp: Option<String> },
    MatSurf(String),
    Light{ light_name: String, grp: Option<String> },
}


// NOTE: To simplify implementation for now, we will restrict to not allow MatSurf for now,
// as some nuisances about grouping have not been resolved.

#[derive(Eq, PartialEq, Clone, Debug, Serialize, Hash)]
pub enum SrcId {
    Mat(u16),
    Surf(u16),
    MatSurf(u16),
    Light(u16),
}

impl Deref for SrcId {
    type Target = u16;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::None => panic!("Attempted to deref uninitialized SrcId"),
            Self::Mat(id)     => id,
            Self::Surf(id)    => id,
            Self::MatSurf(id) => id,
            Self::Light(id)   => id,
        }
    }
}

#[derive(Serialize)]
pub struct Ledger<T>
where
    T: crate::Event,
{
    grps:            HashMap<String, SrcId>, // Key: Group name
    src_map:         HashMap<SrcId, Vec<String>>, // Value: Material name, object name, light name.

    next_mat_id:     u16,
    next_surf_id:    u16,
    next_matsurf_id: u16,
    next_light_id:   u16,

    next:            HashMap<Uid<T>, u32>,
    count:           HashMap<Uid<T>, f64>,
    next_seq_id:     u32,
    prev:            HashMap<u32, Uid<T>>,
}

impl<T> Ledger<T>
where
    T: crate::Event,
{
    pub fn new() -> Self {
        Self {
            grps:            HashMap::new(),
            src_map:         HashMap::new(),
            next_mat_id:     0,
            next_surf_id:    0,
            next_matsurf_id: u16::MAX(),
            next_light_id:   0,
            next:            HashMap::new(),
            count:           HashMap::new(),
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

        src_id
    }

    // NOTE: Materials are not grouped, only objects are
    // FIXME: Is `with_mat` necessary? Materials are always paird with surfaces, apart from
    // boundary, which can also be considered a special case of a surface
    pub fn with_mat(&mut self, mat_name: String) -> SrcId {
        let mat_id = SrcId::Mat(self.next_surf_id);
        self.next_surf_id += 1;

        match self.src_map.get_mut(&mat_id) {
            Some(value) => value.push(mat_name),
            None => {
                self.src_map.insert(mat_id.clone(), vec![mat_name]);
            }
        };


        mat_id
    }

    pub fn with_matsurf(&mut self, obj_name: String, mat_name: String, grp: Option<String>) -> SrcId {
        let src_id = if let Some(grp_name) = grp {

            let src_id = match self.grps.get(&grp_name) {
                Some(src_id) => src_id.clone(),
                None => {
                    // Create new MatId
                    let surf_id = SrcId::MatSurf(self.next_surf_id);
                    self.next_surf_id += 1;
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
            let surf_id = SrcId::MatSurf(self.next_surf_id);
            self.next_surf_id += 1;
            surf_id
        };

        let matsurf_name = format!("{}:{}", mat_name, obj_name);
        match self.src_map.get_mut(&src_id) {
            Some(value) => value.push(matsurf_name),
            None => {
                self.src_map.insert(src_id.clone(), vec![matsurf_name]);
            }
        };

        src_id
    }

    // WARN: next_seq_id increment overflows silently in release mode, however that is unlikely to
    // happen unless the simulation scene is extremely complex
    pub fn insert(&mut self, prev_event: Option<Uid<T>>, event: T) -> Uid<T> {
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

        let uid = Uid::new(new_event_seq_no, event);

        if let Some(_event_next_seq_no) = self.next.get(&uid) {
            *self.count.get_mut(&uid).unwrap() += 1.0;
        } else {
            self.next.insert(uid.clone(), self.next_seq_id);
            self.count.insert(uid.clone(), 1.0);
            self.prev.insert(self.next_seq_id, uid.clone());
            self.next_seq_id += 1;
        }
        uid
    }

    pub fn insert_weighted(
        &mut self,
        prev_event: Option<Uid<T>>,
        event: T,
        weight: f64,
    ) -> Uid<T> {
        let uid = self.insert(prev_event, event);
        *self.count.get_mut(&uid).unwrap() += weight - 1.0;
        uid
    }
    pub fn get_next(&self, uid: &Uid<T>) -> Option<u32> {
        self.next.get(&uid).cloned()
    }
    pub fn get_prev(&self, seq_no: u32) -> Option<Uid<T>> {
        self.prev.get(&seq_no).cloned()
    }

    pub fn get_chain(&self, last_uid: Uid<T>) -> Vec<Uid<T>> {
        let mut chain = Vec::new();
        let mut seq_no = last_uid.seq_no;
        while let Some(uid) = self.get_prev(seq_no) {
            chain.push(uid.clone());
            seq_no = uid.seq_no;
        }
        chain.reverse();
        chain
    }
}
