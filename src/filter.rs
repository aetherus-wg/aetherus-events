use std::collections::VecDeque;
use std::fmt;
/// Define a filtering scheme that can be composed by concatenation of various fields in the event
/// bitfield description.
///
/// # Examples
///
/// 1. We could filter for all Scattering Events coming from a specific material with MatId as such
/// ` filter_seq!(MCRT|Material|{Inelastic, Elastic}|*|*|MatId)`
///
/// 2. Filter for all interactions with objects that have SurfId(x) or MatId(x) described by
///    MatSurfId(x)
/// `filter_seq!(MCRT|*|*|MatSurfId)`
///
/// 3. Filter for events that have N number of interactions described by
/// `
/// use aetherus_events::filter_seq;
/// filter_seq!([MCRT|Interface|Refraction|SurfId, MCRT|Material|{Inelastic, Elastic}|*|*|MatId, ... ]);
/// `
///
/// 4. Filter for permutations of events
/// `
/// filter_seq!(perm![ MCRT|Interface|*|SurfId,
///                    MCRT|Material|{Elastic, Inelastic}|*|*|MatId,
///                    ... ])
/// `

/// Macro to create a filter specification using pipe-delimited syntax
/// Single event filter:
/// ```ignore
/// filter_seq!(MCRT|Material|{Inelastic, Elastic}|*|*|MatId)
/// ```
///
/// Sequence of events:
/// ```ignore
/// filter_seq!([MCRT|Interface|*|SurfId, MCRT|Material|{Inelastic, Elastic}|*|*|MatId])
/// ```
///
/// Permutation (any order):
/// ```ignore
/// filter_perm![MCRT|Interface|*|SurfId, MCRT|Material|{Inelastic, Elastic}|*|*|MatId]
/// ```
use crate::ledger::{Ledger, Uid};

#[derive(Clone, Copy)]
pub struct BitsMatch {
    pub mask: u32,
    pub value: u32,
}
impl BitsMatch {
    pub fn new(mask: u32, value: u32) -> Self {
        BitsMatch { mask, value }
    }
    pub fn from_event(event: u32) -> Self {
        BitsMatch { mask: 0xFFFFFFFF, value: event }
    }
}
impl fmt::Debug for BitsMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BitsMatch {{ mask: 0x{:08X}, value: 0x{:08X} }}", self.mask, self.value)
    }
}

#[derive(Clone, Debug)]
pub enum BitsProperty {
    Match(BitsMatch),
    NoMatch(BitsMatch),
}

impl Default for BitsProperty {
    fn default() -> Self {
        BitsProperty::Match(BitsMatch { mask: 0, value: 0 })
    }
}

impl BitsProperty {
    pub fn matches(&self, event: u32) -> bool {
        match self {
            BitsProperty::Match(bits_match) => {
                (event & bits_match.mask) == bits_match.value
            },
            BitsProperty::NoMatch(bits_match) => {
                (event & bits_match.mask) != bits_match.value
            },
        }
    }
}

struct SeqQueueEntry {
    pub uid: Uid,
    pub bits_property_seq: VecDeque<BitsProperty>,
}

pub fn find_forward_uid_seq(ledger: &Ledger, bits_property_seq: Vec<BitsProperty>) -> Vec<Uid> {
    let mut seq_queue: VecDeque<SeqQueueEntry> = VecDeque::new();
    let mut found_uids: Vec<Uid> = Vec::new();
    // Initialize the queue with all events that have seq_no=0
    for uid in ledger.get_start_events() {
        seq_queue.push_back(SeqQueueEntry {
            uid: *uid,
            bits_property_seq: bits_property_seq.clone().into(),
        });
    }
    while !seq_queue.is_empty() {
        let uid_seq = seq_queue.pop_front().unwrap();
        if ledger.get_next(&uid_seq.uid).is_empty() {
            // If last UID in sequence of events, output as valid UID
            if uid_seq.bits_property_seq.is_empty() {
                found_uids.push(uid_seq.uid);
            }
        } else {
            let next_uids = ledger.get_next(&uid_seq.uid);
            assert!(next_uids.len() > 0, "No more subsequent events for UID: {}", uid_seq.uid);
            for next_uid in next_uids {
                if uid_seq.bits_property_seq.is_empty() {
                    seq_queue.push_back(SeqQueueEntry {
                        uid: next_uid,
                        bits_property_seq: uid_seq.bits_property_seq.clone()
                    });

                } else {
                    let bits_property = uid_seq.bits_property_seq.front().unwrap();
                    let mut new_bits_property_seq = uid_seq.bits_property_seq.clone();
                    if bits_property.matches(next_uid.event) {
                        // Match found, proceed to next event in sequence
                        new_bits_property_seq.pop_front();
                    }
                    seq_queue.push_back(SeqQueueEntry {
                        uid: next_uid,
                        bits_property_seq: new_bits_property_seq
                    });
                }
            }
        }
    }

    found_uids
}

pub fn find_dangling_uids(ledger: &Ledger, bits_property: BitsProperty) -> Vec<Uid> {
    let mut found_uids: Vec<Uid> = Vec::new();
    for uid in ledger.get_dangling_uids() {
        if bits_property.matches(uid.event) {
            found_uids.push(uid);
        }
    }
    found_uids
}

#[macro_export]
macro_rules! filter_seq {
    // Single event filter
    // 1. Generic EventType: filter_seq!(Pipeline | EventType | SrcId)
    // i.e. `filter_seq!(MCRT | _ | MatSurfId(u16))` or `filter_seq!(Emission | Laser | LightId(u16))
    ($pipeline:ident, $src_id:expr) => {{
        use $crate::raw::{Pipeline, RawField};
        use $crate::{filter_mcrt_seq, filter_emit_seq, filter_detect_seq};
        use $crate::filter::BitsMatch;
        // TODO: Check if ident is MCRT, then SrcId matches Surf, Mat or MatSurf Ids

        match Pipeline::$pipeline {
            Pipeline::Emission => {
                let (mut mask, mut value) = filter_emit_seq!($src_id);
                mask = mask   | Pipeline::mask();
                value = value | Pipeline::Emission.encode();
                BitsMatch::new(mask, value)
            },
            Pipeline::MCRT => {
                panic!("MCRT event filtering requires SuperType and SubType specification")
            },
            Pipeline::Detection => {
                let (mut mask, mut value) = filter_detect_seq!($src_id);
                mask = mask   | Pipeline::mask();
                value = value | Pipeline::Detection.encode();
                BitsMatch::new(mask, value)
            },
            _ => {
                panic!("Unsupported pipeline type {} in filter_seq! macro", stringify!($pipeline));
            }
        }
    }};
    ($pipeline:ident, $type:ident, $src_id:expr) => {{
        use $crate::raw::{Pipeline, RawField};
        use $crate::{filter_mcrt_seq, filter_emit_seq, filter_detect_seq};
        use $crate::filter::BitsMatch;
        // TODO: Check if ident is MCRT, then SrcId matches Surf, Mat or MatSurf Ids

        match Pipeline::$pipeline {
            Pipeline::Emission => {
                let (mut mask, mut value) = filter_emit_seq!($type, $src_id);
                mask = mask   | Pipeline::mask();
                value = value | Pipeline::Emission.encode();
                BitsMatch::new(mask, value)
            },
            Pipeline::MCRT => {
                let (mut mask, mut value) = filter_mcrt_seq!($type, $src_id);
                mask = mask   | Pipeline::mask();
                value = value | Pipeline::MCRT.encode();
                BitsMatch::new(mask, value)
            },
            Pipeline::Detection => {
                let (mut mask, mut value) = filter_detect_seq!($type, $src_id);
                mask = mask   | Pipeline::mask();
                value = value | Pipeline::Detection.encode();
                BitsMatch::new(mask, value)
            },
            _ => {
                panic!("Unsupported pipeline type {} in filter_seq! macro", stringify!($pipeline));
            }
        }
    }};
    // 2. Super/Sub-Type: filter_seq!(Pipeline | SuperType | SubType | SrcId)
    // i.e. `filter_seq!(MCRT | Interface | Reflection | MatSurfId(u16))` or
    //      `filter_seq!(MCRT | Interface | _ | MatSurfId(u16))`
    //      `filter_seq!(MCRT | Material | Absorption | MatId(u16))`
    ($pipeline:ident, $supertype:ident, $subtype:ident, $src_id:expr) => {{
        use $crate::raw::{Pipeline, RawField};
        use $crate::{filter_mcrt_seq, filter_emit_seq, filter_detect_seq};
        use $crate::filter::BitsMatch;
        // TODO: Check if ident is MCRT, then SrcId matches Surf, Mat or MatSurf Ids

        match Pipeline::$pipeline {
            Pipeline::Emission => {
                let (mut mask, mut value) = filter_emit_seq!($supertype, $subtype, $src_id);
                mask  = mask  | Pipeline::mask();
                value = value | Pipeline::Emission.encode();
                BitsMatch::new(mask, value)
            },
            Pipeline::MCRT => {
                let (mut mask, mut value) = filter_mcrt_seq!($supertype, $subtype, $src_id);
                mask  = mask  | Pipeline::mask();
                value = value | Pipeline::MCRT.encode();
                BitsMatch::new(mask, value)
            },
            Pipeline::Detection => {
                let (mut mask, mut value) = filter_detect_seq!($supertype, $subtype, $src_id);
                mask  = mask  | Pipeline::mask();
                value = value | Pipeline::Detection.encode();
                BitsMatch::new(mask, value)
            },
            _ => {
                panic!("Unsupported pipeline type {} in filter_seq! macro", stringify!($pipeline));
            }
        }
    }};

    // 3. Super/Sub-Type: filter_seq!(Pipeline | SuperType | SubType | Scatter | Direction | SrcId)
    // i.e. `filter_seq!(MCRT | Material | Elastic | Mie | {Forward, Backward} | MatId)` or
    //      `filter_seq!(MCRT | Material | Elastic | _ | _ | _)` or
    //      `filter_seq!(MCRT | Material | _ | _ | _ | MatId(u16))` or
    ($pipeline:ident, $supertype:ident, $subtype:ident, $scatter:ident, $dir:ident, $src_id:expr) => {{
        use $crate::raw::{Pipeline, RawField};
        use $crate::filter::BitsMatch;
        use $crate::{filter_mcrt_seq, filter_emit_seq, filter_detect_seq};
        // TODO: Check if ident is MCRT, then SrcId matches Surf, Mat or MatSurf Ids
        eprintln!("Filtering seq: {} | {} | {} | {} | {} | {}", stringify!($pipeline), stringify!($supertype), stringify!($subtype), stringify!($scatter), stringify!($dir), stringify!($src_id));

        match Pipeline::$pipeline {
            Pipeline::Emission => {
                let (mut mask, mut value) = filter_emit_seq!($supertype, $subtype, $scatter, $dir, $src_id);
                mask  = mask  | Pipeline::mask();
                value = value | Pipeline::Emission.encode();
                BitsMatch::new(mask, value)
            },
            Pipeline::MCRT => {
                let (mut mask, mut value) = filter_mcrt_seq!($supertype, $subtype, $scatter, $dir, $src_id);
                mask  = mask  | Pipeline::mask();
                value = value | Pipeline::MCRT.encode();
                BitsMatch::new(mask, value)
            },
            Pipeline::Detection => {
                let (mut mask, mut value) = filter_detect_seq!($supertype, $subtype, $scatter, $dir, $src_id);
                mask  = mask  | Pipeline::mask();
                value = value | Pipeline::Detection.encode();
                BitsMatch::new(mask, value)
            },
            _ => {
                panic!("Unsupported pipeline type {} in filter_seq! macro", stringify!($pipeline));
            }
        }
    }};

    // 4. TODO: Identify any sequence based on each type shift and bit size

    // Sequence of event filters
    ([ $($spec:tt),* $(,)? ]) => {
        vec![
            $($crate::filter_seq!($spec)),*
        ]
    };
}

#[macro_export]
macro_rules! filter_mcrt_seq {
    // 1. Generic EventType: filter_seq!(Pipeline::MCRT | EventType | SrcId)
    ($event_type:ident, $src_id:expr) => {
        if ($src_id != SrcId::None) {
            assert!(
                matches!($src_id, SrcId::Mat(_)| SrcId::Surf(_) | SrcId::MatSurf(_)),
                "MCRT events can only be filtered by MatId, SurfId, or MatSurfId"
            );
        }
        // This format might be supported only for Custom singlet codec
        //panic!("MCRT event filtering requires SuperType and SubType specification");
    };
    ($supertype:ident, $subtype:ident, $src_id:expr) => {{
        use $crate::raw::*;
        if ($src_id != SrcId::None) {
            assert!(
                matches!($src_id, SrcId::Mat(_) | SrcId::Surf(_) | SrcId::MatSurf(_)),
                "MCRT events can only be filtered by MatId, SurfId, or MatSurfId"
            );
        }
        let mut mask = MCRT::mask();
        let mut value = MCRT::$supertype.encode();
        if (stringify!($subtype) != "_") {
            mask  |= $supertype::mask();
            value |= $supertype::$subtype.encode();
        }
        if ($src_id != SrcId::None) {
            mask  |= SrcId::mask();
            // FIXME: Use encode() function, but the default in RawField trait requires Into<u8>
            value |= (*$src_id as u32);
        }
        (mask, value)
    }};
    ($supertype:ident, $subtype:ident, $scatter:ident, $dir:ident, $src_id:expr) => {{
        use $crate::raw::*;
        if ($src_id != SrcId::None) {
            assert!(
                matches!($src_id, SrcId::Mat(_) | SrcId::Surf(_) | SrcId::MatSurf(_)),
                "MCRT events can only be filtered by MatId, SurfId, or MatSurfId"
            );
        }
        let mut mask = MCRT::mask();
        let mut value = MCRT::$supertype.encode();
        if (stringify!($subtype) != "_") {
            mask  |= $supertype::mask();
            value |= $supertype::$subtype.encode();
        }
        if (stringify!($scatter) != "_") {
            mask  |= $subtype::mask();
            value |= $subtype::$scatter.encode();
        }
        if (stringify!($dir) != "_") {
            mask  |= ScatterDir::mask();
            value |= ScatterDir::$dir.encode();
        }
        if ($src_id != SrcId::None) {
            mask  |= SrcId::mask();
            value |= (*$src_id as u32); // Fixup with encode() function
        }
        (mask, value)
    }};
}

#[macro_export]
macro_rules! filter_emit_seq {
    // 1. Generic EventType: filter_seq!(Pipeline::MCRT | EventType | SrcId)
    ($src_id:expr) => {{
        if $src_id != SrcId::None {
            assert!(matches!($src_id,  SrcId::Light(_)), "Emission events can only be filtered by LightId");

            (SrcId::mask(), *$src_id as u32)
        } else {
            (0, 0)
        }
    }};
    ($event_type:tt, $src_id:expr) => {{
        if $src_id != SrcId::None {
            assert!(matches!($src_id,  SrcId::Light(_)), "Emission events can only be filtered by LightId");

            (SrcId::mask(), *$src_id as u32)
        } else {
            (0, 0)
        }
    }};
    ($supertype:ident, $subtype:ident, $src_id:expr) => {{
        use $crate::SrcId;
        if $src_id != SrcId::None {
            assert!(matches!($src_id, SrcId::Light(_)), "Emission events can only be filtered by LightId");

            (SrcId::mask(), *$src_id as u32 )
        } else {
            (0, 0)
        }
    }};
    ($supertype:ident, $subtype:ident, $scatter:ident, $dir:ident, $src_id:expr) => {
        (0, 0)
    };
}

#[macro_export]
macro_rules! filter_detect_seq {
    // 1. Generic EventType: filter_seq!(Pipeline::MCRT | EventType | SrcId)
    ($src_id:expr) => {{
        // TODO: Complete implementation and SrcId::Detector
        assert!(matches!($src_id, SrcId::None), "Detection events do not have associated SrcId");
        (0, 0)
    }};
    ($event_type:tt, $src_id:expr) => {
        // TODO: Complete implementation and SrcId::Detector
        (0, 0)
    };
    ($supertype:ident, $subtype:ident, $src_id:expr) => {{
        (0, 0)
    }};
    ($supertype:ident, $subtype:ident, $scatter:ident, $dir:ident, $src_id:expr) => {
        (0, 0)
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{EventId, SrcId, EventType};

    #[test]
    fn test_find_dangling_uids_empty_ledger() {
        let ledger = Ledger::new(); // Assuming Ledger has a `new` method
        let bits_property = BitsProperty::default(); // Assuming BitsProperty has a `default` method

        let result = find_dangling_uids(&ledger, bits_property);
        assert!(result.is_empty(), "Expected no dangling UIDs in an empty ledger");
    }

    #[test]
    fn test_find_dangling_uids_no_dangling() {
        let mut ledger = Ledger::new();

        // Populate ledger with non-dangling UIDs
        let event = ledger.insert_start(EventId::new(EventType::Detection, SrcId::None));
        let event = ledger.insert(event, EventId::new(EventType::Detection, SrcId::None));
        let event = ledger.insert(event, EventId::new(EventType::Detection, SrcId::None));

        let bits_property = BitsProperty::NoMatch(BitsMatch::new(0xFFFFFFFF, event.event));

        let result = find_dangling_uids(&ledger, bits_property);
        assert_eq!(result.len(), 0, "Expected no dangling UIDs");
    }

    #[test]
    fn test_find_dangling_uids_dangling() {
        let mut ledger = Ledger::new();

        // Populate ledger with non-dangling UIDs
        let event = ledger.insert_start(EventId::new(EventType::Detection, SrcId::None));
        let event = ledger.insert(event, EventId::new(EventType::Detection, SrcId::None));
        let event = ledger.insert(event, EventId::new(EventType::Detection, SrcId::None));

        let bits_property = BitsProperty::Match(BitsMatch::new(0xFFFFFFFF, event.event));

        let result = find_dangling_uids(&ledger, bits_property);
        assert_eq!(result.len(), 1, "Expected exactly one dangling UIDs");

    }
}
