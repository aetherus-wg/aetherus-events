use serde::{Deserialize, Serialize};
use std::hash::{Hash, Hasher};
use std::str::FromStr;

use crate::events::EventId;
use crate::{Encode, RawEvent}; // traits:

// ----------------------------------------------------
// Definition of Unique IDentifier (Uid) and methods/traits
// ----------------------------------------------------

/// Unique identifier for an event in the ledger.
///
/// Contains:
/// - `seq_id`: Sequence position in the chain (0 = root)
/// - `event`: 32-bit encoded event type
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Uid {
    pub seq_id: u32,
    #[serde(
        serialize_with = "array_bytes::ser_hexify_prefixed",
        deserialize_with = "array_bytes::de_dehexify"
    )]
    pub event:  u32, // u32 Event
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
        write!(f, "{}, 0x{:08X}", self.seq_id, self.event)
    }
}

impl FromStr for Uid {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(", ").collect();
        let s = s.trim_start_matches("0x");
        if parts.len() != 2 {
            return Err(format!("Invalid Uid format: {}", s));
        }
        let seq_id = parts[0]
            .parse::<u32>()
            .map_err(|e| format!("Failed to parse seq_id: {}", e))?;
        let event = u32::from_str_radix(parts[1].trim_start_matches("0x"), 16)
            .map_err(|e| format!("Failed to parse event: {}", e))?;
        Ok(Uid { seq_id, event })
    }
}

impl Uid {
    pub fn new(seq_id: u32, event: impl Into<u32>) -> Self {
        Self { seq_id, event: event.into() }
    }

    pub fn from_event(seq_id: u32, event: &EventId) -> Self {
        Self {
            seq_id,
            event: event.encode(),
        }
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
