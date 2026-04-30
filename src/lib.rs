//! Aetherus Event Type Ledger
//!
//! This library provides an event encoding and tracking system for LIDAR photon modeling,
//! based on the Aetherus event specification. It defines pipelines for Emission, MCRT (Monte Carlo
//! Radiative Transfer), Detection, and Processing stages.
//!
//! ## Encoding Scheme
//!
//! Events are encoded as 32-bit integers with the following structure:
//! - Pipeline: 4 bits (bits 24-27)
//! - SuperType: 2 bits (bits 22-23)
//! - SubType: 6 bits (bits 16-21)
//! - Source ID: 16 bits (bits 0-15)
//!
//! ## Modules
//!
//! - `raw`: Low-level bitfield encoding/decoding traits and types
//! - `mcrt`: MCRT-specific event types (Interface, Reflector, Material)
//! - `emission`: Light source emission types
//! - `filter`: Event filtering using bitmask matching
//! - `ledger`: Event chain tracking with unique identifiers (UID)
//! - `reader`: CSV/JSON file reading utilities
//!
//! ## Quick Start
//!
//! ```rust
//! use aetherus_events::{EventId, Decode, Encode, EventType, SrcId, mcrt_event};
//!
//! // Create an event
//! let event = EventId::new_mcrt(
//!     mcrt_event!(Material, Elastic, Mie, Forward),
//!     SrcId::Mat(1)
//! );
//!
//! // Encode to raw u32
//! let encoded = event.encode();
//! println!("Encoded: {:08X}", encoded);
//!
//! // Decode back
//! let decoded = EventId::decode(encoded);
//! println!("Decoded: {:?}", decoded);
//! ```
//!
//! See [README](https://github.com/aetherus-wg/aetherus-events) for full specification.

pub mod emission;
pub mod filter;
pub mod ledger;
pub mod mcrt;
pub mod raw;
pub mod reader;

pub use crate::ledger::Ledger;

use log::warn;
use raw::{Pipeline, RawField};
use serde::{Deserialize, Serialize};
use std::ops::Deref;

use crate::filter::BitsMatch;

// =======================================
// Traits for encoding and decoding events
// =======================================
pub trait Encode<T> {
    fn encode(&self) -> T;
}

pub trait Decode<T> {
    fn decode(raw: T) -> Self
    where
        Self: Sized;
}

pub trait RawEvent:
    std::hash::Hash + Clone + Eq + std::fmt::Debug + serde::Serialize + for<'de> serde::Deserialize<'de>
{
    fn pipeline(&self) -> Pipeline;
    fn decode(&self) -> EventId;
    fn id(&self) -> u16;
    fn raw(&self) -> u32;
}

// =======================================
// Top level Event Type encoding and decoding
// =======================================
#[derive(Debug, PartialEq)]
pub enum EventType {
    None,
    Emission(emission::Emission),
    MCRT(mcrt::MCRT),
    Detection,
    Processing,
}

// EventId represents the EventType and *SrcId concatenated
#[derive(Debug)]
pub struct EventId {
    pub event_type: EventType,
    pub src_id:     SrcId,
}

impl std::fmt::Display for EventId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.event_type {
            EventType::None => write!(f, "None"),
            EventType::Emission(_) => write!(f, "Emission|{:4X}", *self.src_id),
            EventType::MCRT(mcrt) => match mcrt {
                mcrt::MCRT::Interface(interf) => match interf {
                    mcrt::Interface::Reflection => write!(f, "Reflection|{:4X}", *self.src_id),
                    mcrt::Interface::Refraction => write!(f, "Refraction|{:4X}", *self.src_id),
                    _ => write!(f, "Interface|{:4X}", *self.src_id),
                },
                mcrt::MCRT::Reflector(_) => write!(f, "Reflector|{:4X}", *self.src_id),
                mcrt::MCRT::Material(_) => write!(f, "Material|{:4X}", *self.src_id),
            },
            EventType::Detection => write!(f, "Detection|{:4X}", *self.src_id),
            EventType::Processing => write!(f, "Processing"),
        }
    }
}

impl Into<u32> for EventId {
    fn into(self) -> u32 {
        self.encode()
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, Hash)]
pub enum SrcId {
    None,
    SrcId(u16),
    Mat(u16),
    Surf(u16),
    MatSurf(u16),
    Light(u16),
    Detector(u16),
}

impl std::fmt::Display for SrcId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SrcId::None        => write!(f, "None"),
            SrcId::SrcId(id)   => write!(f, "SrcId({})", id),
            SrcId::Mat(id)     => write!(f, "Mat({})", id),
            SrcId::Surf(id)    => write!(f, "Surf({})", id),
            SrcId::MatSurf(id) => write!(f, "MatSurf({})", id),
            SrcId::Light(id)   => write!(f, "Light({})", id),
            SrcId::Detector(id)     => write!(f, "Det({})", id),
        }
    }
}

impl RawField for SrcId {
    fn mask() -> u32 { 0x0000FFFF }
    fn shift() -> usize { 0 }
    fn bitsize() -> usize { 16 }
    // FIXME: The decode and encode implementations don't work because the default trait functions
    // required that Self is TryFrom<u8> and Into<u8>.
    fn decode(raw: u32) -> Self {
        let id = (raw & Self::mask()) as u16;
        match Pipeline::decode(raw) {
            Pipeline::Emission => SrcId::Light(id),
            Pipeline::MCRT     => {
                match raw::MCRT::decode(raw) {
                    raw::MCRT::Interface => SrcId::MatSurf(id),
                    raw::MCRT::Reflector => SrcId::Surf(id),
                    raw::MCRT::Material  => SrcId::Mat(id),
                }
            }
            Pipeline::Detection  => {
                warn!("Detection pipeline does not have SrcId associated.");
                SrcId::None
            }
            Pipeline::Processing => {
                warn!("Processing pipeline does not have SrcId associated.");
                SrcId::None
            }
        }
    }
    fn encode(&self) -> u32 {
        match self {
            SrcId::None         => 0u32,
            SrcId::SrcId(id)    => *id as u32,
            SrcId::Mat(id)      => *id as u32,
            SrcId::Surf(id)     => *id as u32,
            SrcId::MatSurf(id)  => *id as u32,
            SrcId::Light(id)    => *id as u32,
            SrcId::Detector(id) => *id as u32,
        }
    }
}

impl Deref for SrcId {
    type Target = u16;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::None         => &0u16,
            Self::SrcId(id)    => id,
            Self::Mat(id)      => id,
            Self::Surf(id)     => id,
            Self::MatSurf(id)  => id,
            Self::Light(id)    => id,
            Self::Detector(id) => id,
        }
    }
}

impl SrcId {
    pub fn bits_match(&self) -> BitsMatch {
        match self {
            SrcId::None          => BitsMatch { mask: 0, value: 0 },
            SrcId::SrcId(n)
            | SrcId::Mat(n)
            | SrcId::Surf(n)
            | SrcId::MatSurf(n)
            | SrcId::Light(n)
            | SrcId::Detector(n) => BitsMatch {
                mask: 0xFFFF,
                value: *n as u32,
            },
        }
    }
}

impl EventId {
    pub fn new(event_type: EventType, src_id: SrcId) -> Self {
        EventId { event_type, src_id }
    }
    pub fn new_emission(emission_event: emission::Emission, light_id: SrcId) -> Self {
        EventId {
            event_type: EventType::Emission(emission_event),
            src_id: light_id,
        }
    }
    pub fn new_mcrt(mcrt_event: mcrt::MCRT, matsurf_id: SrcId) -> Self {
        EventId {
            event_type: EventType::MCRT(mcrt_event),
            src_id: matsurf_id,
        }
    }
}

impl Decode<u32> for EventId {
    fn decode(raw: u32) -> Self {
        let pipeline = raw::Pipeline::decode(raw);
        let src_id_raw = (raw & 0xFFFF) as u16;
        let (event_type, src_id) = match pipeline {
            // TODO: Resolve correct SrcId type for MCRT rather than using the superset
            raw::Pipeline::MCRT => (
                EventType::MCRT(mcrt::MCRT::decode(raw)),
                SrcId::MatSurf(src_id_raw),
            ),
            raw::Pipeline::Emission => (
                EventType::Emission(emission::Emission::decode(raw)),
                SrcId::Light(src_id_raw),
            ),
            raw::Pipeline::Detection => (EventType::Detection, SrcId::None),
            _ => panic!("Cannot decode {:?} pipeline event", pipeline),
        };
        EventId { event_type, src_id }
    }
}

impl Encode<u32> for EventId {
    fn encode(&self) -> u32 {
        let event_type_code = match &self.event_type {
            EventType::None               => panic!("Cannot encode None event type"),
            EventType::MCRT(mcrt_event)   => raw::Pipeline::MCRT.encode() | mcrt_event.encode(),
            EventType::Emission(emission) => raw::Pipeline::Emission.encode() | emission.encode(),
            EventType::Detection          => raw::Pipeline::Detection.encode(),
            _ => panic!("Cannot encode event type as MCRT event"),
        };
        event_type_code | (*self.src_id as u32)
    }
}

// NOTE: Implementing this seems superfluous to the EventId::decode(u32)
// Only reason this could be useful if there are other desirable way to encode the events,
// but that's doubtful since the encoding scheme is taylored for u32
impl RawEvent for u32 {
    fn pipeline(&self) -> raw::Pipeline {
        let pipe_code = ((self >> 24) & 0b1111) as u8;
        Pipeline::try_from(pipe_code).unwrap()
    }
    fn decode(&self) -> EventId {
        EventId::decode(*self)
    }
    fn id(&self) -> u16 {
        (self & 0xFFFF) as u16
    }
    fn raw(&self) -> u32 {
        *self
    }
}

// --------------------------------------
// Unit tests for encoding and decoding
// --------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decoding_mcrt_event() {
        let raw_event: u32 = 0x03a40001; // Pipeline: MCRT (3), MCRT Type: Material (2), Material Type: Elastic (0), Elastic Type: Mie (1), SrcId: 1
        let event_id = EventId::decode(raw_event);
        println!("Decoded: {:?}", event_id);
        match event_id.event_type {
            EventType::MCRT(mcrt_event) => match mcrt_event {
                mcrt::MCRT::Material(material_event) => match material_event {
                    mcrt::Material::Elastic(elastic_event) => match elastic_event {
                        mcrt::Elastic::Mie(scatter_dir) => {
                            assert_eq!(scatter_dir, mcrt::ScatterDir::Unknown);
                        }
                        _ => panic!("Expected Elastic::Mie"),
                    },
                    _ => panic!("Expected Material::Elastic"),
                },
                _ => panic!("Expected MCRT::Material"),
            },
            _ => panic!("Expected EventType::MCRT"),
        }
        assert_eq!(event_id.src_id, SrcId::MatSurf(1));
    }

    #[test]
    fn encoding_mcrt_event() {
        let mcrt_event = mcrt_event!(Material, Elastic, Mie, Unknown);
        let event_id = EventId::new_mcrt(mcrt_event, SrcId::Mat(1));
        let raw_event = event_id.encode();
        assert_eq!(raw_event, 0x03a40001); // Pipeline: MCRT (3), MCRT Type: Material (2), Material Type: Elastic (0), Elastic Type: Mie (1), SrcId: 1
    }
}
