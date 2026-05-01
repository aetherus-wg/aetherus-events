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
//! use aetherus_events::prelude::*;
//! use aetherus_events::{Encode, Decode};
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

pub mod prelude;

pub mod raw;

pub mod uid;
pub mod src;
pub mod event;
pub mod ledger;

pub mod read;
pub mod filter;

pub use crate::ledger::Ledger;
pub use crate::src::SrcId;
pub use crate::event::{EventType, EventId};
pub use crate::uid::Uid;

use crate::raw::Pipeline;

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

