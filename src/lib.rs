pub mod raw;
pub mod emission;
pub mod mcrt;
pub mod ledger;
//mod filter;

use raw::{Pipeline, RawField};
use serde::{Deserialize, Serialize};
use std::ops::Deref;
use log::warn;

// =======================================
// Traits for encoding and decoding events
// =======================================
trait Encode {
    fn encode(&self) -> u32;
}

trait Decode {
    fn decode(raw: u32) -> Self where Self: Sized;
}

pub trait RawEvent: std::hash::Hash + Clone + Eq + std::fmt::Debug + serde::Serialize + for<'de> serde::Deserialize<'de> {
    fn pipeline(&self) -> Pipeline;
    fn decode(&self) -> EventId;
    fn id(&self) -> u16;
    fn raw(&self) -> u32;
}

// =======================================
// Top level Event Type encoding and decoding
// =======================================
#[derive(Debug)]
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
    pub src_id:     u16,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum SrcName {
    Light(String),
    Surf(String),
    MatSurf(String),
    Mat(String),
    Detector(String),
}

impl ToString for SrcName {
    fn to_string(&self) -> String {
        match self {
            SrcName::Light(name) => name.clone(),
            SrcName::Surf(name) => name.clone(),
            SrcName::MatSurf(name) => name.clone(),
            SrcName::Mat(name) => name.clone(),
            SrcName::Detector(name) => name.clone(),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, Hash)]
pub enum SrcId {
    None,
    Mat(u16),
    Surf(u16),
    MatSurf(u16),
    Light(u16),
}

impl std::fmt::Display for SrcId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SrcId::None        => write!(f, "None"),
            SrcId::Mat(id)     => write!(f, "Mat({})", id),
            SrcId::Surf(id)    => write!(f, "Surf({})", id),
            SrcId::MatSurf(id) => write!(f, "MatSurf({})", id),
            SrcId::Light(id)   => write!(f, "Light({})", id),
        }
    }
}

impl RawField for SrcId {
    fn mask() -> u32 { 0x0000FFFF }
    fn shift() -> usize { 0 }
    fn bitsize() -> usize { 16 }
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
            },
            Pipeline::Detection  => {
                warn!("Detection pipeline does not have SrcId associated.");
                SrcId::None
            },
            Pipeline::Processing => {
                warn!("Processing pipeline does not have SrcId associated.");
                SrcId::None
            },
        }
    }
    fn encode(&self) -> u32 {
        match self {
            SrcId::None        => 0u32,
            SrcId::Mat(id)     => *id as u32,
            SrcId::Surf(id)    => *id as u32,
            SrcId::MatSurf(id) => *id as u32,
            SrcId::Light(id)   => *id as u32,
        }
    }
}

impl Deref for SrcId {
    type Target = u16;
    fn deref(&self) -> &Self::Target {
        match self {
            SrcId::None       => panic!("Cannot deref None SrcId"),
            Self::Mat(id)     => id,
            Self::Surf(id)    => id,
            Self::MatSurf(id) => id,
            Self::Light(id)   => id,
        }
    }
}

impl EventId {
    pub fn new(event_type: EventType, src_id: u16) -> Self {
        EventId {
            event_type,
            src_id,
        }
    }
    pub fn new_emission(emission_event: emission::Emission, light_id: u16) -> Self {
        EventId {
            event_type: EventType::Emission(emission_event),
            src_id: light_id,
        }
    }
    pub fn new_mcrt(mcrt_event: mcrt::MCRT, matsurf_id: u16) -> Self {
        EventId {
            event_type: EventType::MCRT(mcrt_event),
            src_id: matsurf_id,
        }
    }
}

impl Decode for EventId {
    fn decode(raw: u32) -> Self {
        let pipeline = raw::Pipeline::decode(raw);
        let event_type = match pipeline {
            raw::Pipeline::MCRT => EventType::MCRT(mcrt::MCRT::decode(raw)),
            raw::Pipeline::Emission => EventType::Emission(emission::Emission::decode(raw)),
            raw::Pipeline::Detection => EventType::Detection,
            _ => panic!("Cannot decode {:?} pipeline event", pipeline),
        };
        let src_id = (raw & 0xFFFF) as u16;
        EventId {
            event_type,
            src_id,
        }
    }
}

impl Encode for EventId {
    fn encode(&self) -> u32 {
        let event_type_code = match &self.event_type {
            EventType::None => panic!("Cannot encode None event type"),
            EventType::MCRT(mcrt_event) => raw::Pipeline::MCRT.encode() | mcrt_event.encode(),
            EventType::Emission(emission) => raw::Pipeline::Emission.encode() | emission.encode(),
            EventType::Detection => raw::Pipeline::Detection.encode(),
            _ => panic!("Cannot encode event type as MCRT event"),
        };
        event_type_code | (self.src_id as u32)
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decoding_mcrt_event() {
        let raw_event: u32 = 0x03a40001; // Pipeline: MCRT (3), MCRT Type: Material (2), Material Type: Elastic (0), Elastic Type: Mie (1), SrcId: 1
        let event_id = EventId::decode(raw_event);
        println!("Decoded: {:?}", event_id);
        match event_id.event_type {
            EventType::MCRT(mcrt_event) => {
                match mcrt_event {
                    mcrt::MCRT::Material(material_event) => {
                        match material_event {
                            mcrt::Material::Elastic(elastic_event) => {
                                match elastic_event {
                                    mcrt::Elastic::Mie(scatter_dir) => {
                                        assert_eq!(scatter_dir, mcrt::ScatterDir::Any);
                                    },
                                    _ => panic!("Expected Elastic::Mie"),
                                }
                            },
                            _ => panic!("Expected Material::Elastic"),
                        }
                    },
                    _ => panic!("Expected MCRT::Material"),
                }
            },
            _ => panic!("Expected EventType::MCRT"),
        }
        assert_eq!(event_id.src_id, 1);
    }

    #[test]
    fn encoding_mcrt_event() {
        let mcrt_event = mcrt_event!(Material, Elastic, Mie, Any);
        let event_id = EventId::new_mcrt(mcrt_event, 1);
        let raw_event = event_id.encode();
        assert_eq!(raw_event, 0x03a40001); // Pipeline: MCRT (3), MCRT Type: Material (2), Material Type: Elastic (0), Elastic Type: Mie (1), SrcId: 1
    }
}

