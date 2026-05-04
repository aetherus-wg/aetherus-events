use crate::{
    Decode, Encode,
    events::{Emission, Interface, MCRT},
    raw::{self, RawField},
    src::SrcId,
};

// =======================================
// Top level Event Type encoding and decoding
// =======================================
#[derive(Debug, PartialEq)]
pub enum EventType {
    None,
    Emission(Emission),
    MCRT(MCRT),
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
                MCRT::Interface(interf) => match interf {
                    Interface::Reflection => write!(f, "Reflection|{:4X}", *self.src_id),
                    Interface::Refraction => write!(f, "Refraction|{:4X}", *self.src_id),
                    _ => write!(f, "Interface|{:4X}", *self.src_id),
                },
                MCRT::Reflector(_) => write!(f, "Reflector|{:4X}", *self.src_id),
                MCRT::Material(_) => write!(f, "Material|{:4X}", *self.src_id),
            },
            EventType::Detection => write!(f, "Detection|{:4X}", *self.src_id),
            EventType::Processing => write!(f, "Processing"),
        }
    }
}

impl From<EventId> for u32 {
    fn from(event: EventId) -> u32 {
        event.encode()
    }
}

impl EventId {
    pub fn new(event_type: EventType, src_id: SrcId) -> Self {
        EventId { event_type, src_id }
    }
    pub fn new_emission(emission_event: Emission, light_id: SrcId) -> Self {
        EventId {
            event_type: EventType::Emission(emission_event),
            src_id:     light_id,
        }
    }
    pub fn new_mcrt(mcrt_event: MCRT, matsurf_id: SrcId) -> Self {
        EventId {
            event_type: EventType::MCRT(mcrt_event),
            src_id:     matsurf_id,
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
                EventType::MCRT(MCRT::decode(raw)),
                SrcId::MatSurf(src_id_raw),
            ),
            raw::Pipeline::Emission => (
                EventType::Emission(Emission::decode(raw)),
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

// --------------------------------------
// Unit tests for encoding and decoding
// --------------------------------------

#[cfg(test)]
mod tests {
    use crate::{
        events::{Elastic, Material, ScatterDir},
        mcrt_event,
    };

    use super::*;

    #[test]
    fn decoding_mcrt_event() {
        let raw_event: u32 = 0x03a40001; // Pipeline: MCRT (3), MCRT Type: Material (2), Material Type: Elastic (0), Elastic Type: Mie (1), SrcId: 1
        let event_id = EventId::decode(raw_event);
        println!("Decoded: {:?}", event_id);
        match event_id.event_type {
            EventType::MCRT(mcrt_event) => match mcrt_event {
                MCRT::Material(material_event) => match material_event {
                    Material::Elastic(elastic_event) => match elastic_event {
                        Elastic::Mie(scatter_dir) => {
                            assert_eq!(scatter_dir, ScatterDir::Unknown);
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
