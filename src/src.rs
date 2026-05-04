use crate::{
    filter::BitsMatch,
    raw::{self, Pipeline, RawField},
};
use log::warn;
use serde::{Deserialize, Serialize};
use std::{ops::Deref, str::FromStr};

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
    fn mask() -> u32 {
        0x0000FFFF
    }
    fn shift() -> usize {
        0
    }
    fn bitsize() -> usize {
        16
    }
    // FIXME: The decode and encode implementations don't work because the default trait functions
    // required that Self is TryFrom<u8> and Into<u8>.
    fn decode(raw: u32) -> Self {
        let id = (raw & Self::mask()) as u16;
        match Pipeline::decode(raw) {
            Pipeline::Emission => SrcId::Light(id),
            Pipeline::MCRT => match raw::MCRT::decode(raw) {
                raw::MCRT::Interface => SrcId::MatSurf(id),
                raw::MCRT::Reflector => SrcId::Surf(id),
                raw::MCRT::Material  => SrcId::Mat(id),
            },
            Pipeline::Detection => {
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
            SrcId::None => BitsMatch { mask: 0, value: 0 },
            SrcId::SrcId(n)
            | SrcId::Mat(n)
            | SrcId::Surf(n)
            | SrcId::MatSurf(n)
            | SrcId::Light(n)
            | SrcId::Detector(n) => BitsMatch {
                mask:  0xFFFF,
                value: *n as u32,
            },
        }
    }
}

/// Used to deserialize SrcId value from JSON, YML, etc.
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
