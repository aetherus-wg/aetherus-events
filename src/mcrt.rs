//! High-level MCRT (Monte Carlo Ray Tracing) event types.
//!
//! This module provides idiomatic Rust enum types for MCRT events with
//! proper encode/decode implementations to/from 32-bit raw integers.
//!
//! ## Hierarchy
//!
//! ```text
//! MCRT
//! ├── Interface: Reflection, Refraction, ReEmittance, Boundary
//! ├── Reflector: Diffuse, Specular, Composite, RetroReflective
//! └── Material
//!     ├── Absorption
//!     ├── Inelastic: Raman, Fluorescence
//!     └── Elastic: HenyeyGreenstein, Mie, Rayleigh, SphericalCdf
//! ```
//!
//! ## Usage
//!
//! Use the `mcrt_event!` macro to construct events:
//!
//! ```rust
//! use aetherus_events::mcrt_event;
//!
//! // Simple event
//! let event = mcrt_event!(Interface, Reflection);
//!
//! // Material event with scattering
//! let event = mcrt_event!(Material, Elastic, Mie, Forward);
//! ```
//!
//! See [Aetherus Event Specification](https://github.com/aetherus-wg/aetherus-events)
//! for full encoding details.

use crate::raw::{self, RawField};
use crate::{Decode, Encode};

// NOTE: To simplify implementation for now, we will restrict to not allow MatSurf for now,
// as some nuisances about grouping have not been resolved.

/// MCRT (Monte Carlo Ray Tracing) event with associated source ID.
#[derive(PartialEq, Debug)]
pub enum MCRT {
    Interface(Interface),
    Reflector(Reflector),
    Material(Material),
}

/// Interface events: interaction at surface boundaries.
///
/// - `Reflection`: Specular reflection at an interface
/// - `Refraction`: Refraction through an interface (Fresnel)
/// - `ReEmittance`: Re-emittance (BRDF/BTDF) from bidirectional transport
/// - `Boundary`: Generic boundary event
#[derive(PartialEq, Debug)]
pub enum Interface {
    Reflection,
    Refraction,
    ReEmittance,
    Boundary,
}

/// Reflector events: surface reflection types.
///
/// - `Diffuse`: Lambertian diffuse reflection
/// - `Specular`: Mirror-like specular
/// - `Composite`: Combined diffuse + specular
/// - `RetroReflective`: Retroreflection
/// - `CompositeRetroReflective`: Combined retroreflection
#[derive(PartialEq, Debug)]
pub enum Reflector {
    Diffuse,
    Specular,
    Composite,
    RetroReflective,
    CompositeRetroReflective,
}

/// Material events: volumetric interactions.
///
/// - `Absorption`: Photon absorbed (terminates path)
/// - `Inelastic`: Inelastic scattering (Raman/Fluorescence)
/// - `Elastic`: Elastic scattering (Rayleigh/Mie/HG)
#[derive(PartialEq, Debug)]
pub enum Material {
    Absorption,
    Inelastic(Inelastic),
    Elastic(Elastic),
}

/// Inelastic scattering events with direction.
#[derive(PartialEq, Debug)]
pub enum Inelastic {
    Raman(ScatterDir),
    Fluorescence(ScatterDir),
}

/// Elastic scattering events with direction and phase function model.
#[derive(PartialEq, Debug)]
pub enum Elastic {
    HenyeyGreenstein(ScatterDir),
    Mie(ScatterDir),
    Rayleigh(ScatterDir),
    SphericalCdf(ScatterDir),
}

/// Scattering direction relative to incident direction.
///
/// Direction is determined by scattering angle θ:
/// - `Forward`: 0 ≤ θ < 30deg
/// - `Side`: 30deg ≤ θ < 150deg
/// - `Backward`: 150deg ≤ θ ≤ 180deg
/// - `Unknown`: No constraint
#[derive(PartialEq, Debug)]
pub enum ScatterDir {
    Unknown,
    Forward,
    Side,
    Backward,
}

impl ScatterDir {
    pub fn new() -> Self {
        ScatterDir::Unknown
    }
    pub fn from(theta: f64) -> Self {
        if theta < std::f64::consts::FRAC_PI_4 {
            ScatterDir::Forward
        } else if theta < 3.0 * std::f64::consts::FRAC_PI_4 {
            ScatterDir::Side
        } else {
            ScatterDir::Backward
        }
    }
    pub fn from_with_spec(theta: f64, intervals: [f64; 4]) -> Self {
        assert_eq!(intervals[0], 0.0);
        assert_eq!(intervals[3], std::f64::consts::PI);

        if theta >= intervals[0] && theta < intervals[1] {
            ScatterDir::Forward
        } else if theta >= intervals[1] && theta < intervals[2] {
            ScatterDir::Side
        } else {
            ScatterDir::Backward
        }
    }
}

impl Encode<u32> for MCRT {
    fn encode(&self) -> u32 {
        match self {
            MCRT::Interface(it) => raw::MCRT::Interface.encode() | it.encode(),
            MCRT::Reflector(rt) => raw::MCRT::Reflector.encode() | rt.encode(),
            MCRT::Material(mt)  => raw::MCRT::Material.encode() | mt.encode(),
        }
    }
}

impl Decode<u32> for MCRT {
    fn decode(raw: u32) -> Self
    where
        Self: Sized,
    {
        let mcrt_type = raw::MCRT::decode(raw);
        match mcrt_type {
            raw::MCRT::Interface => MCRT::Interface(Interface::decode(raw)),
            raw::MCRT::Reflector => MCRT::Reflector(Reflector::decode(raw)),
            raw::MCRT::Material  => MCRT::Material(Material::decode(raw)),
        }
    }
}

impl Encode<u32> for Interface {
    fn encode(&self) -> u32 {
        match self {
            Interface::Reflection  => raw::Interface::Reflection.encode(),
            Interface::Refraction  => raw::Interface::Refraction.encode(),
            Interface::ReEmittance => raw::Interface::ReEmittance.encode(),
            Interface::Boundary    => raw::Interface::Boundary.encode(),
        }
    }
}

impl Decode<u32> for Interface {
    fn decode(raw: u32) -> Self
    where
        Self: Sized,
    {
        let interface_type = raw::Interface::decode(raw);
        match interface_type {
            raw::Interface::Reflection  => Interface::Reflection,
            raw::Interface::Refraction  => Interface::Refraction,
            raw::Interface::ReEmittance => Interface::ReEmittance,
            raw::Interface::Boundary    => Interface::Boundary,
        }
    }
}

impl Encode<u32> for Reflector {
    fn encode(&self) -> u32 {
        match self {
            Reflector::Diffuse                  => raw::Reflector::Diffuse.encode(),
            Reflector::Specular                 => raw::Reflector::Specular.encode(),
            Reflector::Composite                => raw::Reflector::Composite.encode(),
            Reflector::RetroReflective          => raw::Reflector::RetroReflective.encode(),
            Reflector::CompositeRetroReflective => raw::Reflector::CompRetroRef.encode(),
        }
    }
}

impl Decode<u32> for Reflector {
    fn decode(raw: u32) -> Self
    where
        Self: Sized,
    {
        let reflect_type = raw::Reflector::decode(raw);
        match reflect_type {
            raw::Reflector::Diffuse         => Reflector::Diffuse,
            raw::Reflector::Specular        => Reflector::Specular,
            raw::Reflector::Composite       => Reflector::Composite,
            raw::Reflector::RetroReflective => Reflector::RetroReflective,
            raw::Reflector::CompRetroRef    => Reflector::CompositeRetroReflective,
        }
    }
}

impl Encode<u32> for Material {
    fn encode(&self) -> u32 {
        match self {
            Material::Absorption    => raw::Material::Absorption.encode(),
            Material::Inelastic(it) => raw::Material::Inelastic.encode() | it.encode(),
            Material::Elastic(et)   => raw::Material::Elastic.encode() | et.encode(),
        }
    }
}

impl Decode<u32> for Material {
    fn decode(raw: u32) -> Self
    where
        Self: Sized,
    {
        let material_type = raw::Material::decode(raw);
        match material_type {
            raw::Material::Absorption    => Material::Absorption,
            raw::Material::Inelastic     => Material::Inelastic(Inelastic::decode(raw)),
            raw::Material::Elastic       => Material::Elastic(Elastic::decode(raw)),
        }
    }
}

impl Encode<u32> for Inelastic {
    fn encode(&self) -> u32 {
        match self {
            Inelastic::Raman(dir)        => raw::Inelastic::Raman.encode() | dir.encode(),
            Inelastic::Fluorescence(dir) => raw::Inelastic::Fluorescence.encode() | dir.encode(),
        }
    }
}

impl Decode<u32> for Inelastic {
    fn decode(raw: u32) -> Self
    where
        Self: Sized,
    {
        let inelastic_type = raw::Inelastic::decode(raw);
        match inelastic_type {
            raw::Inelastic::Raman        => Inelastic::Raman(ScatterDir::decode(raw)),
            raw::Inelastic::Fluorescence => Inelastic::Fluorescence(ScatterDir::decode(raw)),
        }
    }
}

impl Encode<u32> for Elastic {
    fn encode(&self) -> u32 {
        match self {
            Elastic::HenyeyGreenstein(dir) => raw::Elastic::HenyeyGreenstein.encode() | dir.encode(),
            Elastic::Mie(dir)              => raw::Elastic::Mie.encode()              | dir.encode(),
            Elastic::Rayleigh(dir)         => raw::Elastic::Rayleigh.encode()         | dir.encode(),
            Elastic::SphericalCdf(dir)     => raw::Elastic::SphericalCdf.encode()     | dir.encode(),
        }
    }
}

impl Decode<u32> for Elastic {
    fn decode(raw: u32) -> Self
    where
        Self: Sized,
    {
        let elastic_type = raw::Elastic::decode(raw);
        match elastic_type {
            raw::Elastic::HenyeyGreenstein => Elastic::HenyeyGreenstein(ScatterDir::decode(raw)),
            raw::Elastic::Mie              => Elastic::Mie(ScatterDir::decode(raw)),
            raw::Elastic::Rayleigh         => Elastic::Rayleigh(ScatterDir::decode(raw)),
            raw::Elastic::SphericalCdf     => Elastic::SphericalCdf(ScatterDir::decode(raw)),
        }
    }
}

impl Encode<u32> for ScatterDir {
    fn encode(&self) -> u32 {
        match self {
            ScatterDir::Unknown  => raw::ScatterDir::Unknown.encode(),
            ScatterDir::Forward  => raw::ScatterDir::Forward.encode(),
            ScatterDir::Side     => raw::ScatterDir::Side.encode(),
            ScatterDir::Backward => raw::ScatterDir::Backward.encode(),
        }
    }
}

impl Decode<u32> for ScatterDir {
    fn decode(raw: u32) -> Self
    where
        Self: Sized,
    {
        let dir_type = raw::ScatterDir::decode(raw);
        match dir_type {
            raw::ScatterDir::Unknown  => ScatterDir::Unknown,
            raw::ScatterDir::Forward  => ScatterDir::Forward,
            raw::ScatterDir::Side     => ScatterDir::Side,
            raw::ScatterDir::Backward => ScatterDir::Backward,
        }
    }
}

// Write a macro that given the sequence of super and sub types, build the MCRT Event
// i.e.
// 1. mcrt_event!(Interface, Reflection) -> MCRT::Interface(Interface::Reflection)
// 2. mcrt_event!(Material,Elastic,Mie,Any) -> MCRT::Material(Material::Elastic(Elastic::Mie(ScatterDir::Any)))
#[macro_export]
macro_rules! mcrt_event {
    ($event_type:ident) => {
        $crate::mcrt::MCRT::$event_type
    };
    ($subtype:ident, $sstype:ident) => {
        $crate::mcrt::MCRT::$subtype($crate::mcrt::$subtype::$sstype)
    };
    ($stype:ident, $sstype:ident, $ssstype:ident, $dirtype:ident) => {
        $crate::mcrt::MCRT::$stype($crate::mcrt::$stype::$sstype(
            $crate::mcrt::$sstype::$ssstype($crate::mcrt::ScatterDir::$dirtype),
        ))
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn mcrt_event_macro() {
        let event1 = mcrt_event!(Interface, Reflection);
        assert_eq!(event1, MCRT::Interface(Interface::Reflection));
        let event2 = mcrt_event!(Material, Elastic, Mie, Unknown);
        assert_eq!(
            event2,
            MCRT::Material(Material::Elastic(Elastic::Mie(ScatterDir::Unknown)))
        );
    }

    #[test]
    fn encoding_decoding() {
        let dec_list = vec![
            MCRT::Interface(Interface::Reflection),
            MCRT::Interface(Interface::Refraction),
            MCRT::Interface(Interface::ReEmittance),
            MCRT::Reflector(Reflector::Diffuse),
            MCRT::Reflector(Reflector::Specular),
            MCRT::Reflector(Reflector::Composite),
            MCRT::Reflector(Reflector::RetroReflective),
            MCRT::Material(Material::Absorption),
            MCRT::Material(Material::Inelastic(Inelastic::Raman(ScatterDir::Side))),
            MCRT::Material(Material::Inelastic(Inelastic::Fluorescence(
                ScatterDir::Forward,
            ))),
            MCRT::Material(Material::Elastic(Elastic::HenyeyGreenstein(
                ScatterDir::Backward,
            ))),
            MCRT::Material(Material::Elastic(Elastic::Mie(ScatterDir::Backward))),
            MCRT::Material(Material::Elastic(Elastic::Rayleigh(ScatterDir::Backward))),
            MCRT::Material(Material::Elastic(Elastic::SphericalCdf(
                ScatterDir::Backward,
            ))),
        ];
        let enc_list = vec![
            0x03000001, 0x03010002, 0x03040003, 0x03420004, 0x03440005, 0x03460006, 0x03480007,
            0x03800008, 0x03920009, 0x0395000a, 0x03a3000b, 0x03a7000c, 0x03ab000d, 0x03af000e,
        ];
        for (enc, dec) in enc_list.iter().zip(dec_list.iter()) {
            let decoded_event = MCRT::decode(*enc);
            assert_eq!(*dec, decoded_event);
            assert_eq!(*enc & 0x00ff0000, dec.encode());
        }
    }
}
