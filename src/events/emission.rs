//! Light source emission types.
//!
//! Defines the various emission models for light sources in LIDAR simulations.
//!
//! ## Types
//!
//! - `PencilBeam`: Collimated beam (single direction)
//! - `GaussianBeam`: Gaussian beam profile
//! - `PointSource`: Isotropic point source
//! - `PlaneSource`: Planar source (uniform)
//! - `PlaneWave`: Plane wave

use crate::{impl_u8_raw_field, raw::RawField};
use num_enum::{IntoPrimitive, TryFromPrimitive};

/// Light source emission type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum Emission {
    PencilBeam,
    GaussianBeam,
    PointSource,
    PlaneSource,
    PlaneWave,
}

impl_u8_raw_field!(Emission);
impl RawField for Emission {
    fn mask() -> u32 {
        0x00ff0000
    }
    fn shift() -> usize {
        16
    }
    fn bitsize() -> usize {
        8
    }
}
