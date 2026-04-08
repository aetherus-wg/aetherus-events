use crate::{impl_u8_raw_field, raw::RawField};
use num_enum::{TryFromPrimitive, IntoPrimitive};

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
    fn mask() -> u32 { 0x00ff0000 }
    fn shift() -> usize { 16 }
    fn bitsize() -> usize { 8 }
}
