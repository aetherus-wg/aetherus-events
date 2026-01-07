use num_enum::{TryFromPrimitive, IntoPrimitive};
use std::convert::TryFrom;
use std::usize;

pub trait RawField: Clone {
    fn mask() -> u32;
    fn shift() -> usize;
    fn bitsize() -> usize;
    fn decode(raw: u32) -> Self
    where
        Self: TryFrom<u8>,
        <Self as TryFrom<u8>>::Error: std::fmt::Debug,
    {
        let value = ((raw & Self::mask()) >> Self::shift()) as u8;
        Self::try_from(value).unwrap_or_else( |err| {
            panic!("Failed to convert value: {:?}, error: {:?}", value, err);
        })
    }
    fn encode(&self) -> u32
    where
        Self: Into<u8>,
    {
        let value = (self.clone().into() as u32) << Self::shift();
        debug_assert!(value & Self::mask() == value, "Encoded value exceeds field mask");
        value
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum Pipeline {
    Emission   = 1,
    MCRT       = 3,
    Detection  = 5,
    Processing = 7,
    // Other codes are free to be used for custom pipeline stages
}

impl RawField for Pipeline {
    fn mask() -> u32 { 0x0F000000 }
    fn shift() -> usize { 24 }
    fn bitsize() -> usize { 4 }
}

// SuperType represents the 2-bit super type category
#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum MCRT {
    Interface = 0,
    Reflector = 1,
    Material  = 2,
    //Custom    = 3,
}

impl RawField for MCRT {
    fn mask() -> u32 { 0x00C00000 }
    fn shift() -> usize { 22 }
    fn bitsize() -> usize { 2 }
}

// SubType for Interface events (6 bits, but simplified enum)
#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum Interface {
    Reflection = 0,
    Refraction = 1,
    ReEmittance = 4,
    // Custom 32-63
}

impl RawField for Interface {
    fn mask() -> u32 { 0x003F0000 }
    fn shift() -> usize { 16 }
    fn bitsize() -> usize { 6 }
}

// SubType for Reflector events
#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum Reflector {
    #[num_enum(alternatives = [3])]
    Diffuse         = 0b000010,  // 00001x
    #[num_enum(alternatives = [5])]
    Specular        = 0b000100,  // 00010x
    #[num_enum(alternatives = [7])]
    Composite       = 0b000110,  // 00011x
    RetroReflective = 0b001000,
    CompRetroRef    = 0b001001,
    // Custom others
}

impl RawField for Reflector {
    fn mask() -> u32 { 0x003F0000 }
    fn shift() -> usize { 16 }
    fn bitsize() -> usize { 6 }
}

// MaterialInteraction encodes the interaction type (2 bits)
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
pub enum Material {
    Absorption = 0b00,
    Inelastic  = 0b01,
    Elastic    = 0b10,
}

impl RawField for Material {
    fn mask() -> u32 { 0x00300000 }
    fn shift() -> usize { 20 }
    fn bitsize() -> usize { 2 }
}

// ScatterType for scattering events (2 bits)
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
pub enum Inelastic {
    Raman        = 0b00,
    Fluorescence = 0b01,
}

impl RawField for Inelastic {
    fn mask() -> u32 { 0x000C0000 }
    fn shift() -> usize { 18 }
    fn bitsize() -> usize { 2 }
}

// ScatterType for scattering events (2 bits)
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
pub enum Elastic {
    HenyeyGreenstein = 0b00,
    Mie              = 0b01,
    Rayleigh         = 0b10,
    SphericalCdf     = 0b11,
}

impl RawField for Elastic {
    fn mask() -> u32 { 0x000C0000 }
    fn shift() -> usize { 18 }
    fn bitsize() -> usize { 2 }
}

// Direction for scattering (2 bits)
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
pub enum ScatterDir {
    Any      = 0b00,
    Forward  = 0b01,
    Side     = 0b10,
    Backward = 0b11,
}

impl RawField for ScatterDir {
    fn mask() -> u32 { 0x00030000 }
    fn shift() -> usize { 16 }
    fn bitsize() -> usize { 2 }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mie_encoding() {
        let scatter_dir = Elastic::Mie;
        let encoded = scatter_dir.encode();
        assert_eq!(encoded & Elastic::mask(), encoded);
        let decoded = Elastic::decode(encoded);
        assert_eq!(decoded, scatter_dir);
    }

    #[test]
    fn mie_explicit() {
        let raw_event: u32 = 0x03a40001; // Pipeline: MCRT (3), MCRT Type: Material (2), Material Type: Elastic (0), Elastic Type: Mie (1), SrcId: 1
        let decoded_elastic = Elastic::decode(raw_event);
        assert_eq!(decoded_elastic, Elastic::Mie);
    }

    #[test]
    fn elastic_encoding() {
        let dec_list = vec![Elastic::HenyeyGreenstein, Elastic::Mie, Elastic::Rayleigh, Elastic::SphericalCdf];
        let enc_list = vec![0x00000000, 0x00040000, 0x00080000, 0x000C0000];
        for (enc, dec) in enc_list.iter().zip(dec_list) {
            assert_eq!(*enc, dec.encode());
            assert_eq!(Elastic::decode(*enc), dec);
        }
    }

    #[test]
    fn inelastic_encoding() {
        let dec_list = vec![Inelastic::Raman, Inelastic::Fluorescence];
        let enc_list = vec![0x00000000, 0x00040000];
        for (enc, dec) in enc_list.iter().zip(dec_list) {
            assert_eq!(*enc, dec.encode());
            assert_eq!(Inelastic::decode(*enc), dec);
        }
    }

    #[test]
    fn scatter_dir_encoding() {
        let dec_list = vec![ScatterDir::Any, ScatterDir::Forward, ScatterDir::Side, ScatterDir::Backward];
        let enc_list = vec![0x00000000, 0x00010000, 0x00020000, 0x00030000];
        for (enc, dec) in enc_list.iter().zip(dec_list) {
            assert_eq!(*enc, dec.encode());
            assert_eq!(ScatterDir::decode(*enc), dec);
        }
    }

    #[test]
    fn material_encoding() {
        let dec_list = vec![Material::Absorption, Material::Inelastic, Material::Elastic];
        let enc_list = vec![0x00000000, 0x00100000, 0x00200000];
        for (enc, dec) in enc_list.iter().zip(dec_list) {
            assert_eq!(*enc, dec.encode());
            assert_eq!(Material::decode(*enc), dec);
        }
    }

    #[test]
    fn mcrt_encoding() {
        let dec_list = vec![MCRT::Interface, MCRT::Reflector, MCRT::Material];
        let enc_list = vec![0x00000000, 0x00400000, 0x00800000];
        for (enc, dec) in enc_list.iter().zip(dec_list) {
            assert_eq!(*enc, dec.encode());
            assert_eq!(MCRT::decode(*enc), dec);
        }
    }

    #[test]
    fn interface_encoding() {
        let dec_list = vec![Interface::Reflection, Interface::Refraction, Interface::ReEmittance];
        let enc_list = vec![0x00000000, 0x00010000, 0x00040000];
        for (enc, dec) in enc_list.iter().zip(dec_list) {
            assert_eq!(*enc, dec.encode());
            assert_eq!(Interface::decode(*enc), dec);
        }
    }

    #[test]
    fn reflector_encoding() {
        let dec_list = vec![Reflector::Diffuse, Reflector::Specular, Reflector::Composite, Reflector::RetroReflective, Reflector::CompRetroRef];
        let enc_list = vec![0x00020000, 0x00040000, 0x00060000, 0x00080000, 0x00090000];
        for (enc, dec) in enc_list.iter().zip(dec_list) {
            assert_eq!(*enc, dec.encode());
            assert_eq!(Reflector::decode(*enc), dec);
        }
    }

    #[test]
    fn pipeline_encoding() {
        let dec_list = vec![Pipeline::Emission, Pipeline::MCRT, Pipeline::Detection, Pipeline::Processing];
        let enc_list = vec![0x01000000, 0x03000000, 0x05000000, 0x07000000];
        for (enc, dec) in enc_list.iter().zip(dec_list) {
            assert_eq!(*enc, dec.encode());
            assert_eq!(Pipeline::decode(*enc), dec);
        }
    }
}
