mod raw;
mod ledger;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Pipeline {
    Emission   = 1,
    Mcrt       = 3,
    Detection  = 5,
    Processing = 7,
    // Other codes are free to be used for custom pipeline stages
}

impl TryFrom<u8> for Pipeline {
    type Error = &'static str;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Emission),
            3 => Ok(Self::Mcrt),
            5 => Ok(Self::Detection),
            7 => Ok(Self::Processing),
            _ => Err("Invalid Pipeline value"),
        }
    }
}

pub trait Event: std::hash::Hash + Clone + Eq + std::fmt::Debug + serde::Serialize + for<'de> serde::Deserialize<'de> {
    fn pipeline(&self) -> Pipeline;
    fn id(&self) -> u16;
    fn raw(&self) -> u32;
}

impl Event for u32 {
    fn pipeline(&self) -> Pipeline {
        let pipe_code = ((self >> 24) & 0b1111) as u8;
        Pipeline::try_from(pipe_code).unwrap()
    }
    fn id(&self) -> u16 {
        (self & 0xFFFF) as u16
    }
    fn raw(&self) -> u32 {
        *self
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MatId(u16);

impl MatId {
    pub fn new(id: u16) -> Self {
        Self(id)
    }
    pub fn id(&self) -> u16 {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SurfId(u16);

impl SurfId {
    pub fn new(id: u16) -> Self {
        Self(id)
    }
    pub fn id(&self) -> u16 {
        self.0
    }
}

trait MCEvent: Event {
    fn mcrt_type(&self) -> McrtType {
        match raw::McrtSuper::from(((self.raw() >> 22) & 0b11) as u8) {
            raw::McrtSuper::Interface => McrtType::Interface,
            raw::McrtSuper::Reflector => McrtType::Reflector,
            raw::McrtSuper::Material  => McrtType::Material,
            _ => panic!("Invalid McrtSuper type"),
        }
    }
    fn material_type(&self) -> Option<MaterialType> {
        match self.mcrt_type {

        }
        todo!()
    }

    fn dir(&self) -> Option<ScatterDir> {
        todo!()
    }
}

trait InterfaceEvent: MCEvent {
    fn interface_type(&self) -> InterfaceType {
        assert_eq!(self.mcrt_type(), McrtType::Interface);
        match raw::Interface::from(((self.raw() >> 16) & 0b111111) as u8) {
            raw::Interface::Reflection => InterfaceType::Reflection,
            raw::Interface::Refraction => InterfaceType::Refraction,
            _ => panic!("Invalid Interface type"),
        }
    }
}

trait ReflectEvent: InterfaceEvent {
    fn reflect_type(&self) -> ReflectType {
        assert_eq!(self.mcrt_type(), McrtType::Reflector);
        match raw::Reflect::from(((self.raw() >> 16) & 0b111111) as u8) {
            raw::Reflect::Reflection => InterfaceType::Reflection,
            _ => panic!("Invalid Interface type"),
        }
    }
}

trait MaterialEvent: MCEvent {
    fn material_type(&self) -> MaterialType;
}

trait ScatterEvent: MaterialEvent {
    fn scatter_dir(&self) -> ScatterDir;
}

pub enum InterfaceType {
    Reflection,
    Refraction,
}

pub enum ReflectType {
    Diffuse,
    Specular,
    Composite,
    RetroReflective,
    CompositeRetroReflective,
}

pub enum ScatterDir {
    Any      = 0,
    Forward  = 1,
    Side     = 2,
    Backward = 3,
}

pub enum InelasticType {
    Raman(ScatterDir),
    Fluorescence(ScatterDir),
}

pub enum ElasticType {
    HenyeyGreenstein(ScatterDir),
    Mie(ScatterDir),
    Rayleigh(ScatterDir),
    SphericalCDF(ScatterDir),
}

pub enum MaterialType {
    Absoprtion,
    Inelastic(InelasticType),
    Elastic(ElasticType),
}

pub enum McrtType {
    Interface(InterfaceType),
    Reflector(ReflectType),
    Material(MaterialType),
}

impl ScatterDir {
    pub fn from(theta: f64) -> Self {
        if theta < std::f64::consts::FRAC_PI_4 {
            ScatterDir::Forward
        } else if theta < 3.0 * std::f64::consts::FRAC_PI_4 {
            ScatterDir::Side
        } else {
            ScatterDir::Backward
        }
    }
    pub fn new_with_spec(theta: f64, intervals: [f64;4]) -> Self {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_test() {
        let boundary = Cube::new(Point3::new(0.0,0.0,0.0), Point3::new(10.0,10.0,10.0));
        let outvol = OutputVolume::new(boundary, [10, 10, 10], OutputParameter::Energy);
        assert_eq!(outvol.voxel_volume(), 1.0);
    }
}

