
# Model EventType Ledger Design

The ledger will be composed of two hash maps (key-value stores) that allows us to traverser the unique IDs (UID) of each event in each generation sequence.

[![build](https://img.shields.io/github/actions/workflow/status/aetherus-wg/aetherus-events/build-test.yml?branch=main&style=flat-square)](https://github.com/aetherus-wg/aetherus-events/actions)
[![License](https://img.shields.io/badge/license-MIT%20%2B%20Apache%202.0-yellow?style=flat-square)](#license)
[![User Guide](https://img.shields.io/badge/User%20Guide-mdBook-blue)](https://aetherus-wg.github.io/eldritch-trace/)
![GitHub Tag](https://img.shields.io/github/v/tag/aetherus-wg/aetherus-events)
[![Bencher performance](https://img.shields.io/badge/Bencher-Plot-brightgreen?logo=data:image/png;base64,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)](https://bencher.dev/perf/aetherus-wg-aetherus-events?lower_value=false&upper_value=false&lower_boundary=false&upper_boundary=false&x_axis=version&branches=369de625-72f1-4b6a-aad8-4b04e9c1c911&testbeds=e4eeb74a-80d3-490d-b6f6-9017a74aa0dc&benchmarks=bc35acb4-b146-4dab-9a63-f71f90963a12%2C2c5c0a66-8e5c-4f7a-8e65-5fe8c3548d96%2C04948941-66f1-4080-8c1f-7c1d326ff0b1%2C4d1133b6-24a4-4397-8e3c-eff95aff822e&measures=c5adb7d2-0fa8-46b4-971a-301047835626&start_time=1770823647499&end_time=1778081247499&tab=plots&plots_search=4fe95fdb-97b4-4337-abf1-151e47a317f5&key=true&reports_per_page=4&branches_per_page=8&testbeds_per_page=8&benchmarks_per_page=8&plots_per_page=8&reports_page=1&branches_page=1&testbeds_page=1&benchmarks_page=1&plots_page=1&utm_medium=share&utm_source=bencher&utm_content=img&utm_campaign=perf%2Bimg&utm_term=aetherus-wg-aetherus-events)


The photons collected are received in packets composed of:
```Rust
struct Photon {
    tof:f64,
    power: f64,
    wavelength:f64,:)
    pos: Pos3,
    dir: Dir3,
    uid: UID
}
struct UID {
    seq_no: u32,
    type: EventType
}
```

The following specification focuses mostly on how to encode the `EventType`, but also gives a usage example below.

![Simulation Event Types Encoding Space Segmentation](./docs/imgs/McrtEventsCodingScheme.png)

## TODO

- [ ] Filtering framework to develop, such that various sequences can be identified and various end UIDs are returned
- [ ] Ledger insert needs to be thought with multithreading in mind, spawning a worker which accepts requests and returns *Futures*, such that threads are not being held up unnecessary

![Ledger Inserter UidFuture](./docs/imgs/AetherusUidLedger_insert_Future.excalidraw.png)

## Encoding Scheme

![](./docs/imgs/McrtEventsEncodingSpec.png)

![](./docs/imgs/SurfacesEventsEncodingSpec.png)

![](./docs/imgs/MaterialEventsEncodingSpec.png)

![](./docs/imgs/ScatterDirEncodingSpec.png)

## Ledger Show-case

| UID { seq_no, type} | next(seq_no) | Description/Ptr to struct definition |
| --- | --- | --- |
| {1, Laser} | 2   | Laser emission |
| {1, Background } | 3   | Background emission |
| {2, FS{Mat{Air}}} | 4   | Forward scatter with air |
| {2, BS{Mat{Air}}} | 5   | Background scatter with air |
| {2, Reflection{Mat{TransPLA}}} | 6   | Reflection from Fresnel refraction with target |
| {2, Refraction{Mat{TransPLA}}} | 7   | Refraction from Fresnel refraction with target |
| {7, FS{Mat{TransPLA}}} | 8   | Forward scatter target |
| {8, FS{Mat{TransPLA}}} | 9   | Forward scatter target |
| {9, BS{Mat{TransPLA}}} | 10  | Forward scatter target |
| {10, Refraction{Mat{Air}}} | 11  | Refraction from Fresnel refraction back to air |
| {11, Detection{PhotonCollector}} | 12  | Detected at apperture of SPAD sensor from SSS |
| {6, Detection{PhotonCollector}} | 13  | SPAD Detection of balistic |
| seq_no | UID |
| \--- | \--- |
| 1   | {0, Root} |
| 2   | {1,Laser} |
| 3   | {2, Background} |
| 4   | {2, FS{Mat{Air}}} |
| 6   | {2, Reflection{Mat{TransPLA}}} |
| 7   | {2, Refraction{Mat{TransPLA}}} |
| 8   | {7, FS{Mat{TransPLA}}} |
| 9   | {8, FS{Mat{TransPLA}}} |
| 10  | {9, BS{Mat{TransPLA}}} |
| 11  | {10, Refraction{Mat{Air}}} |
| 12  | {11, Detection{PhotonCollector}} |

### Filter example

`photon.filter_deny(type:Background).filter_deny(type:SSS{TranslucentPLA}).filter_allow(type:Reflection/Refraction{Mat{TranslucentPLA})`

`photon.filter_deny(type:Background).filter_allow(type:SSS{TranslucentPLA})`

## UID & EventType values encoding

```Julia
abstract type AbstractEvent end
abstract type SubTypeAbstract <: UInt6;
abstract type MatSurfId <: UInt16;
struct MatId = MatSurfId;
struct SurfId = MatSurfId;

type SurfId <: MatSurfId

@enum SuperType <: UInt2
    Interface = 0,
    Reflector = 1,
    Material = 2,
    Custom = 3,
end

@enum Pipeline <: UInt4
    # Custom = 0 
    Emisssion = 1,
    # Custom = 2 
    MCRT = 3,
    # Custom = 4 
    Detection = 5,
    # Custom = 6
    Processing = 7,
    # Custom = 8-15
end

@enum InterfaceEvent <: SubTypeAbstract
    Reflect = 0,
    Refract = 1,
end

@enum ReflectEvent <: SubTypeAbstract
    Diffuse = 0
    Specular = 1,
    RetroReflector = 2,
    BRDF = 3,
end

@enum MaterialEvent <: SubTypeAbstract
    interaction_type::UInt2,
    scatter_type::UInt2,
    direction::UInt2,
end

struct EventType {
    super_type::SuperType,
    sub_type::SubTypeAbstract,
}

struct MCEvent <: AbstractEvent {
    msb_inter_id::UInt4, # u4
    pipeline::Pipeline,  # u4
    event_type::EventType, # u8
    inter_id::MatSurfID, # u16
} # u32

struct UID {
    seq_no::UInt32,   # u32
    event_type::Event # u32
} # u64 
```

### Pipeline encoding: 4-bits

The `pipeline` enum is defined as $r_3b_2b_1r_0$, where $r_3=0$ and $r_0=1$ are reserved bits that can be changed on a follow up specification an allowing left and right padding of the stages enumerate, such that further functionality can be interleaved in the modelling pipeline. From the PoV of each model the `PipelinedSuperType` is each SuperType enumeration and shouldn't care about the details apart from setting these bits correctly.

| C   | Emission | C   | MCRT | C   | Detection | C   | Processing | C   | Unordered C |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 0   | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   | 9-15 |

C = Custom

### SuperType events: 4-bits

> [NOTE] From here on we are only talking about types referring to the MCRT/Aetherus events

Aetherus Event SuperTypes:

- Interface
- Reflector `Mirror <: Reflector`
- Material

These are just given values in the order described as an enum:

```c
enum SuperType : uint4 {
    Interface,
    Reflector,
    Material,
    // CustomCodes
}
```

### SubTypes events: 8-bits

Now looking under the hierarchy of each events described by the SuperType above, we can build the hierarchy as follows:

- Interface
    - Reflect
    - Refract
    - ReEmittance [Biomedical Optics: Principles and Imaging](zotero://select/library/items/P88YCXIT) => Include MCRT Result for an object block
        - Reflectance $R_d(r_{out}, \theta_{out}, r_{in}, \theta_{in})$
        - Transmittance $T_d(r_{out}, \theta_{out}, r_{in}, \theta_{in})$
        - ==WARN:== Only defined for a pair of surfaces (front/back), side walls only use `Reflector` type interaction => Edge effects can't be reproduced and side walls must be quite small to limit divergance from this model due to in/out photons through these surfaces.
- Reflector
    - Diffuse
    - Specular `<: Mirror`
    - RetroReflector
    - BRDF (Bi-directional Reflection Distribution Function)
- Material
    - Raman
    - Fluorescence
    - Scatter
        - Heyney-Greenstein | Mie | Rayleigh | SphericalCDF
            - ForwardScatter
            - SideScatter
            - BackwardScatter
            - Any

#### Material SubType

Material events have a particular labelling for scattered photons, that describe the scattering model used, but also the direction the photons are scattered.  
All volume/material interaction are scattering or absorption, however only scattering keeps propagating as events, but maybe we should keep track of absorption as well

| SubType enum | Material Interaction | Scatter Type | Direction |
| ---          | ---                  | ---          | ---       |
| 7 - 6        | 5 - 4                | 3 - 2        | 1 - 0     |
| Material     | Absorption           | 0            | 0         |
| Material     | InelasticScatter     | Raman        | NA        |
| Material     | InelasticScatter     | Fluorescence | NA        |
| Material     | ElasticScatter       | HG           | Any       |
| Material     | ElasticScatter       | HG           | Forward   |
| Material     | ElasticScatter       | HG           | Side      |
| Material     | ElasticScatter       | HG           | Backward  |
| Material     | ElasticScatter       | Mie          | ...       |
| Material     | ElasticScatter       | Rayleigh     | ...       |
| Material     | ElasticScatter       | SphericalCDF | ...       |

### Material/Surface ID: 16-bits

Each surface and material are described by an ID. The ID don't have to be unique, i.e. multiple surfaces or objects can map to the same ID.

Then these scene is described by 2 HashMaps `Surface -> MatSurfID` and `Material -> MatSurfID`.

These IDs are decided on at runtime based on the scene that is composed, but we can restrict the values such that are useful for downstream processing.  
The rules described below are loose, but are desirable to be implement in order to be easier to discern objects in the scene.

I) Use the same ID for Material and Surface of the same object, hence, there will be a single ID to query for in the `MatSurfID` if photons interacted with certain object at all.  
II) Group together multiple objects to map to the same ID if it's not necessary to separate them, as it will make filtering easier. Then the Surface and Material HashMaps will have multiple entries mapping to the same `MatSurfID`. This could be of interest for multiple objects that compose the far-field objects that are not interest in the scene.

## Filtering DSL

Using BNF, EBNF or parsing expression grammar (PEG), we want to describe the
syntax of the DSL that helps us easily describe filtering rules for our detected
photons.

> [!WARNING]
> The filtering DSL has been developed in its own standalone crate at
> [eldritch-trace](https://github.com/aetherus-wg/eldritch-trace)

Resources
- [BNF and EBNF playground](https://bnfplayground.pauliankline.com/) to describe
  context-free grammar for our parser
- [Pest using PEG with palyground](https://pest.rs/)
- [PEG parsers
description](https://we-like-parsers.github.io/pegen/peg_parsers.html)
- [Excelent rundown of
DSLs](https://martinfowler.com/articles/languageWorkbench.html)
