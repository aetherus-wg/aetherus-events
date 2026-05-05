pub use crate::events::{EventId, EventType};
pub use crate::ledger::Ledger;
pub use crate::mcrt_event;
pub use crate::src::SrcId;
pub use crate::uid::Uid;

const MAP_SIZE: usize = 4;

use crate::ledger::{
    LedgerTree as GenericLedgerTree,
    LedgerNode as GenericLedgerNode,
};
use crate::maps::SmallMap;

/// Type alias for the LedgerTree that was found to be optimal to use for MCRT simulations.
pub type LedgerTree = GenericLedgerTree<u32, SmallMap<u32, MAP_SIZE>>;
pub type LedgerNode = GenericLedgerNode<u32, SmallMap<u32, MAP_SIZE>>;
