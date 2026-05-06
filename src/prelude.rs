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
type LedgerMap<T> = SmallMap<T, MAP_SIZE>;
pub type LedgerTree = GenericLedgerTree<u32, LedgerMap<u32>>;
pub type LedgerNode = GenericLedgerNode<u32, LedgerMap<u32>>;
