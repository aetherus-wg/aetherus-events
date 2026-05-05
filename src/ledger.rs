//! Event ledger for tracking photon event chains.
//!
//! The `Ledger` maintains a record of photon event sequences,
//! enabling forward and backward traversal through the event tree.
//!
//! ## Structure
//!
//! Each event is identified by a `Uid` (unique identifier) containing:
//! - `seq_id`: Sequence number in the chain
//! - `event`: 32-bit encoded event type
//!
//! ## Usage
//!
//! ```rust
//! use aetherus_events::prelude::*;
//! use aetherus_events::mcrt_event;
//! use aetherus_events::events::Emission;
//!
//! let mut ledger = LedgerTree::new();
//!
//! // Insert start event (photon emission)
//! let start = ledger.root().insert(EventId::new(
//!     EventType::Emission(Emission::PointSource),
//!     SrcId::Light(1)
//! ));
//!
//! // Add subsequent events
//! let next = start.insert(EventId::new_mcrt(
//!     mcrt_event!(Material, Elastic, Mie, Forward),
//!     SrcId::Mat(1)
//! ));
//!
//! // Resolve LedgerTree before accessing
//! ledger.resolve();
//!
//! // Get the chain
//! let chain = next.get_chain();
//! ```

use log::{error, warn};
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeAs, SerializeAs};
use serde_with::{DisplayFromStr, serde_as};
use std::cell::OnceCell;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock, Weak};
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use crate::filter::BitsProperty;
use crate::maps::EventMap;
use crate::{Decode, RawEvent};
use crate::EventId;
use crate::src::SrcId;
use crate::uid::Uid;

use serde_json;
use std::fs::File;

use std::hash::Hash;

use thousands::Separable;

/// Named source identifier for humans.
///
/// Associates a name with a source ID for readable output.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Hash, Eq)]
pub enum SrcName {
    Light(String),
    Surf(String),
    MatSurf(String),
    Mat(String),
    Detector(String),
}

#[derive(Debug)]
pub struct LedgerNode<T, M> {
    me:          Weak<LedgerNode<T, M>>,
    parent:      Option<Weak<LedgerNode<T, M>>>,
    // Uid = {seq_no, event}
    seq_no:      OnceCell<u32>,
    event:       T,
    next_seq_no: OnceCell<u32>,
    children:    RwLock<M>,
    cnt:         AtomicU32,
}

// WARN: We decide to implement Send + Sync here for LedgerNode,
// assuming that writting of `seq_no` and `next_seq_no` will never happen i
// during multi-threaded execution
unsafe impl<T, M> Send for LedgerNode<T, M> {}
unsafe impl<T, M> Sync for LedgerNode<T, M> {}

impl<T, M> LedgerNode<T, M>
where
    T: RawEvent,
    M: EventMap<T, Arc<LedgerNode<T, M>>>,
{
    pub fn root() -> Arc<Self> {
        Arc::new_cyclic(|me| Self {
            me:          me.clone(),
            seq_no:      OnceCell::new(),
            next_seq_no: 0.into(),
            event:       T::default(),
            parent:      None,
            children:    RwLock::new(M::new()),
            cnt:         AtomicU32::new(0),
        })
    }

    pub fn from_parent(parent: &Arc<Self>, event: T) -> Arc<Self> {
        let seq_no = parent.next_seq_no.clone();
        Arc::new_cyclic(|me| Self {
            me: me.clone(),
            seq_no,
            next_seq_no: OnceCell::new(),
            event,
            parent: Some(Arc::downgrade(parent)),
            children: RwLock::new(M::new()),
            cnt: AtomicU32::new(0),
        })
    }

    pub fn new_children(&self, event: T) -> Arc<Self> {
        let new_node = Self::from_parent(&self.me.upgrade().unwrap(), event.clone());
        self.children
            .write()
            .unwrap()
            .insert(event, new_node.clone());
        new_node.clone()
    }

    pub fn children(&self) -> Vec<Arc<Self>> {
        self.children
            .read()
            .unwrap()
            .values()
            .cloned()
            .collect()
    }

    //fn with_seq_no(&self, seq_no: u32) {
    //    self.seq_no.set(seq_no).unwrap_or_else(|_|
    //        panic!("Failed to set seq_no for node with event _ during ledger tree reconstruction")
    //    );
    //}

    fn with_next_seq_no(&self, next_seq_no: u32) {
        self.next_seq_no.set(next_seq_no).unwrap_or_else(|_|
            panic!("Failed to set next_seq_no {} for node in ledger tree reconstruction", next_seq_no)
        );
    }

    pub fn event(&self) -> &T {
        &self.event
    }

    pub fn uid(&self) -> Option<Uid> {
        self.seq_no
            .get()
            .map(|&seq_no| Uid::new(seq_no, self.event.clone().into()))
    }

    pub fn insert(&self, event: impl Into<T>) -> Arc<Self> {
        let raw_event = event.into();
        let mut cnt = self.cnt.load(Ordering::Relaxed);
        loop {
            if let Some(next) = self.children.read().unwrap().get(&raw_event) {
                return next.clone();
            } else {
                let mut writer = self.children.write().unwrap();
                match self.cnt.compare_exchange(cnt, cnt + 1, Ordering::Acquire, Ordering::Relaxed) {
                    Ok(_) => {
                        // Successfully reserved the next sequence number, now insert the new node
                        let new_node =
                            LedgerNode::from_parent(&self.me.upgrade().unwrap(), raw_event.clone());
                        writer.insert(raw_event, new_node.clone());
                        return new_node;
                    }
                    Err(e) => cnt = e,
                }
            }
        }
    }

    pub fn get_chain(&self) -> Vec<Uid> {
        let mut chain = Vec::new();
        chain.push(self.uid().unwrap());
        let mut parent = self.parent.clone();
        while parent.is_some() {
            let node = parent.as_ref().unwrap().upgrade().unwrap();
            // Avoid processing root node
            if node.seq_no.get().is_some() {
                let uid = node.uid().unwrap();
                chain.push(uid);
                parent = node.parent.clone();
            } else {
                parent = None;
            }
        }
        chain.reverse();
        chain
    }

    /// WARN: This is meant to be called only with dangling UIDs
    pub fn prune(&self) {
        let mut event_type = self.event.clone();

        // 1. First clear all children this node references
        self.children.write().unwrap().clear();

        // 2. Walk up the tree and remove any reference untill we meet a node that bifurcates
        let mut node = self.parent.as_ref().unwrap().clone();
        loop {
            let access_node = node.upgrade().unwrap();
            access_node.children.write().unwrap().remove(&event_type);
            event_type = access_node.event.clone();

            if !access_node.children.read().unwrap().is_empty() {
                // Biffurcation point, stop pruning
                break;
            } else if let Some(parent_ref) = &access_node.parent {
                node = parent_ref.clone();
            } else {
                // Reached the root, stop pruning
                break;
            }
        }
    }

    pub fn get_end_nodes(&self) -> Vec<Arc<Self>> {
        let mut end_nodes = Vec::new();

        let mut stack_nodes = Vec::new();
        stack_nodes.push(self.me.upgrade().unwrap());

        while let Some(node) = stack_nodes.pop() {
            if node.children.read().unwrap().is_empty() {
                // Check that this is not the root node
                if node.next_seq_no.get() != Some(&0) {
                    end_nodes.push(node);
                }
            } else {
                stack_nodes.extend(
                    node.children
                        .read()
                        .unwrap()
                        .values()
                        .cloned()
                );
            }
        }
        end_nodes
    }
}

pub struct LedgerTree<T, M>
where
    M: EventMap<T, Arc<LedgerNode<T, M>>>,
    T: RawEvent,
{
    grps:    HashMap<String, SrcId>,       // Key: Group name
    src_map: HashMap<SrcId, Vec<SrcName>>, // Value: Material name, object name, light name.

    next_mat_id:     u16,
    next_surf_id:    u16,
    next_matsurf_id: u16,
    next_light_id:   u16,

    next_seq_no: u32,

    // This should be an EventType::Root
    root:     Arc<LedgerNode<T, M>>,
    // WARN: This should hold a weak pointer in order to allow cleanup when tree is pruned
    node_map: HashMap<Uid, Weak<LedgerNode<T, M>>>,

    // Attempts to keep track of when new elements have been added to the Ledger
    dirty: bool,
}

// Implement a deep copy of the tree and avoid referencing to nodes
// from the original tree
impl<T, M> Clone for LedgerTree<T, M>
where
    T: RawEvent,
    M: EventMap<T, Arc<LedgerNode<T, M>>>,
{
    fn clone(&self) -> Self {
        fn clone_node<T, M>(
            node: &Arc<LedgerNode<T, M>>,
            parent: Option<Weak<LedgerNode<T, M>>>,
        ) -> Arc<LedgerNode<T, M>>
        where
            T: RawEvent,
            M: EventMap<T, Arc<LedgerNode<T, M>>>,
        {
            let new_node = Arc::new_cyclic(|me| LedgerNode {
                me: me.clone(),
                parent,
                seq_no: node.seq_no.clone(),
                event: node.event.clone(),
                next_seq_no: node.next_seq_no.clone(),
                children: RwLock::new(M::new()),
                cnt: AtomicU32::new(node.cnt.load(Ordering::Relaxed)),
            });

            let children = node.children.read().unwrap();
            for child in children.values() {
                let child_event = child.event.clone();
                let new_child = clone_node(child, Some(Arc::downgrade(&new_node)));
                new_node
                    .children
                    .write()
                    .unwrap()
                    .insert(child_event, new_child);
            }

            new_node
        }

        let root = clone_node(&self.root, None);

        // Rebuild node map
        let mut node_map      = HashMap::new();
        let mut resolve_stack = Vec::new();
        let node = self.root.clone();

        for child in node.children.read().unwrap().values() {
            resolve_stack.push(child.clone());
        }
        while let Some(node) = resolve_stack.pop() {
            if let Some(uid) = node.uid() {
                node_map.insert(uid, Arc::downgrade(&node));
                for child in node.children.read().unwrap().values() {
                    resolve_stack.push(child.clone());
                }
            }
        }

        Self {
            grps: self.grps.clone(),
            src_map: self.src_map.clone(),
            next_mat_id: self.next_mat_id,
            next_surf_id: self.next_surf_id,
            next_matsurf_id: self.next_matsurf_id,
            next_light_id: self.next_light_id,
            root,
            node_map,
            dirty: self.dirty,
            next_seq_no: self.next_seq_no,
        }
    }
}

impl<T, M> Default for LedgerTree<T, M>
where
    T: RawEvent,
    M: EventMap<T, Arc<LedgerNode<T, M>>>,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T, M> LedgerTree<T, M>
where
    T: RawEvent,
    M: EventMap<T, Arc<LedgerNode<T, M>>>,
{
    pub fn new() -> Self {
        Self {
            grps:            HashMap::new(),
            src_map:         HashMap::new(),
            next_mat_id:     0,
            next_surf_id:    0,
            next_matsurf_id: u16::MAX,
            next_light_id:   0,
            root:            LedgerNode::<T, M>::root(),
            node_map:        HashMap::new(),
            // TODO: How to reliably detect mutation?
            dirty:           false,
            next_seq_no:     1,
        }
    }
    pub fn root(&self) -> &Arc<LedgerNode<T, M>> {
        &self.root
    }
    fn check_ids(&self) {
        if self.next_mat_id >= self.next_matsurf_id {
            warn!("Material ID and Material-Surface ID ranges are overlapping");
        }
        if self.next_surf_id >= self.next_matsurf_id {
            warn!("Surface ID and Material-Surface ID ranges are overlapping");
        }
    }
    pub fn with_light(&mut self, light_name: String) -> SrcId {
        let light_id = SrcId::Light(self.next_light_id);
        self.next_light_id += 1;
        match self.src_map.get_mut(&light_id) {
            Some(_value) => {
                panic!("Light ID {} already exists in src_map", *light_id);
                //value.push(SrcName::Light(light_name))
            }
            None => {
                self.src_map
                    .insert(light_id, vec![SrcName::Light(light_name)]);
            }
        };
        light_id
    }

    pub fn with_surf(&mut self, obj_name: String, grp: Option<String>) -> SrcId {
        let src_id = if let Some(grp_name) = grp {
            let src_id = match self.grps.get(&grp_name) {
                Some(src_id) => *src_id,
                None => {
                    // Create new SurfId
                    let surf_id = SrcId::Surf(self.next_surf_id);
                    self.next_surf_id += 1;
                    self.grps.insert(grp_name.clone(), surf_id);
                    surf_id
                }
            };

            match src_id {
                SrcId::Surf(_) => src_id,
                SrcId::MatSurf(_) => src_id,
                SrcId::Mat(_) => {
                    let matsurf_id = self.next_matsurf_id;
                    self.next_matsurf_id -= 1;

                    warn!(
                        "Discarding {:?} and allocate MatSurf({}), moving Map({:?}) to Map(Mat({}))",
                        src_id, matsurf_id, src_id, matsurf_id
                    );
                    if let Some(mat_names) = self.src_map.remove(&SrcId::Mat(*src_id)) {
                        self.src_map.insert(SrcId::Mat(matsurf_id), mat_names);
                    } else {
                        panic!("Material ID {} not found in src_map", *src_id);
                    }

                    SrcId::MatSurf(matsurf_id)
                }
                SrcId::Light(_) => {
                    panic!("Group name {} already used for a light source", grp_name);
                }
                SrcId::Detector(_) => {
                    panic!("Group name {} already used for a detector sink", grp_name);
                }
                SrcId::SrcId(_) => {
                    panic!("SrcId reserved as an abstract super type");
                }
                SrcId::None => {
                    panic!("Group name {} registered an invalid None source", grp_name);
                }
            }
        } else {
            let surf_id = SrcId::Surf(self.next_surf_id);
            self.next_surf_id += 1;
            surf_id
        };

        match self.src_map.get_mut(&src_id) {
            Some(value) => value.push(SrcName::Surf(obj_name)),
            None => {
                self.src_map.insert(src_id, vec![SrcName::Surf(obj_name)]);
            }
        };

        self.check_ids();

        src_id
    }

    // NOTE: Materials are not grouped, only objects are
    // FIXME: Is `with_mat` necessary? Materials are always paird with surfaces, apart from
    // boundary, which can also be considered a special case of a surface
    pub fn with_mat(&mut self, mat_name: String) -> SrcId {
        let mat_id = SrcId::Mat(self.next_mat_id);
        self.next_mat_id += 1;

        match self.src_map.get_mut(&mat_id) {
            Some(value) => value.push(SrcName::Mat(mat_name)),
            None => {
                self.src_map.insert(mat_id, vec![SrcName::Mat(mat_name)]);
            }
        };

        self.check_ids();

        mat_id
    }

    pub fn with_matsurf(
        &mut self,
        obj_name: String,
        mat_name: String,
        grp: Option<String>,
    ) -> SrcId {
        let src_id = if let Some(grp_name) = grp {
            let src_id = match self.grps.get(&grp_name) {
                Some(src_id) => *src_id,
                None => {
                    // Create new MatId
                    let surf_id = SrcId::MatSurf(self.next_matsurf_id);
                    self.next_matsurf_id -= 1;
                    self.grps.insert(grp_name.clone(), surf_id);
                    surf_id
                }
            };

            match src_id {
                SrcId::MatSurf(_) => src_id,
                SrcId::Surf(_) | SrcId::Mat(_) => {
                    let matsurf_id = self.next_matsurf_id;
                    self.next_matsurf_id -= 1;

                    match src_id {
                        SrcId::Surf(_) => {
                            warn!(
                                "Discarding {:?} and allocate MatSurf({}), moving Map({:?}) to Map(Surf({}))",
                                src_id, matsurf_id, src_id, matsurf_id
                            );
                            if let Some(surf_names) = self.src_map.remove(&src_id) {
                                self.src_map.insert(SrcId::Surf(matsurf_id), surf_names);
                            } else {
                                panic!("Surface ID {} not found in src_map", *src_id);
                            }
                        }
                        SrcId::Mat(_) => {
                            warn!(
                                "Discarding {:?} and allocate MatSurf({}), moving Map({:?}) to Map(Mat({}))",
                                src_id, matsurf_id, src_id, matsurf_id
                            );
                            if let Some(surf_names) = self.src_map.remove(&src_id) {
                                self.src_map.insert(SrcId::Mat(matsurf_id), surf_names);
                            } else {
                                panic!("Surface ID {} not found in src_map", *src_id);
                            }
                        }
                        _ => {}
                    };

                    SrcId::MatSurf(matsurf_id)
                }
                SrcId::Light(_) => {
                    panic!("Group name {} already used for a light source", grp_name);
                }
                SrcId::Detector(_) => {
                    panic!("Group name {} already used for a detector sink", grp_name);
                }
                SrcId::SrcId(_) => {
                    panic!("SrcId reserved as an abstract super type");
                }
                SrcId::None => {
                    panic!("Group name {} registered an invalid None source", grp_name);
                }
            }
        } else {
            let surf_id = SrcId::MatSurf(self.next_matsurf_id);
            self.next_matsurf_id -= 1;
            surf_id
        };

        let matsurf_name = format!("{}:{}", obj_name, mat_name);
        match self.src_map.get_mut(&src_id) {
            Some(value) => value.push(SrcName::MatSurf(matsurf_name)),
            None => {
                self.src_map
                    .insert(src_id, vec![SrcName::MatSurf(matsurf_name)]);
            }
        };

        self.check_ids();

        src_id
    }

    fn check_dirty(&self) {
        if self.dirty {
            error!("Ledger is dirty, get_chain may return incomplete results! Run `ledger.resolve()` before");
        }
    }

    pub fn resolve(&mut self) {
        let mut resolve_stack: Vec<(Arc<LedgerNode<T, M>>, u32)> = Vec::new();
        let node = self.root.clone();

        for child in node.children.read().unwrap().values() {
            resolve_stack.push((child.clone(), *node.next_seq_no.get_or_init(|| 0)));
        }

        while let Some((node, seq_no)) = resolve_stack.pop() {
            let set_seq_no = *node.seq_no.get_or_init(|| seq_no);
            assert_eq!(
                set_seq_no, seq_no,
                "Sequence number mismatch during ledger resolution, set vs expected"
            );

            let next_seq_no = *node.next_seq_no.get_or_init(|| {
                let seq_no = self.next_seq_no;
                self.next_seq_no += 1;
                seq_no
            });

            self.node_map
                .insert(node.uid().unwrap(), Arc::downgrade(&node));

            for child in node.children.read().unwrap().values() {
                resolve_stack.push((child.clone(), next_seq_no));
            }
        }

        self.dirty = false;
    }

    pub fn get_next(&self, uid: &Uid) -> Vec<Uid> {
        self.check_dirty();
        let node = self
            .node_map
            .get(uid)
            .unwrap_or_else(|| panic!("UID {} not found in ledger", uid));
        let access_node = node.upgrade().unwrap();
        let children_map = access_node.children.read().unwrap();
        children_map
            .values()
            .map(|node| node.uid().unwrap())
            .collect()
    }

    pub fn get_src_dict(&self) -> HashMap<SrcName, SrcId> {
        let mut src_dict = HashMap::new();
        for (src_id, src_names) in &self.src_map {
            for src_name in src_names {
                src_dict.insert(src_name.clone(), *src_id);
            }
        }
        src_dict
    }

    pub fn get_chain(&self, uid: &Uid) -> Vec<Uid> {
        self.check_dirty();
        if let Some(node) = self.node_map.get(uid) {
            if let Some(node) = node.upgrade() {
                node.get_chain()
            } else {
                panic!("UID {} not found in ledger", uid);
            }
        } else {
            panic!("UID {} not found in ledger", uid);
        }
    }

    pub fn emit_dot<'a, I>(&self, uids: I) -> String
    where
        I: IntoIterator<Item = &'a Uid>,
    {
        self.emit_dot_with_freq(uids, &HashMap::new())
    }

    pub fn emit_dot_with_freq<'a, I>(&self, uids: I, freq_dict: &HashMap<Uid, usize>) -> String
    where
        I: IntoIterator<Item = &'a Uid>,
    {
        let mut dot = String::from("digraph Ledger {\n  rankdir=LR;\n  node [shape=record];\n");
        let mut nodes = HashSet::new();
        let mut pairs = HashSet::new();

        let with_cnt = !freq_dict.is_empty();

        for uid in uids {
            let chain_uids = self.get_chain(uid);
            for chain_uid in chain_uids.iter() {
                let event = EventId::decode(chain_uid.event);
                if !nodes.contains(chain_uid) {
                    let event_str = format!("{}", event).replace("|", "\\|");
                    if with_cnt && let Some(cnt) = freq_dict.get(chain_uid) {
                        dot.push_str(&format!(
                            "  n{:016X} [label=\"{{<l>{}|{}|<r>cnt({})}}\"];\n",
                            chain_uid.encode(),
                            chain_uid.seq_id,
                            event_str,
                            cnt.separate_with_commas(),
                        ));
                    } else {
                        dot.push_str(&format!(
                            "  n{:016X} [label=\"{{<l>{}|<r>{}}}\"];\n",
                            chain_uid.encode(),
                            chain_uid.seq_id,
                            event_str,
                        ));
                    }
                    nodes.insert(*chain_uid);
                }
            }
            for (chain_uid, chain_uid_next) in chain_uids.windows(2).map(|w| (w[0], w[1])) {
                if !pairs.contains(&(chain_uid, chain_uid_next)) {
                    dot.push_str(&format!(
                        "  n{:016X}:r -> n{:016X}:l;\n",
                        chain_uid.encode(),
                        chain_uid_next.encode()
                    ));
                    pairs.insert((chain_uid, chain_uid_next));
                }
            }
        }
        dot.push_str("}\n");
        dot
    }

    pub fn prune_node(&mut self, node: &Arc<LedgerNode<T, M>>) {
        self.check_dirty();
        if let Some(uid) = node.uid() {
            self.node_map.remove(&uid);
        }
        node.prune();
    }

    pub fn prune_uid(&mut self, uid: &Uid) {
        self.check_dirty();
        if let Some(node) = self.node_map.get(uid) {
            if let Some(node) = node.upgrade() {
                self.prune_node(&node);
            } else {
                panic!("UID {} not found in ledger", uid);
            }
        } else {
            panic!("UID {} not found in ledger", uid);
        }
    }

    pub fn get_dangling_uids(&self) -> Vec<Uid> {
        self.check_dirty();
        let end_nodes = self.root.get_end_nodes();
        end_nodes.iter().map(|node| node.uid().unwrap()).collect()
    }

    pub fn get_node(&self, uid: &Uid) -> Option<Arc<LedgerNode<T, M>>> {
        self.check_dirty();
        if let Some(node) = self.node_map.get(uid) {
            node.upgrade()
        } else {
            None
        }
    }

    pub fn find_dangling_uids(&self, bits_property: BitsProperty) -> Vec<Uid> {
        let mut found_uids: Vec<Uid> = Vec::new();
        for end_node in self.root.get_end_nodes() {
            let uid = end_node.uid().unwrap();
            if bits_property.matches(uid.event) {
                found_uids.push(uid);
            }
        }
        found_uids
    }
}

impl<T, M> From<LedgerTree<T, M>> for Ledger
where
    M: EventMap<T, Arc<LedgerNode<T, M>>>,
    T: RawEvent,
{
    fn from(tree: LedgerTree<T, M>) -> Self {
        Self::from(&tree)
    }
}

impl<T, M> From<&LedgerTree<T, M>> for Ledger
where
    T: RawEvent,
    M: EventMap<T, Arc<LedgerNode<T, M>>>,
{
    fn from(tree: &LedgerTree<T, M>) -> Self {
        let mut ledger = Ledger::new();

        ledger.grps            = tree.grps.clone();
        ledger.src_map         = tree.src_map.clone();
        ledger.next_mat_id     = tree.next_mat_id;
        ledger.next_surf_id    = tree.next_surf_id;
        ledger.next_matsurf_id = tree.next_matsurf_id;
        ledger.next_light_id   = tree.next_light_id;
        ledger.next_seq_id     = tree.next_seq_no;

        // Traverse the resolved tree and reconstruct next / prev maps.
        // We do a DFS from the root.
        let mut stack = vec![];
        for child in tree.root.children.read().unwrap().values() {
            ledger.insert_start(child.uid().unwrap());
            stack.push((child.uid().unwrap(), child.clone()));
        }

        while let Some((prev_uid, node)) = stack.pop() {
            for child in node.children.read().unwrap().values() {
                ledger.insert(
                    prev_uid,
                    child.uid().unwrap(),
                    *child.next_seq_no.get().unwrap(),
                );
                stack.push((child.uid().unwrap(), child.clone()));
            }
        }

        ledger
    }
}

impl<T, M> From<Ledger> for LedgerTree<T, M>
where
    T: RawEvent,
    M: EventMap<T, Arc<LedgerNode<T, M>>>,
{
    fn from(value: Ledger) -> Self {
        Self::from(&value)
    }
}

impl<T, M> From<&Ledger> for LedgerTree<T, M>
where
    T: RawEvent,
    M: EventMap<T, Arc<LedgerNode<T, M>>>,
{
    fn from(value: &Ledger) -> Self {
        let mut tree = LedgerTree::<T, M>::new();

        tree.grps            = value.grps.clone();
        tree.src_map         = value.src_map.clone();
        tree.next_mat_id     = value.next_mat_id;
        tree.next_surf_id    = value.next_surf_id;
        tree.next_matsurf_id = value.next_matsurf_id;
        tree.next_light_id   = value.next_light_id;
        tree.next_seq_no     = value.next_seq_id;

        // Rebuild root
        tree.root = LedgerNode::<T, M>::root();

        let mut stack = Vec::new();

        for &uid in value.start_events().iter() {
            let node = tree.root.new_children(uid.event.into());
            tree.node_map.insert(uid, Arc::downgrade(&node));

            stack.push((uid, node.clone()));
        }

        while let Some((prev_uid, parent_node)) = stack.pop() {
            if let Some(next_seq_no) = value.get_next_seq_id(&prev_uid) {
                parent_node.with_next_seq_no(next_seq_no);
            }

            for uid in value.get_next(&prev_uid) {
                let new_node = parent_node.new_children(uid.event.into());
                tree.node_map.insert(uid, Arc::downgrade(&new_node));

                stack.push((uid, new_node.clone()));
            }
        }

        tree.dirty = false;
        tree
    }
}

// ----------------------------------------------------
// Definition of Ledger struct and methods
// ----------------------------------------------------
// - write ledger to JSON file
// - Ledger methods:
//   - Initialise sources: Materials, Surfaces, Lights, etc
//   - Group sources for batch ID
//   - insert events and build the event chain
//   - query events, using the next/prev maps as a doubled linked list

pub fn write_ledger_to_json<P>(ledger: &Ledger, file_path: P) -> Result<(), serde_json::Error>
where
    P: AsRef<std::path::Path>,
{
    // Write the JSON string to a file
    let file = File::create(file_path).expect("Unable to create file");
    serde_json::to_writer_pretty(file, ledger)
}

/// Event ledger for tracking photon event chains.
///
/// Maintains a record of all photon events with:
/// - Source mappings (src_map): Named sources mapped to source IDs
/// - Event chains (next/prev): Linked lists of event sequences
/// - Start events (start_events): Root events, which have no previous event cause
#[serde_as]
#[derive(Serialize, Deserialize, Default)]
pub struct Ledger {
    grps:         HashMap<String, SrcId>, // Key: Group name
    #[serde_as(as = "HashMap<DisplayFromStr, _>")]
    src_map:      HashMap<SrcId, Vec<SrcName>>, // Value: Material name, object name, light name.
    start_events: Vec<Uid>,

    next_mat_id:     u16,
    next_surf_id:    u16,
    next_matsurf_id: u16,
    next_light_id:   u16,

    // Use a nested map: (uid.seq_id -> (uid.event -> next_seq_id)) instead of uid -> next_seq_id
    // in order to retrive children of an event easily. This enables a tree search.
    #[serde_as(as = "HashMap<_, HexInnerMap>")]
    next:        HashMap<u32, HashMap<u32, u32>>,
    #[serde_as(as = "HashMap<_, DisplayFromStr>")]
    prev:        HashMap<u32, Uid>,
    next_seq_id: u32,
}

impl Ledger {
    pub fn new() -> Self {
        Self {
            grps:            HashMap::new(),
            src_map:         HashMap::new(),
            start_events:    Vec::new(),
            next_mat_id:     0,
            next_surf_id:    0,
            next_matsurf_id: u16::MAX,
            next_light_id:   0,
            next:            HashMap::new(),
            prev:            HashMap::new(),
            next_seq_id:     0,
        }
    }

    pub fn start_events(&self) -> &Vec<Uid> {
        &self.start_events
    }

    pub fn insert_start(&mut self, uid: Uid) {
        self.insert_entry(uid, 1);
        self.start_events.push(uid);
    }

    // WARN: next_seq_id increment overflows silently in release mode, however that is unlikely to
    // happen unless the simulation scene is extremely complex
    pub fn insert(&mut self, prev_event: Uid, next_event: Uid, next_next_seq_no: u32) {
        // Push a new entry in next with the new_event UID
        // Obs: seq_id=0 is reserved for root identification, hence all new events with no
        // previous cause start with seq_id=0
        let next_seq_id = self
            .get_next_seq_id(&prev_event)
            .ok_or("Previous event not found in ledger")
            .unwrap();
        assert_eq!(
            next_seq_id, next_event.seq_id,
            "Previous event next_seq_no and new event seq_no don't match"
        );

        self.insert_entry(next_event, next_next_seq_no);
    }

    fn insert_entry(&mut self, uid: Uid, next_seq_id: u32) {
        assert!(self.get_next_seq_id(&uid).is_none());
        self.next.entry(uid.seq_id).or_default();
        self.next
            .get_mut(&uid.seq_id)
            .unwrap()
            .insert(uid.event, next_seq_id);
        self.prev.insert(next_seq_id, uid);
        // Prepare the next seq_id entry
        self.next.insert(next_seq_id, HashMap::new());
    }

    pub fn get_next_seq_id(&self, uid: &Uid) -> Option<u32> {
        match self.next.get(&uid.seq_id) {
            None => None,
            Some(map) => map.get(&uid.event).cloned(),
        }
    }

    pub fn get_next(&self, uid: &Uid) -> Vec<Uid> {
        let mut next_uids = Vec::new();
        if let Some(next_seq_id) = self.get_next_seq_id(uid)
            && let Some(map) = self.next.get(&next_seq_id)
        {
            for next_event in map.keys() {
                let next_uid = Uid::new(next_seq_id, *next_event);
                next_uids.push(next_uid);
            }
        }
        next_uids
    }
}

// ----------------------------------------------------
// Helper methods and structs
// ----------------------------------------------------
// - Custom serializer/deserializer for HashMap<u32, u32> with hex keys

pub struct HexInnerMap;

impl SerializeAs<HashMap<u32, u32>> for HexInnerMap {
    fn serialize_as<S>(value: &HashMap<u32, u32>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        let mut map = serializer.serialize_map(Some(value.len()))?;
        for (k, v) in value {
            let key = format!("0x{:08X}", k); // hex key
            map.serialize_entry(&key, v)?;
        }
        map.end()
    }
}

impl<'de> DeserializeAs<'de, HashMap<u32, u32>> for HexInnerMap {
    fn deserialize_as<D>(deserializer: D) -> Result<HashMap<u32, u32>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{Error as DeError, MapAccess, Visitor};
        use std::collections::HashMap as StdHashMap;
        use std::fmt;

        struct HexInnerVisitor;

        impl<'de> Visitor<'de> for HexInnerVisitor {
            type Value = HashMap<u32, u32>;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("map with hex-encoded u32 keys")
            }

            fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut out = StdHashMap::new();
                while let Some((k, v)) = access.next_entry::<String, u32>()? {
                    let key = u32::from_str_radix(&k[2..10], 16)
                        .map_err(|e| A::Error::custom(format!("invalid hex key {k}: {e}")))?;
                    out.insert(key, v);
                }
                Ok(out)
            }
        }

        deserializer.deserialize_map(HexInnerVisitor)
    }
}

#[cfg(test)]
mod tests {
    use crate::RawEvent;
    use crate::events::Emission;
    use crate::events::EventType;
    use crate::filter::BitsProperty;
    use crate::maps::SmallMap;
    use crate::mcrt_event;
    use crate::pattern;

    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn produce_src_id() {
        let surfs = vec![
            "surf1".to_string(),
            "surf2".to_string(),
            "surf3".to_string(),
        ];
        let mats = vec!["mat1".to_string(), "mat2".to_string()];

        let objects = vec![
            ("obj1".to_string(), "mat1".to_string()),
            ("obj2".to_string(), "mat2".to_string()),
            ("obj3".to_string(), "mat1".to_string()),
        ];

        let mut ledger_tree = LedgerTree::<u32, SmallMap<u32, 8>>::new();

        for mat in mats {
            let src_id = ledger_tree.with_mat(mat.clone());
            assert!(ledger_tree.src_map.contains_key(&src_id));
            assert_eq!(
                ledger_tree.src_map.get(&src_id).unwrap().to_vec(),
                vec![SrcName::Mat(mat.clone())]
            );
        }

        for surf in surfs {
            let src_id = ledger_tree.with_surf(surf.clone(), None);
            assert!(ledger_tree.src_map.contains_key(&src_id));
            assert_eq!(
                ledger_tree.src_map.get(&src_id).unwrap().to_vec(),
                vec![SrcName::Surf(surf.clone())]
            );
        }

        for (obj, mat) in objects {
            let src_id = ledger_tree.with_matsurf(obj.clone(), mat.clone(), None);
            assert!(ledger_tree.src_map.contains_key(&src_id));
            let expected_name = format!("{}:{}", obj.clone(), mat.clone());
            assert_eq!(
                ledger_tree.src_map.get(&src_id).unwrap().to_vec(),
                vec![SrcName::MatSurf(expected_name)]
            );
        }

        let ledger = Ledger::from(&ledger_tree);

        assert_eq!(ledger.src_map, ledger_tree.src_map);
        assert_eq!(ledger.grps, ledger_tree.grps);

        // Inspect the ledger
        println!("Ledger src_map: {:?}", ledger_tree.src_map);
    }

    #[test]
    fn insert_events() {
        let mut ledger = LedgerTree::<u32, SmallMap<u32, 8>>::new();
        let root_node = ledger.root();

        let emission_event = EventId {
            event_type: EventType::Emission(Emission::PointSource),
            src_id:     SrcId::Light(2),
        };
        let node1 = root_node.insert(emission_event);

        let mcrt_event = EventId {
            event_type: EventType::MCRT(mcrt_event!(Material, Elastic, HenyeyGreenstein, Forward)),
            src_id:     SrcId::Mat(2),
        };
        let node2 = node1.insert(mcrt_event);

        let mcrt_event = EventId {
            event_type: EventType::MCRT(mcrt_event!(Material, Elastic, Mie, Forward)),
            src_id:     SrcId::Mat(2),
        };
        let node3 = node2.insert(mcrt_event);

        ledger.resolve();

        let uid1 = node1.uid().unwrap();
        let uid2 = node2.uid().unwrap();
        let uid3 = node3.uid().unwrap();

        assert_eq!(uid1.seq_id, 0);
        assert_eq!(uid2.seq_id, 1);
        assert_eq!(uid3.seq_id, 2);

        // Check the chain
        let chain = ledger.get_chain(&uid3);
        println!("Chain: {:?}", chain);
        println!(
            "Chain: {:?}",
            chain
                .iter()
                .map(|uid| format!(
                    "Uid(seq_id: {}, event: {:?})",
                    uid.seq_id,
                    uid.event.decode().event_type
                ))
                .collect::<Vec<String>>()
        );
        assert_eq!(chain.len(), 3);
        assert_eq!(chain[0], uid1);
        assert_eq!(chain[1], uid2);
        assert_eq!(chain[2], uid3);
    }

    #[test]
    fn ledger_and_ledger_tree_conversion() {
        let mut ledger_tree = LedgerTree::<u32, SmallMap<u32, 8>>::new();
        let surf_src_id = ledger_tree.with_surf("surface1".to_string(), Some("group1".to_string()));
        let mat_src_id = ledger_tree.with_mat("material1".to_string());
        let emission_event = EventId {
            event_type: EventType::Emission(Emission::PointSource),
            src_id:     SrcId::Light(1),
        };
        let node1 = ledger_tree.root().insert(emission_event);

        let mcrt_event = EventId {
            event_type: EventType::MCRT(mcrt_event!(Interface, Refraction)),
            src_id:     surf_src_id,
        };
        let node2 = node1.insert(mcrt_event);

        let mcrt_event = EventId {
            event_type: EventType::MCRT(mcrt_event!(Material, Elastic, Mie, Forward)),
            src_id:     mat_src_id,
        };
        let _node3 = node2.insert(mcrt_event);

        ledger_tree.resolve();

        let ledger: Ledger = ledger_tree.into();

        let ledger_tree_read: LedgerTree<u32, SmallMap<u32, 8>> = (&ledger).into();
        let ledger_clone: Ledger = ledger_tree_read.into();

        assert_eq!(ledger.grps,         ledger_clone.grps);
        assert_eq!(ledger.src_map,      ledger_clone.src_map);
        assert_eq!(ledger.start_events, ledger_clone.start_events);
        assert_eq!(ledger.next,         ledger_clone.next);
        assert_eq!(ledger.prev,         ledger_clone.prev);
    }

    #[test]
    fn write_ledger_json() {
        let mut ledger_tree = LedgerTree::<u32, SmallMap<u32, 8>>::new();
        let surf_src_id = ledger_tree.with_surf("surface1".to_string(), Some("group1".to_string()));
        let mat_src_id = ledger_tree.with_mat("material1".to_string());
        let emission_event = EventId {
            event_type: EventType::Emission(Emission::PointSource),
            src_id:     SrcId::Light(1),
        };
        let node1 = ledger_tree.root().insert(emission_event);

        let mcrt_event = EventId {
            event_type: EventType::MCRT(mcrt_event!(Interface, Refraction)),
            src_id:     surf_src_id,
        };
        let node2 = node1.insert(mcrt_event);

        let mcrt_event = EventId {
            event_type: EventType::MCRT(mcrt_event!(Material, Elastic, Mie, Forward)),
            src_id:     mat_src_id,
        };
        let node3 = node2.insert(mcrt_event);

        ledger_tree.resolve();

        let uid2 = node2.uid().unwrap();
        let uid3 = node3.uid().unwrap();

        assert_eq!(uid2.seq_id, 1);
        assert_eq!(uid3.seq_id, 2);

        let chain = ledger_tree.get_chain(&uid3);
        println!(
            "Chain: {:?}",
            chain
                .iter()
                .map(|uid| format!(
                    "Uid(seq_id: {}, event: {:?})",
                    uid.seq_id,
                    uid.event.decode().event_type
                ))
                .collect::<Vec<String>>()
        );

        let ledger: Ledger = ledger_tree.into();

        // Create a temporary directory
        let temp_dir = tempdir().expect("Failed to create temporary directory");
        let temp_file_path = temp_dir.path().join("test_ledger.json");
        println!("Temporary file path: {:?}", temp_file_path);
        write_ledger_to_json(&ledger, temp_file_path.to_str().unwrap())
            .expect("Failed to save ledger to JSON.");

        // Keep the temporary directory for inspection
        let _persisted_dir = temp_dir.keep();
        println!(
            "Temporary directory persisted at: {}",
            _persisted_dir.display()
        );

        let stored_ledger: Ledger = {
            let contents: String =
                fs::read_to_string(temp_file_path).expect("Unabel to read json file");
            serde_json::from_str(&contents).expect("Unable to parse ledger file")
            // FIXME: Directly reading from file causes conflict with `de_dexify` which expects a
            // borrowed value
            // let file = File::open(temp_file_path).expect("Unable to open file");
            // serde_json::from_reader(file).expect("Unable to parse ledger file")
        };

        assert_eq!(ledger.grps,         stored_ledger.grps);
        assert_eq!(ledger.src_map,      stored_ledger.src_map);
        assert_eq!(ledger.start_events, stored_ledger.start_events);
        assert_eq!(ledger.next,         stored_ledger.next);
        assert_eq!(ledger.prev,         stored_ledger.prev);
    }

    #[test]
    fn test_prune_dangling_uids() {
        let mut ledger_tree = LedgerTree::<u32, SmallMap<u32, 8>>::new();

        // Populate ledger with non-dangling UIDs
        let node1 = ledger_tree.root().insert(EventId::new(EventType::Detection, SrcId::None));
        let node2 = node1.insert(EventId::new(EventType::Detection, SrcId::None));
        let _node3 = node2.insert(EventId::new(EventType::Detection, SrcId::None));

        ledger_tree.resolve();

        let result = ledger_tree.get_dangling_uids();
        assert_eq!(
            result.len(), 1,
            "Expected exactly one dangling UID in a simple chain"
        );
    }

    #[test]
    fn test_prune_until_bifurcation() {
        let mut ledger_tree = LedgerTree::<u32, SmallMap<u32, 8>>::new();

        // Populate ledger with non-dangling UIDs
        let node_0     = ledger_tree.root()
            .insert(EventId::new_emission(Emission::PencilBeam, SrcId::Light(0)));
        let node_1     = node_0
            .insert(EventId::new_mcrt(mcrt_event!(Material, Elastic, Mie, Unknown), SrcId::Mat(0)));

        let node_21    = node_1
            .insert(EventId::new_mcrt(mcrt_event!(Material, Elastic, Mie, Unknown), SrcId::Mat(0)));
        let node_22    = node_21
            .insert(EventId::new_mcrt(mcrt_event!(Material, Elastic, Mie, Unknown), SrcId::Mat(0)));
        let _node_231  = node_22
            .insert(EventId::new_mcrt(mcrt_event!(Interface, Boundary), SrcId::Surf(0)));
        let node_232   = node_22
            .insert(EventId::new_mcrt(mcrt_event!(Material, Elastic, Mie, Unknown), SrcId::Mat(0)));
        let _node_2321 = node_232
            .insert(EventId::new_mcrt(mcrt_event!(Interface, Boundary), SrcId::Surf(0)));

        let node_31    = node_1
            .insert(EventId::new_mcrt(mcrt_event!(Material, Elastic, Mie, Unknown), SrcId::Mat(0)));
        let _node_32   = node_31
            .insert(EventId::new(EventType::Detection, SrcId::Surf(1)));

        ledger_tree.resolve();

        let dangling_uids = ledger_tree.get_dangling_uids();
        assert_eq!(dangling_uids.len(), 3, "Expected dangling UIDs");
        let dangling_lost_uids = dangling_uids
            .into_iter()
            .filter(|uid| {
                BitsProperty::NoMatch(pattern!(Detection, SrcId::Surf(1))).matches(uid.event)
            })
            .collect::<Vec<_>>();

        assert_eq!(
            dangling_lost_uids.len(),
            2,
            "Expected dangling UID to be pruned"
        );

        //println!("{:?}", ledger);

        for uid in dangling_lost_uids.iter() {
            println!("Pruning UID: {:?}", uid);
            ledger_tree.prune_uid(uid);
        }

        for uid in dangling_lost_uids.iter() {
            assert!(ledger_tree.get_node(uid).is_none());
        }
    }
}
