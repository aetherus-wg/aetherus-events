use std::{iter::Map, slice, sync::Arc};

use smallvec::SmallVec;

use crate::{RawEvent, ledger::LedgerNode};

pub trait EventMap<K, V> {
    type Item;
    type Values<'a>: Iterator<Item = &'a V>
    where
        Self: 'a,
        K: 'a,
        V: 'a;

    fn new() -> Self;
    fn get(&self, querry: &K) -> Option<&V>;
    fn values(&self) -> Self::Values<'_>;

    /// Don't override existent entry,
    /// but return its values instead of the one wanted to insert
    fn insert(&mut self, k: K, v: V) -> V;

    fn insert_with<F>(&mut self, k: K, f: F) -> V
    where
        F: FnOnce() -> V;

    fn remove(&mut self, querry: &K) -> Option<V>;
    fn clear(&mut self);
    fn is_empty(&self) -> bool;
}

#[derive(Debug)]
pub struct SmallMap<'a, K, const N: usize>
where
    K: RawEvent,
{
    // Sorted array of (K, V) pairs.
    // This is a simple implementation of a map that allows for efficient lookups and insertions
    // when the size of the map is quite small
    #[allow(clippy::type_complexity)]
    items: SmallVec<[(K, Arc<LedgerNode<'a, K, SmallMap<'a, K, N>>>); N]>,
}

impl<'a, K: RawEvent, const N: usize> EventMap<K, Arc<LedgerNode<'a, K, SmallMap<'a, K, N>>>>
    for SmallMap<'a, K, N>
{
    type Item = Arc<LedgerNode<'a, K, SmallMap<'a, K, N>>>;
    type Values<'e>
        = Map<slice::Iter<'e, (K, Self::Item)>, fn(&(K, Self::Item)) -> &Self::Item>
    where
        K: 'e,
        Self::Item: 'e;

    fn new() -> Self {
        Self {
            items: SmallVec::new(),
        }
    }

    fn get(&self, query: &K) -> Option<&Self::Item> {
        self.items
            .binary_search_by(|(k, _)| k.cmp(query))
            .ok()
            .map(|idx| &self.items[idx].1)
    }

    fn values(&self) -> Self::Values<'_> {
        self.items.iter().map(|(_k, v)| v)
    }

    fn insert(&mut self, k: K, v: Self::Item) -> Self::Item {
        self.insert_with(k, || v)
    }

    fn insert_with<F>(&mut self, k: K, f: F) -> Self::Item
    where
        F: FnOnce() -> Self::Item,
    {
        match self.items.binary_search_by(|(key, _)| key.cmp(&k)) {
            Ok(idx) => {
                self.items[idx].1.clone()
            }
            Err(idx) => {
                let v = f();
                self.items.insert(idx, (k, v.clone())); // keep sorted
                v
            }
        }
    }

    fn remove(&mut self, query: &K) -> Option<Self::Item> {
        self.items
            .binary_search_by(|(k, _)| k.cmp(query))
            .ok()
            .map(|idx| self.items.remove(idx).1)
    }

    fn clear(&mut self) {
        self.items.clear();
    }

    fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

pub struct EventHashMap<'a, K: RawEvent> {
    items: std::collections::HashMap<K, Arc<LedgerNode<'a, K, EventHashMap<'a, K>>>>,
}

impl<'a, K: RawEvent> EventMap<K, Arc<LedgerNode<'a, K, EventHashMap<'a, K>>>> for EventHashMap<'a, K> {
    type Item = Arc<LedgerNode<'a, K, EventHashMap<'a, K>>>;
    type Values<'e>
        = std::collections::hash_map::Values<'e, K, Self::Item>
    where
        K: 'e;

    // TODO: Investigate if `with_capacity` improves performance for HashMap use vs SmallMap
    fn new() -> Self {
        Self {
            items: std::collections::HashMap::new(),
        }
    }
    fn get(&self, query: &K) -> Option<&Self::Item> {
        self.items.get(query)
    }
    fn values(&self) -> Self::Values<'_> {
        self.items.values()
    }
    fn insert_with<F>(&mut self, k: K, f: F) -> Self::Item
    where
        F: FnOnce() -> Self::Item,
    {
        self.items.entry(k).or_insert_with(f).clone()
    }
    fn insert(&mut self, k: K, v: Self::Item) -> Self::Item {
        self.items.entry(k).or_insert(v).clone()
    }
    fn remove(&mut self, query: &K) -> Option<Self::Item> {
        self.items.remove(query)
    }
    fn clear(&mut self) {
        self.items.clear();
    }
    fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

pub struct EventBTreeMap<'a, K: RawEvent> {
    items: std::collections::BTreeMap<K, Arc<LedgerNode<'a, K, EventBTreeMap<'a, K>>>>,
}

impl<'a, K: RawEvent> EventMap<K, Arc<LedgerNode<'a, K, EventBTreeMap<'a, K>>>> for EventBTreeMap<'a, K> {
    type Item = Arc<LedgerNode<'a, K, EventBTreeMap<'a, K>>>;
    type Values<'e>
        = std::collections::btree_map::Values<'e, K, Self::Item>
    where
        K: 'e,
        Self::Item: 'e;

    fn new() -> Self {
        Self {
            items: std::collections::BTreeMap::new(),
        }
    }
    fn get(&self, query: &K) -> Option<&Self::Item> {
        self.items.get(query)
    }
    fn values(&self) -> Self::Values<'_> {
        self.items.values()
    }
    fn insert_with<F>(&mut self, k: K, f: F) -> Self::Item
    where
        F: FnOnce() -> Self::Item,
    {
        self.items.entry(k).or_insert_with(f).clone()
    }
    fn insert(&mut self, k: K, v: Self::Item) -> Self::Item {
        self.items.entry(k).or_insert(v).clone()
    }
    fn remove(&mut self, query: &K) -> Option<Self::Item> {
        self.items.remove(query)
    }
    fn clear(&mut self) {
        self.items.clear();
    }
    fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}
