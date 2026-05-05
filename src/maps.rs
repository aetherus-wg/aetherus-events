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
    fn insert(&mut self, k: K, v: V) -> Option<V>;
    fn remove(&mut self, querry: &K) -> Option<V>;
    fn clear(&mut self);
    fn is_empty(&self) -> bool;
}

#[derive(Debug)]
pub struct SmallMap<K, const N: usize>
where
    K: RawEvent,
{
    // Sorted array of (K, V) pairs.
    // This is a simple implementation of a map that allows for efficient lookups and insertions
    // when the size of the map is quite small
    #[allow(clippy::type_complexity)]
    items: SmallVec<[(K, Arc<LedgerNode<K, SmallMap<K, N>>>); N]>,
}

impl<K: RawEvent, const N: usize> EventMap<K, Arc<LedgerNode<K, SmallMap<K, N>>>>
    for SmallMap<K, N>
{
    type Item = Arc<LedgerNode<K, SmallMap<K, N>>>;
    type Values<'a>
        = Map<slice::Iter<'a, (K, Self::Item)>, fn(&(K, Self::Item)) -> &Self::Item>
    where
        K: 'a,
        Self::Item: 'a;

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

    fn insert(&mut self, k: K, v: Self::Item) -> Option<Self::Item> {
        match self.items.binary_search_by(|(key, _)| key.cmp(&k)) {
            Ok(idx) => {
                let old = std::mem::replace(&mut self.items[idx].1, v); // replace existing
                Some(old)
            }
            Err(idx) => {
                self.items.insert(idx, (k, v)); // keep sorted
                None
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

pub struct EventHashMap<K: RawEvent> {
    items: std::collections::HashMap<K, Arc<LedgerNode<K, EventHashMap<K>>>>,
}

impl<K: RawEvent> EventMap<K, Arc<LedgerNode<K, EventHashMap<K>>>> for EventHashMap<K> {
    type Item = Arc<LedgerNode<K, EventHashMap<K>>>;
    type Values<'a>
        = std::collections::hash_map::Values<'a, K, Self::Item>
    where
        K: 'a;

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
    fn insert(&mut self, k: K, v: Self::Item) -> Option<Self::Item> {
        self.items.insert(k, v)
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

pub struct EventBTreeMap<K: RawEvent> {
    items: std::collections::BTreeMap<K, Arc<LedgerNode<K, EventBTreeMap<K>>>>,
}

impl<K: RawEvent> EventMap<K, Arc<LedgerNode<K, EventBTreeMap<K>>>> for EventBTreeMap<K> {
    type Item = Arc<LedgerNode<K, EventBTreeMap<K>>>;
    type Values<'a>
        = std::collections::btree_map::Values<'a, K, Self::Item>
    where
        K: 'a,
        Self::Item: 'a;

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
    fn insert(&mut self, k: K, v: Self::Item) -> Option<Self::Item> {
        self.items.insert(k, v)
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
