use crate::{
    database::{Db, Fact, NodeRef, Render},
    typecheck::Type,
};
use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

#[derive(Debug, Clone)]
pub struct InferredParameter;

impl Fact for InferredParameter {}

impl Render for InferredParameter {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is inferred type parameter")
    }
}

pub type Replacements = Map<NodeRef>;
pub type Substitutions = Map<Type>;

#[derive(Debug, Clone)]
pub struct Map<T>(Arc<RwLock<BTreeMap<NodeRef, T>>>);

impl<T> Default for Map<T> {
    fn default() -> Self {
        Map(Default::default())
    }
}

impl<T> Map<T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&self, node: NodeRef, value: T) {
        self.0.write().unwrap().insert(node, value);
    }

    pub fn contains(&self, node: &NodeRef) -> bool {
        self.0.read().unwrap().contains_key(node)
    }

    pub fn get(&self, node: &NodeRef) -> Option<T>
    where
        T: Clone,
    {
        self.0.read().unwrap().get(node).cloned()
    }

    pub fn with(&self, node: &NodeRef, f: impl FnOnce(Option<T>) -> T) -> T
    where
        T: Clone,
    {
        let mut lock = self.0.write().unwrap();
        let value = f(lock.remove(node));
        lock.insert(node.clone(), value.clone());
        value
    }

    pub fn get_or_insert_with(&self, node: &NodeRef, value: impl FnOnce() -> T) -> T
    where
        T: Clone,
    {
        self.0
            .write()
            .unwrap()
            .entry(node.clone())
            .or_insert_with(value)
            .clone()
    }

    pub fn entries(&self) -> impl Iterator<Item = (NodeRef, T)>
    where
        T: Clone,
    {
        self.0
            .read()
            .unwrap()
            .iter()
            .map(|(key, value)| (key.clone(), value.clone()))
            .collect::<Vec<_>>()
            .into_iter()
    }

    pub fn keys(&self) -> Vec<NodeRef> {
        self.0.read().unwrap().keys().cloned().collect()
    }

    pub fn values(&self) -> Vec<T>
    where
        T: Clone,
    {
        self.0.read().unwrap().values().cloned().collect()
    }

    pub fn modify(&self, mut f: impl FnMut(&mut T))
    where
        T: Clone,
    {
        for (_, value) in self.0.write().unwrap().iter_mut() {
            f(value);
        }
    }

    pub fn extend(&self, iter: impl IntoIterator<Item = (NodeRef, T)>) {
        for (node, value) in iter {
            self.insert(node, value);
        }
    }
}

impl<T> FromIterator<(NodeRef, T)> for Map<T> {
    fn from_iter<I: IntoIterator<Item = (NodeRef, T)>>(iter: I) -> Self {
        Map(Arc::new(RwLock::new(BTreeMap::from_iter(iter))))
    }
}

impl<T> Extend<(NodeRef, T)> for Map<T> {
    fn extend<I: IntoIterator<Item = (NodeRef, T)>>(&mut self, iter: I) {
        (*self).extend(iter);
    }
}
