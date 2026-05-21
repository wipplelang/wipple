use crate::{db::Db, visit::Visit};
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    sync::{Arc, Weak},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct AstKey {
    layer: usize,
    index: usize,
    #[serde(skip)]
    ptr: Option<Arc<()>>,
}

impl AstKey {
    #[allow(clippy::borrowed_box, reason = "allow cloning")]
    pub fn get<'a>(&self, db: &'a Db) -> &'a Box<dyn Visit> {
        db.ast(self)
    }
}

#[derive(Debug, Clone)]
pub struct Ast {
    layer: usize,
    next_index: usize,
    map: BTreeMap<usize, (Box<dyn Visit>, Option<Weak<()>>)>,
}

impl Ast {
    pub fn new(layer: usize) -> Self {
        Self {
            layer,
            next_index: 0,
            map: Default::default(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct SerializedAst {
    layer: usize,
    next_id: usize,
    map: Vec<(usize, Box<dyn Visit>)>,
}

impl Serialize for Ast {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedAst {
            layer: self.layer,
            next_id: self.next_index,
            map: self
                .map
                .iter()
                .map(|(index, (visit, _))| (*index, visit.clone()))
                .collect(),
        }
        .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Ast {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let serialized = SerializedAst::deserialize(deserializer)?;

        Ok(Ast {
            layer: serialized.layer,
            next_index: serialized.next_id,
            map: serialized
                .map
                .into_iter()
                .map(|(index, visit)| (index, (visit, None)))
                .collect(),
        })
    }
}

impl Ast {
    #[allow(clippy::borrowed_box, reason = "allow cloning")]
    pub fn get(&self, key: &AstKey) -> Option<&Box<dyn Visit>> {
        if key.layer != self.layer {
            return None;
        }

        self.map.get(&key.index).map(|(visit, _)| visit)
    }

    pub fn insert(&mut self, visit: Box<dyn Visit>) -> AstKey {
        let ptr = Arc::new(());

        let index = self.next_index;
        self.next_index += 1;

        self.map.insert(index, (visit, Some(Arc::downgrade(&ptr))));

        AstKey {
            layer: self.layer,
            index,
            ptr: Some(ptr),
        }
    }

    pub fn gc(&mut self) {
        self.map
            .retain(|_, (_, ptr)| ptr.as_ref().is_none_or(|ptr| ptr.upgrade().is_some()));
    }
}
