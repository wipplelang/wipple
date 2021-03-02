use crate::*;
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};
use uuid::Uuid;

pub type EnvironmentValues = HashMap<EnvironmentKey, Dynamic>;

#[derive(Clone, Default)]
pub struct Environment {
    pub values: EnvironmentValues,
    pub parent: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment::default()
    }

    pub fn get(&self, key: &EnvironmentKey) -> Option<&Dynamic> {
        self.values.get(key)
    }

    pub fn get_mut(&mut self, key: &EnvironmentKey) -> Option<&mut Dynamic> {
        self.values.get_mut(key)
    }

    pub fn get_or_insert(&mut self, key: &EnvironmentKey, default: Dynamic) -> &mut Dynamic {
        self.values.entry(key.clone()).or_insert(default)
    }

    pub fn set(&mut self, key: &EnvironmentKey, value: Dynamic) {
        self.values.insert(key.clone(), value);
    }
}

#[derive(Clone)]
pub struct UseFn(Rc<dyn Fn(&Dynamic, &Dynamic) -> Dynamic>);

impl UseFn {
    pub fn new<T: Clone + 'static>(r#use: impl Fn(&T, &T) -> T + 'static) -> Self {
        UseFn(Rc::new(move |parent, new| {
            let parent = parent.downcast_ref::<T>().unwrap();
            let new = new.downcast_ref::<T>().unwrap();

            Dynamic::new(r#use(parent, new))
        }))
    }

    pub fn take_parent<T: Clone + 'static>() -> Self {
        UseFn::new(|parent: &T, _| parent.clone())
    }

    pub fn take_new<T: Clone + 'static>() -> Self {
        UseFn::new(|_, new: &T| new.clone())
    }
}

impl Environment {
    pub fn r#use(&mut self, new: &EnvironmentValues) {
        for (key, new_value) in new {
            match self.get_mut(&key) {
                Some(parent_value) => {
                    let used_value = key.r#use.0(parent_value, &new_value);
                    *parent_value = used_value;
                }
                None => {
                    if key.insert {
                        self.set(&key, new_value.clone());
                    }
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct EnvironmentKey {
    pub id: Uuid,
    pub r#use: UseFn,
    /// What to do when the key is not in the parent environment
    pub insert: bool,
}

impl EnvironmentKey {
    pub fn new(r#use: UseFn, insert: bool) -> Self {
        EnvironmentKey {
            id: Uuid::new_v4(),
            r#use,
            insert,
        }
    }
}

impl PartialEq for EnvironmentKey {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for EnvironmentKey {}

impl Hash for EnvironmentKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
