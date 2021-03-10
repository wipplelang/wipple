use crate::*;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    hash::{Hash, Hasher},
    rc::Rc,
};
use uuid::Uuid;

pub type EnvironmentValues = HashMap<EnvironmentKey, Dynamic>;
pub type EnvironmentRef = Rc<RefCell<Environment>>;

#[derive(Clone)]
pub struct Environment {
    pub values: EnvironmentValues,
    pub parent: Option<EnvironmentRef>,
}

impl Environment {
    pub fn blank() -> Self {
        Environment {
            values: EnvironmentValues::new(),
            parent: None,
        }
    }

    pub fn child_of(parent: &EnvironmentRef) -> Environment {
        Environment {
            values: EnvironmentValues::new(),
            parent: Some(parent.clone()),
        }
    }
}

impl Environment {
    pub fn get(&mut self, key: &EnvironmentKey) -> Option<&mut Dynamic> {
        self.values.get_mut(key)
    }

    pub fn get_or_insert(&mut self, key: &EnvironmentKey, default: Dynamic) -> &mut Dynamic {
        self.values.entry(key.clone()).or_insert(default)
    }

    pub fn set(&mut self, key: &EnvironmentKey, value: Dynamic) {
        self.values.insert(key.clone(), value);
    }

    pub fn into_ref(self) -> EnvironmentRef {
        Rc::new(RefCell::new(self))
    }
}

#[derive(Clone)]
pub struct UseFn(pub Rc<dyn Fn(&Dynamic, &Dynamic) -> Dynamic>);

impl UseFn {
    pub fn new<T: Clone + 'static>(r#use: impl Fn(&T, &T) -> T + 'static) -> Self {
        UseFn(Rc::new(move |parent, new| {
            let parent = parent.cast::<T>();
            let new = new.cast::<T>();

            Dynamic::new(r#use(parent, new))
        }))
    }

    pub fn take_parent() -> Self {
        UseFn(Rc::new(move |parent, _| parent.clone()))
    }

    pub fn take_new() -> Self {
        UseFn(Rc::new(move |_, new| new.clone()))
    }
}

impl Environment {
    pub fn r#use(&mut self, new: &Environment) {
        for (key, new_value) in &new.values {
            match self.get(&key) {
                Some(parent_value) => {
                    let used_value = key.r#use.0(parent_value, new_value);
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

thread_local! {
    static GLOBAL_ENV: EnvironmentRef = Environment::blank().into_ref();
}

impl Environment {
    pub fn global() -> EnvironmentRef {
        GLOBAL_ENV.with(|env| env.clone())
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

impl Debug for EnvironmentKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(EnvironmentKey {})", self.id)
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
