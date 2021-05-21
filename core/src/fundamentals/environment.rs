use crate::*;
use std::{
    collections::HashMap,
    fmt::Debug,
    hash::{Hash, Hasher},
    rc::Rc,
};

#[cfg(feature = "debug_cell")]
use debug_cell::RefCell;

#[cfg(not(feature = "debug_cell"))]
use std::cell::RefCell;

pub type EnvironmentValues = HashMap<EnvironmentKey, Dynamic>;
pub type Environment = Rc<RefCell<EnvironmentInner>>;

#[derive(Clone)]
pub struct EnvironmentInner {
    pub values: EnvironmentValues,
    pub parent: Option<Environment>,
}

impl EnvironmentInner {
    pub fn get(&mut self, key: &EnvironmentKey) -> Option<&mut Dynamic> {
        self.values.get_mut(key)
    }

    pub fn get_or_insert(
        &mut self,
        key: &EnvironmentKey,
        default: impl FnOnce() -> Dynamic,
    ) -> &mut Dynamic {
        self.values.entry(key.clone()).or_insert_with(default)
    }

    pub fn set(&mut self, key: &EnvironmentKey, value: Dynamic) {
        self.values.insert(key.clone(), value);
    }
}

impl From<EnvironmentInner> for Environment {
    fn from(env: EnvironmentInner) -> Self {
        Rc::new(RefCell::new(env))
    }
}

impl From<EnvironmentInner> for Rc<RefCell<Option<EnvironmentInner>>> {
    fn from(env: EnvironmentInner) -> Self {
        Rc::new(RefCell::new(Some(env)))
    }
}

fn_wrapper! {
    pub struct UseMergeFn(&mut Dynamic, Dynamic);
}

impl UseMergeFn {
    pub fn of<T: TypeInfo>(r#use: impl Fn(&mut T, T) + 'static) -> Self {
        UseMergeFn::new(move |current, new| r#use(current.cast_mut(), new.into_cast()))
    }
}

impl EnvironmentInner {
    pub fn r#use(&mut self, other: &Self) {
        for (key, new_value) in &other.values {
            match &key.visibility {
                EnvironmentKeyVisibility::Public(merge) => match self.get(&key) {
                    Some(current_value) => merge(current_value, new_value.clone()),
                    None => self.set(&key, new_value.clone()),
                },
                EnvironmentKeyVisibility::Private => {}
            }
        }
    }
}

/// Helper functions for managing environment
pub mod env {
    use super::*;

    thread_local! {
        static GLOBAL_ENV: Environment = Environment::from(blank());
    }

    pub fn blank() -> EnvironmentInner {
        EnvironmentInner {
            values: EnvironmentValues::new(),
            parent: None,
        }
    }

    pub fn child_of(parent: &Environment) -> EnvironmentInner {
        EnvironmentInner {
            values: EnvironmentValues::new(),
            parent: Some(parent.clone()),
        }
    }

    pub fn global() -> Environment {
        GLOBAL_ENV.with(|env| env.clone())
    }

    pub fn is_global(env: &Environment) -> bool {
        Environment::as_ptr(env) == Environment::as_ptr(&global())
    }
}

#[derive(Clone)]
pub enum EnvironmentKeyVisibility {
    /// The value cannot be `use`d by another environment.
    Private,

    /// The value can be `use`d by another environment. If the value already
    /// exists in the other environment, the provided `UseFn` will be called to
    /// handle merging them. If the value doesn't exist, it will be copied.
    Public(UseMergeFn),
}

#[derive(Clone)]
pub struct EnvironmentKey {
    pub id: Id,
    pub visibility: EnvironmentKeyVisibility,
}

impl EnvironmentKey {
    pub fn of<T: TypeInfo>(visibility: EnvironmentKeyVisibility) -> Self {
        EnvironmentKey {
            id: Id::of::<T>(),
            visibility,
        }
    }

    pub fn new(visibility: EnvironmentKeyVisibility) -> Self {
        EnvironmentKey {
            id: Id::new(),
            visibility,
        }
    }
}

impl Debug for EnvironmentKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "EnvironmentKey({:?})", self.id)
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
