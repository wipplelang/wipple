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
    pub struct UseFn(&Dynamic, &Dynamic) -> Dynamic;
}

impl UseFn {
    pub fn from<T: TypeInfo>(r#use: impl Fn(&T, &T) -> T + 'static) -> Self {
        UseFn::new(move |parent, new| {
            let parent = parent.cast::<T>();
            let new = new.cast::<T>();

            Dynamic::new(r#use(parent, new))
        })
    }
}

impl EnvironmentInner {
    pub fn r#use(&mut self, other: &EnvironmentInner) {
        for (key, new_value) in &other.values {
            match &key.visibility {
                EnvironmentVisibility::Public(r#use) => match self.get(&key) {
                    Some(parent_value) => {
                        let used_value = r#use(parent_value, new_value);
                        *parent_value = used_value;
                    }
                    None => self.set(&key, new_value.clone()),
                },
                EnvironmentVisibility::Private => {}
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
pub enum EnvironmentVisibility {
    /// The value cannot be accessed by children and cannot be `use`d by another
    /// environment.
    Private,

    /// The value can be accessed by children but cannot be `use`d by another
    /// environment. (TODO: Implement)
    // Protected,

    /// The value can be accessed by children and can be `use`d by another
    /// environment. If the value already exists in the other environment, the
    /// provided `UseFn` will be called to handle merging them. If the value
    /// doesn't exist, it will be copied.
    Public(UseFn),
}

#[derive(Clone)]
pub struct EnvironmentKey {
    pub id: Id,
    pub visibility: EnvironmentVisibility,
}

impl EnvironmentKey {
    pub fn of<T: TypeInfo>(visibility: EnvironmentVisibility) -> Self {
        EnvironmentKey {
            id: Id::of::<T>(),
            visibility,
        }
    }

    pub fn new(visibility: EnvironmentVisibility) -> Self {
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
