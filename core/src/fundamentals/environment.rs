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

fn_wrapper_struct! {
    pub type UseFn(&Dynamic, &Dynamic) -> Dynamic;
}

impl UseFn {
    pub fn from<T: Clone + 'static>(r#use: impl Fn(&T, &T) -> T + 'static) -> Self {
        UseFn::new(move |parent, new| {
            let parent = parent.cast::<T>();
            let new = new.cast::<T>();

            Dynamic::new(r#use(parent, new))
        })
    }
}

impl Environment {
    pub fn r#use(&mut self, other: &Environment) {
        for (key, new_value) in &other.values {
            let r#use = match &key.visibility {
                EnvironmentVisibility::Public(r#use) => r#use,
                EnvironmentVisibility::Private => continue,
            };

            match self.get(&key) {
                Some(parent_value) => {
                    let used_value = r#use(parent_value, new_value);
                    *parent_value = used_value;
                }
                None => self.set(&key, new_value.clone()),
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

    pub fn is_global(env: &EnvironmentRef) -> bool {
        env.as_ptr() == Environment::global().as_ptr()
    }
}

#[derive(Clone)]
pub enum EnvironmentVisibility {
    /// The value cannot be `use`d by another environment
    Private,

    /// The value can be `use`d by another environment. If the value does not
    /// exist in the other environment, it will just be copied over
    Public(UseFn),
}

#[derive(Clone)]
pub struct EnvironmentKey {
    pub id: ID,
    pub visibility: EnvironmentVisibility,
}

impl EnvironmentKey {
    pub fn of<T: 'static>(visibility: EnvironmentVisibility) -> Self {
        EnvironmentKey {
            id: ID::of::<T>(),
            visibility,
        }
    }

    pub fn new(visibility: EnvironmentVisibility) -> Self {
        EnvironmentKey {
            id: ID::new(),
            visibility,
        }
    }
}

impl Debug for EnvironmentKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(EnvironmentKey {:?})", self.id)
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
