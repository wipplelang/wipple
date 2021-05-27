use crate::*;
use derivative::*;
use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

#[derive(Debug, Clone, Default)]
pub struct Env(Rc<Inner>);

#[derive(Debug, Clone, Default)]
struct Inner {
    values: RefCell<HashMap<Id, (EnvKey, Any)>>,
    parent: Option<Env>,
}

impl Env {
    pub fn new() -> Self {
        Env::default()
    }

    pub fn with(prepare: impl FnOnce(&Env)) -> Env {
        let env = Env::new();
        prepare(&env);
        env
    }

    pub fn try_with<E>(
        prepare: impl FnOnce(&Env) -> std::result::Result<(), E>,
    ) -> std::result::Result<Env, E> {
        let env = Env::new();
        prepare(&env)?;
        Ok(env)
    }

    pub fn child(&self) -> Env {
        Env(Rc::new(Inner {
            parent: Some(self.clone()),
            ..Default::default()
        }))
    }

    pub fn parent(&self) -> Option<&Env> {
        self.0.parent.as_ref()
    }
}

thread_local! {
    static GLOBAL_ENV: Env = Env::new();
}

impl Env {
    pub fn global() -> Env {
        GLOBAL_ENV.with(Clone::clone)
    }

    pub fn clear_global() {
        Env::global().0.values.take();
    }

    pub fn is_global(&self) -> bool {
        std::ptr::eq(&self.0, &Env::global().0)
    }
}

impl Env {
    pub fn get<T: TypeInfo + Default>(&self, key: &EnvKey) -> T {
        self.values()
            .get(&key.id)
            .map(|(_, value)| value.cast::<T>().clone())
            .unwrap_or_default()
    }

    pub fn set<T: TypeInfo + Default>(&self, key: &EnvKey, value: T) {
        let key = key.clone();
        self.values().insert(key.id, (key, value.into()));
    }

    pub fn update<T: TypeInfo + Default, U>(
        &self,
        key: &EnvKey,
        update: impl FnOnce(&mut T) -> U,
    ) -> U {
        let mut value = self.get(key);
        let result = update(&mut value);
        self.set(key, value);
        result
    }
}

impl Env {
    fn values(&self) -> RefMut<HashMap<Id, (EnvKey, Any)>> {
        self.0.values.borrow_mut()
    }
}

#[derive(Debug, Clone, Derivative)]
#[derivative(PartialEq, Eq, Hash)]
pub struct EnvKey {
    id: Id,

    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub visibility: EnvKeyVisibility,
}

impl EnvKey {
    pub fn new(visibility: EnvKeyVisibility) -> Self {
        EnvKey {
            id: Id::new(),
            visibility,
        }
    }
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub enum EnvKeyVisibility {
    /// The value cannot be `use`d by another environment.
    Private,

    /// The value can be `use`d by another environment. If the value already
    /// exists in the other environment, the provided `UseMergeFn` will be
    /// called to handle merging them. If the value doesn't exist, it will just
    /// be copied over.
    Public(UseMergeFn),
}

stored_closure!(pub struct UseMergeFn(&mut Any, &Any, &Env, &Stack) -> Result<()>);

impl UseMergeFn {
    pub fn merging<T: TypeInfo>(
        merge: impl Fn(&mut T, &T, &Env, &Stack) -> Result<()> + 'static,
    ) -> Self {
        UseMergeFn::new(move |current, new, env, stack| {
            merge(current.cast_mut(), new.cast(), env, stack)
        })
    }
}

impl Env {
    pub fn r#use(&self, other: &Env, stack: &Stack) -> Result<()> {
        let mut values = self.values();

        for (id, (key, new_value)) in other.values().iter() {
            match &key.visibility {
                EnvKeyVisibility::Public(merge) => match values.get_mut(id) {
                    Some((_, current_value)) => {
                        merge(current_value, new_value, self, stack)?;
                    }
                    None => {
                        values.insert(*id, (key.clone(), new_value.clone()));
                    }
                },
                EnvKeyVisibility::Private => {}
            }
        }

        Ok(())
    }
}
