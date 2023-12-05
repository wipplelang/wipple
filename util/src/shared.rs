use parking_lot::{Mutex, MutexGuard};
use std::sync::Arc;

#[derive(Debug, Default)]
pub struct Shared<T: ?Sized>(Arc<Mutex<T>>);

impl<T> Shared<T> {
    pub fn new(value: T) -> Self {
        Shared(Arc::new(Mutex::new(value)))
    }

    pub fn into_unique(self) -> T {
        Arc::into_inner(self.0)
            .expect("called `into_unique` on a `Shared` value with more than one reference")
            .into_inner()
    }

    pub fn try_into_unique(self) -> T
    where
        T: Clone,
    {
        Arc::try_unwrap(self.0)
            .map(|value| value.into_inner())
            .unwrap_or_else(|value| value.lock().clone())
    }
}

impl<T: ?Sized> Shared<T> {
    pub fn lock(&self) -> MutexGuard<T> {
        self.0.lock()
    }

    pub fn id_ptr(&self) -> *const () {
        Arc::as_ptr(&self.0) as *const ()
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl<T: ?Sized> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(self.0.clone())
    }
}

impl<T: ?Sized + PartialEq> PartialEq for Shared<T> {
    fn eq(&self, other: &Self) -> bool {
        *self.0.lock() == *other.0.lock()
    }
}

impl<T: ?Sized + Eq> Eq for Shared<T> {}

impl<T: ?Sized + std::hash::Hash> std::hash::Hash for Shared<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.lock().hash(state);
    }
}
