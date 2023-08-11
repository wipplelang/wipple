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
}

impl<T: ?Sized> Shared<T> {
    pub fn lock(&self) -> MutexGuard<T> {
        self.0.lock()
    }
}

impl<T: ?Sized> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(self.0.clone())
    }
}
