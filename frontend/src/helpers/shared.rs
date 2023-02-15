use parking_lot::{Mutex, MutexGuard};
use std::sync::Arc;

#[derive(Debug, Default)]
pub struct Shared<T: ?Sized>(Arc<Mutex<T>>);

impl<T> Shared<T> {
    pub fn new(value: T) -> Self {
        Shared(Arc::new(Mutex::new(value)))
    }

    pub fn into_unique(self) -> T {
        Arc::try_unwrap(self.0)
            .unwrap_or_else(|_| {
                panic!("called `unique` on a `Shared` with more than one reference")
            })
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
