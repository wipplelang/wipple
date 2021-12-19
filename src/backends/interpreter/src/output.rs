#![allow(clippy::type_complexity)]

use lazy_static::lazy_static;
use std::sync::{Arc, RwLock};

lazy_static! {
    pub static ref OUTPUT: RwLock<Option<Arc<dyn Fn(&str) + Send + Sync>>> = Default::default();
}

pub fn set_output(output: impl Fn(&str) + Send + Sync + 'static) {
    *OUTPUT.write().unwrap() = Some(Arc::new(output));
}
