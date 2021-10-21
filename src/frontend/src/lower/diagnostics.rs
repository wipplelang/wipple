use crate::lower::*;
use std::ops::{Deref, DerefMut};

pub use wipple_diagnostics::*;

pub struct Diagnostics<'a> {
    inner: &'a mut wipple_diagnostics::Diagnostics,
    pub declared_variables: Vec<VariableId>,
    pub used_variables: Vec<VariableId>,
}

impl<'a> Diagnostics<'a> {
    pub fn new(inner: &'a mut wipple_diagnostics::Diagnostics) -> Self {
        Diagnostics {
            inner,
            declared_variables: Default::default(),
            used_variables: Default::default(),
        }
    }
}

impl<'a> Deref for Diagnostics<'a> {
    type Target = &'a mut wipple_diagnostics::Diagnostics;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a> DerefMut for Diagnostics<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
