use crate::Driver;
use derivative::Derivative;
use std::collections::{HashMap, HashSet};
use wipple_util::WithInfo;

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Scope<D: Driver> {
    pub kind: ScopeKind,
    pub paths: HashMap<String, Vec<WithInfo<<D as Driver>::Info, crate::Path>>>,
}

impl<D: Driver> Scope<D> {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            paths: HashMap::new(),
        }
    }

    pub fn into_paths(self) -> HashMap<String, Vec<WithInfo<D::Info, crate::Path>>> {
        self.paths
    }

    pub fn filters_locals(&self) -> bool {
        matches!(self.kind, ScopeKind::Constant)
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Default(bound = ""))]
pub struct Scopes<D: Driver>(pub Vec<Scope<D>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Block,
    Constant,
}

impl<D: Driver> Scopes<D> {
    pub fn push_block_scope(&mut self) {
        self.0.push(Scope::new(ScopeKind::Block));
    }

    pub fn push_constant_scope(&mut self) {
        self.0.push(Scope::new(ScopeKind::Constant));
    }

    pub fn pop_scope(&mut self) -> Scope<D> {
        self.0.pop().unwrap()
    }

    pub fn define(&mut self, name: String, path: WithInfo<D::Info, crate::Path>) {
        let paths = &mut self.0.last_mut().unwrap().paths;
        let paths = paths.entry(name).or_default();

        // Overwrite existing locals with the same name
        if path.item.last().unwrap().is_local() {
            paths.retain(|path| !path.item.last().unwrap().is_local());
        }

        paths.push(path);
    }

    pub fn merge(&mut self, scope: &Scope<D>) {
        for (name, paths) in &scope.paths {
            for path in paths {
                if !path.item.last().unwrap().is_local() {
                    self.define(name.clone(), path.clone());
                }
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Captures {
    pub declared: HashSet<crate::Path>,
    pub used: HashSet<crate::Path>,
}
