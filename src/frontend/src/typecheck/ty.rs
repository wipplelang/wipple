use kind::kind;
use serde::Serialize;
use std::{cell::RefCell, fmt, sync::Arc};
use wipple_parser::LocalIntern;

#[non_exhaustive]
#[derive(Debug, Clone, Serialize)]
pub struct Ty {
    pub kind: Arc<RefCell<TyKind>>,
}

#[kind(Ty::new())]
#[derive(Debug, Clone, Serialize)]
pub enum TyKind {
    Unknown, // TODO: Bounds
    Generic, // TODO: Generic ID and bounds
    Unit,    // eventually, more general tuple type
    Number,
    Text,
    Function { input: Ty, body: Ty },
    File { path: LocalIntern<String> },
}

impl Ty {
    pub fn new(kind: TyKind) -> Self {
        Ty {
            kind: Arc::new(RefCell::new(kind)),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.kind.borrow() {
            TyKind::Unknown => write!(f, "_"),
            TyKind::Generic => write!(f, "_"), // TODO
            TyKind::Unit => write!(f, "."),
            TyKind::Number => write!(f, "Number"),
            TyKind::Text => write!(f, "Text"),
            TyKind::Function { input, body } => write!(f, "{} -> {}", input, body),
            TyKind::File { path, .. } => write!(f, "File \"{}\"", path),
        }
    }
}
