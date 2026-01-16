use crate::{
    database::{Db, NodeRef},
    nodes::{IsExpression, IsPattern, IsType},
};

// Prefer showing patterns first, then expressions, then types
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum GroupOrder {
    Pattern,
    Expression,
    Type,
    Other,
}

impl GroupOrder {
    pub fn new(db: &mut Db, node: &NodeRef) -> Self {
        if db.contains::<IsPattern>(node) {
            GroupOrder::Pattern
        } else if db.contains::<IsExpression>(node) {
            GroupOrder::Expression
        } else if db.contains::<IsType>(node) {
            GroupOrder::Type
        } else {
            GroupOrder::Other
        }
    }
}
