use crate::QueryCtx;
use wipple_core::{
    db::Node,
    visit::definitions::{ConstantDefinition, ConstantValue, Defined},
};

pub fn missing_constant_value(db: &QueryCtx<'_>, node: Node) -> bool {
    db.get(node)
        .and_then(|Defined(definition)| definition.downcast_ref::<ConstantDefinition>())
        .is_some()
        && !db.contains::<ConstantValue>(node)
}
