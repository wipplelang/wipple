use wipple_core::{
    db::{Db, Node},
    visit::definitions::{ConstantDefinition, ConstantValue, Defined},
};

pub fn missing_constant_value(db: &Db, node: Node) -> bool {
    db.get(node)
        .and_then(|Defined(definition)| definition.downcast_ref::<ConstantDefinition>())
        .is_some()
        && !db.contains::<ConstantValue>(node)
}
