use crate::QueryCtx;
use wipple_core::{db::Node, span::Str, typecheck::resolved::ResolvedField};

pub fn unknown_field(db: &QueryCtx<'_>, node: Node) -> Option<(Str, Node)> {
    db.get(node).and_then(|ResolvedField { index, value }| {
        index.as_ref().err().cloned().map(|field| (field, *value))
    })
}
