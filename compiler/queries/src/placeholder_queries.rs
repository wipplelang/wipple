use crate::QueryCtx;
use wipple_core::{
    db::Node,
    typecheck::{groups::Typed, ty::ConstructedTy},
};
use wipple_syntax::expressions::placeholder_expression::IsPlaceholder;

pub fn placeholder<'a>(
    db: &QueryCtx<'a>,
    node: Node,
) -> Option<(Node, Vec<Node>, Option<&'a ConstructedTy>)> {
    if !db.contains::<IsPlaceholder>(node) {
        return None;
    }

    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    Some((
        node,
        group
            .nodes
            .iter()
            .copied()
            .filter(|other| *other != node)
            .collect(),
        group.tys.first(),
    ))
}
