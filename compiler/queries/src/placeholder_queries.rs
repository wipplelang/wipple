use crate::{QueryCtx, Trace};
use wipple_core::{
    db::Node,
    typecheck::{groups::Typed, ty::ConstructedTy},
};
use wipple_syntax::expressions::placeholder_expression::IsPlaceholder;

pub fn placeholder<'a>(
    db: &QueryCtx<'a>,
    node: Node,
) -> Option<(Vec<Node>, Option<&'a ConstructedTy>, Trace<'a>)> {
    if !db.contains::<IsPlaceholder>(node) {
        return None;
    }

    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    Some((
        group.nodes().filter(|other| *other != node).collect(),
        group.tys().next(),
        Trace::collect(db, node),
    ))
}
