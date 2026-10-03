use crate::{QueryCtx, Trace};
use wipple_core::{
    db::Node,
    typecheck::bounds::{ResolvedBound, ResolvedBounds, UnresolvedBound},
};

pub fn resolved_bounds<'a>(db: &QueryCtx<'a>, node: Node) -> Vec<&'a ResolvedBound> {
    let Some(ResolvedBounds(bounds)) = db.get(node) else {
        return Vec::new();
    };

    bounds
        .values()
        .filter_map(|result| result.as_ref().ok())
        .collect()
}

pub fn unresolved_bounds<'a>(
    db: &QueryCtx<'a>,
    node: Node,
) -> Vec<(&'a UnresolvedBound, Trace<'a>)> {
    let Some(ResolvedBounds(bounds)) = db.get(node) else {
        return Vec::new();
    };

    bounds
        .values()
        .filter_map(|result| result.as_ref().err())
        .map(|unresolved| (unresolved, Trace::collect(db, node)))
        .collect()
}
