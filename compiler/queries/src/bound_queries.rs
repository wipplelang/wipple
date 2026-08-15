use crate::QueryCtx;
use wipple_core::{
    db::Node,
    traces::Traces,
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

pub fn unresolved_bounds<'a>(db: &QueryCtx<'a>, node: Node) -> Vec<(&'a UnresolvedBound, Traces)> {
    let Some(ResolvedBounds(bounds)) = db.get(node) else {
        return Vec::new();
    };

    bounds
        .values()
        .filter_map(|result| result.as_ref().err())
        .map(|bound| {
            (
                bound,
                db.traces_for(
                    node,
                    bound
                        .parameters
                        .values()
                        .flat_map(|ty| ty.referenced_nodes()),
                ),
            )
        })
        .collect()
}
