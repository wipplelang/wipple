use crate::QueryCtx;
use wipple_core::{
    db::Node,
    traces::Traces,
    typecheck::bounds::{Bounds, ResolvedBound, UnresolvedBound},
};

pub fn resolved_bounds<'a>(db: &QueryCtx<'a>, node: Node) -> Vec<&'a ResolvedBound> {
    let Some(Bounds(bounds)) = db.get(node) else {
        return Vec::new();
    };

    bounds
        .iter()
        .filter_map(|(_, result)| result.as_ref().ok())
        .collect()
}

pub fn unresolved_bounds<'a>(db: &QueryCtx<'a>, node: Node) -> Vec<(&'a UnresolvedBound, Traces)> {
    let Some(Bounds(bounds)) = db.get(node) else {
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
