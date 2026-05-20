use wipple_core::{
    db::{Db, Node},
    typecheck::bounds::{Bounds, ResolvedBound, UnresolvedBound},
};

pub fn resolved_bounds(db: &Db, node: Node) -> Vec<&ResolvedBound> {
    let Some(Bounds(bounds)) = db.get(node) else {
        return Vec::new();
    };

    bounds
        .iter()
        .filter_map(|(_, result)| result.as_ref().ok())
        .collect()
}

pub fn unresolved_bounds(db: &Db, node: Node) -> Vec<&UnresolvedBound> {
    let Some(Bounds(bounds)) = db.get(node) else {
        return Vec::new();
    };

    bounds
        .iter()
        .filter_map(|(_, result)| result.as_ref().err())
        .collect()
}
