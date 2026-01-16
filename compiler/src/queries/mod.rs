mod bounds;
mod comments;
mod definitions;
mod filter;
mod highlight;
mod placeholders;
mod types;

pub use bounds::*;
pub use comments::*;
pub use definitions::*;
pub use filter::*;
pub use highlight::*;
pub use placeholders::*;
pub use types::*;

use crate::database::{Db, Fact, NodeRef};

pub struct QueryCtx<'a> {
    pub db: &'a Db,
    pub node: NodeRef,
}

pub fn fact<T: Fact>(ctx: &QueryCtx<'_>, f: &mut dyn FnMut((NodeRef, T))) {
    if let Some(value) = ctx.db.get::<T>(&ctx.node) {
        f((ctx.node.clone(), value));
    }
}

pub fn find<T>(
    db: &Db,
    mut query: impl FnMut(&QueryCtx<'_>, &mut dyn FnMut(T)),
    filter: impl FnMut(&T) -> bool,
) -> Option<T> {
    db.iter_nodes()
        .filter_map(|node| find_for(db, &node, &mut query))
        .find(filter)
}

pub fn find_for<T>(
    db: &Db,
    node: &NodeRef,
    mut f: impl FnMut(&QueryCtx<'_>, &mut dyn FnMut(T)),
) -> Option<T> {
    let ctx = QueryCtx {
        db,
        node: node.clone(),
    };

    let mut result = None;
    f(&ctx, &mut |data| {
        result.get_or_insert(data);
    });

    result
}
