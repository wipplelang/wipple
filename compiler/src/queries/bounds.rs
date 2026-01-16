use crate::{
    database::NodeRef,
    queries::QueryCtx,
    typecheck::{Bound, Bounds},
};

pub fn resolved_bound(ctx: &QueryCtx<'_>, f: &mut dyn FnMut((Bound, NodeRef))) {
    let Some(Bounds(bounds)) = ctx.db.get(&ctx.node) else {
        return;
    };

    for item in bounds {
        if let Some(instance) = item.instance {
            f((item.bound, instance.node));
        }
    }
}

pub fn unresolved_bound(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(Bound)) {
    let Some(Bounds(bounds)) = ctx.db.get(&ctx.node) else {
        return;
    };

    for item in bounds {
        if item.instance.is_none() {
            f(item.bound);
        }
    }
}
