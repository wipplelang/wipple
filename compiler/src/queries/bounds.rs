use crate::{
    queries::QueryCtx,
    typecheck::{Bound, Bounds, BoundsItemInstance},
};

pub fn resolved_bound(ctx: &QueryCtx<'_>, f: &mut dyn FnMut((Bound, BoundsItemInstance))) {
    let Some(Bounds(bounds)) = ctx.db.get(&ctx.node) else {
        return;
    };

    for (_, item) in bounds {
        if let Some(instance) = item.instance {
            f((item.bound, instance));
        }
    }
}

pub fn unresolved_bound(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(Bound)) {
    let Some(Bounds(bounds)) = ctx.db.get(&ctx.node) else {
        return;
    };

    for (_, item) in bounds {
        if item.instance.is_none() {
            f(item.bound);
        }
    }
}
