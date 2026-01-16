use crate::{
    database::NodeRef,
    nodes::IsPlaceholder,
    queries::QueryCtx,
    typecheck::{ConstructedType, Typed},
};

pub fn placeholder(ctx: &QueryCtx<'_>, f: &mut dyn FnMut((NodeRef, Option<ConstructedType>))) {
    if !ctx.db.contains::<IsPlaceholder>(&ctx.node) {
        return;
    }

    let ty = ctx
        .db
        .get::<Typed>(&ctx.node)
        .and_then(|Typed { group }| group)
        .and_then(|group| group.types.first().cloned());

    f((ctx.node.clone(), ty));
}
