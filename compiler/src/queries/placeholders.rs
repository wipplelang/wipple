use crate::{
    database::NodeRef,
    nodes::IsPlaceholder,
    queries::QueryCtx,
    typecheck::{ConstructedType, Typed},
};

pub fn placeholder(
    ctx: &QueryCtx<'_>,
    f: &mut dyn FnMut((NodeRef, Vec<NodeRef>, Option<ConstructedType>)),
) {
    if !ctx.db.contains::<IsPlaceholder>(&ctx.node) {
        return;
    }

    let Some(group) = ctx
        .db
        .get::<Typed>(&ctx.node)
        .and_then(|Typed { group }| group)
    else {
        return;
    };

    let nodes = group
        .nodes
        .iter()
        .filter(|&node| *node != ctx.node)
        .cloned()
        .collect::<Vec<_>>();

    let ty = group.types.first().cloned();

    f((ctx.node.clone(), nodes, ty));
}
