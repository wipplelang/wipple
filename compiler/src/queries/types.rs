use crate::{
    database::NodeRef,
    queries::QueryCtx,
    typecheck::{Constraint, ConstructedType, Group, Instantiated, Type, Typed},
};

pub fn has_type(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(ConstructedType)) {
    let Some(Typed { group: Some(group) }) = ctx.db.get(&ctx.node) else {
        return;
    };

    if let Some(ty) = group.types.first() {
        f(ty.clone());
    }
}

pub fn related(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(NodeRef)) {
    let Some(Typed { group: Some(group) }) = ctx.db.get(&ctx.node) else {
        return;
    };

    for related in &group.nodes {
        if *related != ctx.node {
            f(related.clone());
        }
    }
}

#[derive(Debug)]
pub struct QueriedConflictingTypes {
    pub source: Option<NodeRef>,
    pub from: NodeRef,
    pub nodes: Vec<NodeRef>,
    pub types: Vec<ConstructedType>,
    pub trace: Vec<Box<dyn Constraint>>,
}

pub fn conflicting_types(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(QueriedConflictingTypes)) {
    let Some(Typed { group: Some(group) }) = ctx.db.get(&ctx.node) else {
        return;
    };

    if group.types.len() <= 1 || !is_first_node_in_group(ctx, &group) {
        return;
    }

    let source = ctx
        .db
        .get::<Instantiated>(&ctx.node)
        .map(|fact| fact.source_node);

    let nodes = group
        .nodes
        .iter()
        .filter(|&node| *node != ctx.node)
        .cloned()
        .collect::<Vec<_>>();

    f(QueriedConflictingTypes {
        source,
        from: ctx.node.clone(),
        nodes,
        types: group.types.clone(),
        trace: group.trace.clone(),
    });
}

pub fn incomplete_type(ctx: &QueryCtx<'_>, f: &mut dyn FnMut((NodeRef, ConstructedType))) {
    let Some(Typed { group: Some(group) }) = ctx.db.get(&ctx.node) else {
        return;
    };

    if group.types.len() != 1 || !is_first_node_in_group(ctx, &group) {
        return;
    }

    let ty = group.types.first().cloned().unwrap();
    if Type::Constructed(ty.clone()).references_nodes(|_| true) {
        f((ctx.node.clone(), ty));
    }
}

pub fn unknown_type(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(NodeRef)) {
    let Some(Typed { group: Some(group) }) = ctx.db.get(&ctx.node) else {
        return;
    };

    if group.types.is_empty() && is_first_node_in_group(ctx, &group) {
        f(ctx.node.clone());
    }
}

fn is_first_node_in_group(ctx: &QueryCtx<'_>, group: &Group) -> bool {
    group.nodes.first().is_some_and(|node| *node == ctx.node)
}
