use crate::QueryCtx;
use std::collections::BTreeSet;
use wipple_core::{
    db::Node,
    traces::Traces,
    typecheck::{
        groups::{Annotated, Typed},
        instantiate::Instantiated,
        ty::ConstructedTy,
    },
};

pub fn has_type<'a>(db: &QueryCtx<'a>, node: Node) -> Option<&'a ConstructedTy> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    group.tys.first()
}

pub fn in_group(db: &QueryCtx<'_>, node: Node) -> impl Iterator<Item = Node> {
    let Some(Typed(Some(group))) = db.get(node) else {
        return Default::default();
    };

    group.nodes.iter().copied()
}

#[derive(Debug, Clone)]
pub struct ConflictingTypes {
    pub source: Option<Node>,
    pub from: Node,
    pub nodes: BTreeSet<Node>,
    pub tys: Vec<ConstructedTy>,
    pub traces: Traces,
}

pub fn conflicting_types(db: &QueryCtx<'_>, node: Node) -> Option<ConflictingTypes> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    if group.tys.len() <= 1 {
        return None;
    }

    let mut nodes = group.nodes.clone();
    nodes.remove(&node);

    // Prioritize non-annotated nodes
    if db.contains::<Annotated>(node)
        && nodes
            .iter()
            .any(|&other| !db.contains::<Annotated>(other) && db.filter(other))
    {
        return None;
    }

    let traces = db.traces_for(node, nodes.iter().copied());

    let (source, from) = db
        .get::<Instantiated>(node)
        .map(|instantiated| (Some(instantiated.source_node), instantiated.from))
        .unwrap_or((None, node));

    Some(ConflictingTypes {
        source,
        from,
        nodes,
        tys: group.tys.clone(),
        traces,
    })
}

pub fn incomplete_type<'a>(db: &QueryCtx<'a>, node: Node) -> Option<(Node, &'a ConstructedTy)> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    if group.tys.len() != 1 {
        return None;
    }

    let ty = &group.tys[0];

    if ty.children.iter().any(|&ty| {
        db.get(ty)
            .and_then(|Typed(group)| group.as_ref())
            .is_some_and(|group| group.tys.is_empty())
    }) {
        return Some((node, ty));
    }

    None
}

pub fn unknown_type(db: &QueryCtx<'_>, node: Node) -> bool {
    let Some(Typed(group)) = db.get(node) else {
        return false;
    };

    let Some(group) = group else {
        return true;
    };

    group.tys.is_empty()
}
