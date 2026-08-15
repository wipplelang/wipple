use crate::QueryCtx;
use std::collections::BTreeSet;
use wipple_core::{
    db::Node,
    traces::Traces,
    typecheck::{groups::Typed, instantiate::Instantiated, ty::ConstructedTy},
};

pub fn has_type<'a>(db: &QueryCtx<'a>, node: Node) -> Option<&'a ConstructedTy> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    group.tys().next()
}

pub fn in_group(db: &QueryCtx<'_>, node: Node) -> impl Iterator<Item = Node> {
    let Some(Typed(Some(group))) = db.get(node) else {
        return Default::default();
    };

    group.nodes().collect::<Vec<_>>().into_iter()
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

    if group.tys().count() <= 1 {
        return None;
    }

    let (mut nodes, _) = group.min_ranked_nodes().unwrap_or_default();

    if !nodes.remove(&node) {
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
        tys: group.tys().cloned().collect(),
        traces,
    })
}

pub fn incomplete_type<'a>(db: &QueryCtx<'a>, node: Node) -> Option<(Node, &'a ConstructedTy)> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    let mut tys = group.tys();
    let ty = tys.next()?;

    if tys.next().is_some() {
        return None;
    }

    if ty.children.iter().any(|&ty| {
        db.get(ty)
            .and_then(|Typed(group)| group.as_ref())
            .is_some_and(|group| group.tys().next().is_none())
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

    group.tys().next().is_none()
}
