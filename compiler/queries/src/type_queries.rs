use crate::QueryCtx;
use std::collections::BTreeSet;
use wipple_core::{
    db::Node,
    traces::Traces,
    typecheck::{
        groups::{NodeRank, Typed},
        instantiate::Instantiated,
        ty::ConstructedTy,
    },
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
    pub related: BTreeSet<Node>,
    pub group: BTreeSet<Node>,
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

    if group.get_rank(node) > group.min_rank() {
        return None;
    }

    let traces = db.traces_for(node, group.nodes());

    let source = db
        .get::<Instantiated>(node)
        .map(|instantiated| instantiated.source_node);

    let mut related = group.nodes().collect::<BTreeSet<_>>();
    related.remove(&node);
    related.retain(|&node| group.get_rank(node) <= NodeRank::Inherited);

    Some(ConflictingTypes {
        source,
        from: node,
        related,
        group: group.nodes().collect(),
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
