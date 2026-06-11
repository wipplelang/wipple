use wipple_core::{
    db::{Db, Node},
    traces::Traces,
    typecheck::{
        groups::{Annotated, Typed},
        instantiate::Instantiated,
        ty::ConstructedTy,
    },
};

pub fn has_type(db: &Db, node: Node) -> Option<&ConstructedTy> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    group.tys.first()
}

pub fn in_group(db: &Db, node: Node) -> &[Node] {
    let Some(Typed(Some(group))) = db.get(node) else {
        return &[];
    };

    &group.nodes
}

#[derive(Debug, Clone)]
pub struct ConflictingTypes {
    pub source: Option<Node>,
    pub from: Node,
    pub nodes: Vec<Node>,
    pub tys: Vec<ConstructedTy>,
    pub traces: Traces,
}

pub fn conflicting_types(db: &Db, node: Node) -> Option<ConflictingTypes> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    if group.tys.len() <= 1 {
        return None;
    }

    let mut nodes = group
        .nodes
        .iter()
        .copied()
        .filter(|other| *other != node)
        .collect::<Vec<_>>();

    // Prioritize non-annotated nodes
    if db.contains::<Annotated>(node) && nodes.iter().any(|&other| !db.contains::<Annotated>(other))
    {
        return None;
    }

    let traces = db.traces_for(node, nodes.iter().copied());
    nodes.extend(traces.nodes(db));

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

pub fn incomplete_type(db: &Db, node: Node) -> Option<(Node, &ConstructedTy)> {
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

pub fn unknown_type(db: &Db, node: Node) -> bool {
    let Some(Typed(group)) = db.get(node) else {
        return false;
    };

    let Some(group) = group else {
        return true;
    };

    group.tys.is_empty()
}
