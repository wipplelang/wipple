use wipple_core::{
    db::{Db, Node},
    typecheck::{
        constraints::Constraint,
        groups::Typed,
        instantiate::Instantiated,
        ty::{ConstructedTy, Ty},
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
    pub trace: Vec<Box<dyn Constraint>>,
}

pub fn conflicting_types(db: &Db, node: Node) -> Option<ConflictingTypes> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    if group.tys.len() <= 1 {
        return None;
    }

    Some(ConflictingTypes {
        source: db
            .get::<Instantiated>(node)
            .map(|instantiated| instantiated.source_node),
        from: node,
        nodes: group
            .nodes
            .iter()
            .copied()
            .filter(|other| *other != node)
            .collect(),
        tys: group.tys.clone(),
        trace: group.trace.clone(),
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

    Ty::Constructed(ty.clone())
        .references_nodes()
        .then_some((node, ty))
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
