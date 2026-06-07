use crate::{
    db::{Db, Node},
    facts::Syntax,
    span::Str,
    typecheck::{groups::Typed, instantiate::Instantiated, solver::GroupedWith, ty::ConstructedTy},
    visit::{TypeParameters, definitions::Defined},
};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, ops::ControlFlow};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Link {
    pub node: Node,
    pub related: Vec<Node>,
    pub tys: Vec<ConstructedTy>,
}

pub fn get_links(db: &Db, definition_node: Node, source_node: Node) -> BTreeMap<Str, Link> {
    let mut links = BTreeMap::new();

    let Some(Defined(definition)) = db.get(definition_node) else {
        return links;
    };

    let mut nodes = Vec::new();

    if let Some(name) = definition.name() {
        links.insert(
            name.clone(),
            Link {
                node: definition_node,
                related: Vec::new(),
                tys: Vec::new(),
            },
        );

        nodes.push((name.clone(), definition_node));
    }

    if let Some(TypeParameters(parameters)) = db.get(definition_node) {
        for &parameter in parameters {
            let Some(Defined(definition)) = db.get(parameter) else {
                continue;
            };

            let Some(name) = definition.name() else {
                continue;
            };

            nodes.push((name.clone(), parameter));
        }
    }

    for (name, name_node) in nodes {
        let Some(instantiated_node) = instantiated_node_for(db, name_node, source_node) else {
            continue;
        };

        let Some(Typed(Some(group))) = db.get(instantiated_node) else {
            continue;
        };

        links.insert(
            name,
            Link {
                node: instantiated_node,
                related: group.nodes.clone(),
                tys: group.tys.clone(),
            },
        );
    }

    links
}

pub fn instantiated_node_for(db: &Db, parameter: Node, source_node: Node) -> Option<Node> {
    let instantiated_node = db.for_each_fact::<Instantiated, _>(&mut |_, node, instantiated| {
        if instantiated.from == parameter && instantiated.source_node == source_node {
            ControlFlow::Break(node)
        } else {
            ControlFlow::Continue(())
        }
    })?;

    let Some(Typed(Some(group))) = db.get(instantiated_node) else {
        return None;
    };

    // Prefer using the first node from the source that was grouped with
    // `instantiated`
    let node = db
        .get(instantiated_node)
        .and_then(|GroupedWith(nodes)| {
            nodes
                .iter()
                .copied()
                .find(|&node| !db.contains::<Instantiated>(node))
        })
        .or_else(|| {
            group
                .nodes
                .iter()
                .copied()
                .find(|&node| db.contains::<Syntax>(node))
        })
        .unwrap_or(group.nodes[0]);

    Some(node)
}
