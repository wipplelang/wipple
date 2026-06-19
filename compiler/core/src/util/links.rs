use crate::{
    db::{Db, Node},
    span::Str,
    typecheck::{
        groups::Typed,
        instantiate::{Instantiated, InstantiatedParameters},
        solver::GroupedWith,
    },
    visit::{TypeParameters, definitions::Defined},
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Link {
    pub node: Node,
    pub force_type: bool,
    pub related: Vec<Node>,
}

pub fn get_links(
    db: &Db,
    definition_node: Node,
    source_node: Node,
    mut get_instantiated: impl FnMut(Node) -> Option<Node>,
) -> BTreeMap<Str, Link> {
    let mut links = BTreeMap::new();

    let Some(Defined(definition)) = db.get(definition_node) else {
        return links;
    };

    let mut nodes = Vec::new();

    if let Some(name) = definition.name() {
        links.insert(
            name.clone(),
            Link {
                node: source_node,
                force_type: false,
                related: Vec::new(),
            },
        );

        nodes.push((name.clone(), source_node));
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

    for (name, parameter_node) in nodes {
        if let Some(instantiated_node) = get_instantiated(parameter_node)
            && let Some(Typed(Some(group))) = db.get(instantiated_node)
        {
            links.insert(
                name,
                Link {
                    node: instantiated_node,
                    force_type: db.contains::<Instantiated>(instantiated_node),
                    related: group.nodes.iter().copied().collect(),
                },
            );
        }
    }

    links
}

pub fn instantiated_node_for(db: &Db, parameter: Node, source_node: Node) -> Option<Node> {
    let InstantiatedParameters(parameters) = db.get(source_node).cloned().unwrap_or_default();

    let instantiated_node = parameters.get(&parameter).copied()?;

    // Collect all nodes related to the instantiated node
    let mut paths = BTreeSet::from([vec![instantiated_node]]);
    let mut seen = BTreeSet::new();
    loop {
        let mut progress = false;
        for prefix in paths.clone() {
            let node = *prefix.last().unwrap();
            if !seen.insert(node) {
                continue;
            }

            let GroupedWith(others) = db.get(node).cloned().unwrap_or_default();

            for other in others {
                let mut path = prefix.clone();
                path.push(other);

                progress |= paths.insert(path);
            }
        }

        if !progress {
            break;
        }
    }

    // Find a path terminating in a non-instantiated node

    let path = paths.into_iter().find(|path| {
        let (&last, prefix) = path.split_last().unwrap();

        !db.contains::<Instantiated>(last)
            && prefix.iter().all(|&node| db.contains::<Instantiated>(node))
    })?;

    // Return the most recent node that has a type
    path.into_iter().rev().find(|&node| {
        db.get(node)
            .and_then(|Typed(group)| group.as_ref())
            .is_some()
    })
}
