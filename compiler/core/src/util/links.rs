use crate::{
    db::{Db, Node},
    span::Str,
    typecheck::{
        groups::Typed,
        instantiate::{Instantiated, InstantiatedTypes},
        solver::DirectlyGroupedWith,
    },
    visit::{TypeParameters, definitions::Defined},
};
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet},
    slice,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Link {
    pub kind: LinkKind,
    pub related: Vec<Node>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LinkKind {
    Node(Node),
    Type(Node),
    List { nodes: Vec<Node>, separator: String },
}

impl Link {
    pub fn node(node: Node) -> Self {
        Link {
            kind: LinkKind::Node(node),
            related: Vec::new(),
        }
    }

    pub fn ty(node: Node) -> Self {
        Link {
            kind: LinkKind::Type(node),
            related: Vec::new(),
        }
    }

    pub fn list(separator: impl ToString, nodes: impl IntoIterator<Item = Node>) -> Self {
        Link {
            kind: LinkKind::List {
                nodes: Vec::from_iter(nodes),
                separator: separator.to_string(),
            },
            related: Vec::new(),
        }
    }

    pub fn nodes(&self) -> &[Node] {
        match &self.kind {
            LinkKind::Node(node) => slice::from_ref(node),
            LinkKind::Type(node) => slice::from_ref(node),
            LinkKind::List { nodes, .. } => nodes.as_slice(),
        }
    }
}

pub fn get_links(
    db: &Db,
    definition_node: Node,
    source_node: Node,
    mut filter: impl FnMut(&Db, Node) -> bool,
) -> BTreeMap<Str, Link> {
    let mut links = BTreeMap::new();

    let Some(Defined(definition)) = db.get(definition_node) else {
        return links;
    };

    let mut nodes = Vec::new();

    if let Some(name) = definition.name() {
        links.insert(name.clone(), Link::node(source_node));
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
        if let Some(instantiated_node) = linked_node_for(db, parameter_node, source_node)
            && let Some(Typed(Some(group))) = db.get(instantiated_node)
        {
            let mut link = if db.contains::<Instantiated>(instantiated_node) {
                Link::ty(instantiated_node)
            } else {
                Link::node(instantiated_node)
            };

            for node in group.nodes() {
                if filter(db, node) {
                    link.related.push(node);
                }
            }

            links.insert(name, link);
        }
    }

    links
}

fn linked_node_for(db: &Db, parameter: Node, source_node: Node) -> Option<Node> {
    let InstantiatedTypes(instantiated_tys) = db.get(source_node).cloned().unwrap_or_default();

    let instantiated_node = instantiated_tys.get(&parameter).copied()?;

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

            let DirectlyGroupedWith(others) = db.get(node).cloned().unwrap_or_default();

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
