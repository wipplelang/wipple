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

pub fn section<'a, S: AsRef<str>>(mut comments: &'a [S], section: Option<&str>) -> Option<&'a [S]> {
    if let Some(section) = section {
        let heading = format!("# {section}");

        comments = comments
            .splitn(2, |line| line.as_ref().trim() == heading)
            .nth(1)?;
    }

    comments
        .splitn(2, |line| line.as_ref().trim().starts_with('#'))
        .next()
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Link {
    pub kind: LinkKind,
    pub related: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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
        if let Some(instantiated_node) =
            linked_nodes_for(db, parameter_node, source_node, &mut filter)
                .and_then(|mut iter| iter.next())
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

pub fn linked_nodes_for<'a>(
    db: &'a Db,
    parameter: Node,
    source_node: Node,
    mut filter: impl FnMut(&Db, Node) -> bool + 'a,
) -> Option<impl Iterator<Item = Node> + 'a> {
    let instantiated_node = db
        .get(source_node)
        .and_then(|InstantiatedTypes(instantiated_tys)| instantiated_tys.get(&parameter))
        .copied()?;

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

    // Find all paths terminating in a non-instantiated node
    let paths = paths.into_iter().filter(|path| {
        let (&last, prefix) = path.split_last().unwrap();

        !db.contains::<Instantiated>(last)
            && prefix.iter().all(|&node| db.contains::<Instantiated>(node))
    });

    Some(paths.filter_map(move |path| {
        // Return the most recent node that has a type
        path.into_iter().rev().find(|&node| {
            filter(db, node)
                && db
                    .get(node)
                    .and_then(|Typed(group)| group.as_ref())
                    .is_some()
        })
    }))
}
