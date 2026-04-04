use crate::{
    database::{Db, NodeRef},
    nodes::IsType,
    typecheck::{ConstructedType, ConstructedTypeTag, Instantiated, Type},
    visit::{Defined, Definition, Resolved},
};
use petgraph::prelude::DiGraphMap;
use serde::Serialize;
use serde_json::json;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Default)]
pub struct Graph {
    mask: im::OrdSet<NodeRef>,
    replacements: im::OrdMap<NodeRef, NodeRef>,
    edges: im::Vector<Edge>,
    groups: im::Vector<Group>,
}

#[derive(Debug, Clone)]
pub struct Edge {
    pub from: NodeRef,
    pub to: NodeRef,
    pub label: String,
}

#[derive(Debug, Clone)]
pub struct Group {
    pub nodes: Vec<NodeRef>,
    pub labels: Vec<ConstructedType>,
}

impl Graph {
    pub fn set_mask(&mut self, nodes: impl IntoIterator<Item = NodeRef>) {
        self.mask = im::OrdSet::from_iter(nodes);
    }

    /// This should only be called for nodes that are in the same group.
    pub fn replace(&mut self, node: &NodeRef, replacement: &NodeRef) {
        self.replacements.insert(node.clone(), replacement.clone());
    }

    pub fn edge(&mut self, from: &NodeRef, to: &NodeRef, label: &'static str) {
        self.edges.push_back(Edge {
            from: from.clone(),
            to: to.clone(),
            label: label.to_string(),
        });
    }

    pub fn group(
        &mut self,
        nodes: impl IntoIterator<Item = NodeRef>,
        labels: impl IntoIterator<Item = ConstructedType>,
    ) {
        self.groups.push_back(Group {
            nodes: Vec::from_iter(nodes),
            labels: Vec::from_iter(labels),
        });
    }

    pub fn serialize(&self, db: &Db) -> impl Serialize + use<> {
        const MAX_ITERATIONS: usize = 10;

        #[derive(Debug, Default, Serialize)]
        struct Result {
            groups: Vec<serde_json::Value>,
            nodes: Vec<serde_json::Value>,
            edges: Vec<serde_json::Value>,
        }

        let mut reachable_nodes = self
            .mask
            .iter()
            .map(|node| (node.clone(), None::<usize>))
            .collect::<BTreeMap<_, _>>();

        let mut in_group = BTreeSet::new();
        let mut finished_groups = BTreeSet::new();
        let mut groups = BTreeMap::<usize, (BTreeSet<NodeRef>, Vec<ConstructedType>)>::new();
        let mut graph = DiGraphMap::<usize, String>::new();
        for _ in 0..MAX_ITERATIONS {
            let mut progress = false;

            for (index, group) in self.groups.iter().enumerate() {
                if finished_groups.contains(&index) {
                    continue;
                }

                let mut nodes = group
                    .nodes
                    .iter()
                    .filter(|node| self.can_display(db, node))
                    .map(|node| self.replacement_for(node))
                    .collect::<BTreeSet<_>>();

                nodes.retain(|node| {
                    reachable_nodes
                        .get(node)
                        .is_some_and(|existing| existing.is_none_or(|existing| existing == index))
                });

                if nodes.is_empty() {
                    continue;
                }

                if let Some((existing_nodes, existing_labels)) = groups.get_mut(&index) {
                    if *existing_nodes == nodes {
                        finished_groups.insert(index);
                    } else {
                        existing_nodes.extend(nodes.iter().cloned());
                        progress = true;
                    }

                    for label in &group.labels {
                        if !existing_labels.contains(label) {
                            existing_labels.push(label.clone());
                        }
                    }
                } else {
                    groups.insert(index, (nodes.clone(), group.labels.clone()));
                    progress = true;
                }

                in_group.extend(nodes.iter().map(|node| node.id()));
                reachable_nodes.extend(nodes.into_iter().map(|node| (node, Some(index))));
            }

            for edge in &self.edges {
                let from = self.replacement_for(&edge.from);
                let to = self.replacement_for(&edge.to);
                if !reachable_nodes.contains_key(&from) && !reachable_nodes.contains_key(&to) {
                    continue;
                }

                if !self.can_display(db, &from) || !self.can_display(db, &to) {
                    continue;
                }

                reachable_nodes.entry(from.clone()).or_insert(None);
                reachable_nodes.entry(to.clone()).or_insert(None);

                if graph
                    .add_edge(from.id(), to.id(), edge.label.clone())
                    .is_none()
                {
                    progress = true;
                }
            }

            if !progress {
                break;
            }
        }

        // Remove groups containing only types
        let contains_non_type =
            |nodes: &BTreeSet<NodeRef>| nodes.iter().any(|node| !db.contains::<IsType>(node));
        if groups.values().any(|(nodes, _)| contains_non_type(nodes)) {
            groups.retain(|_, (nodes, _)| {
                let keep = contains_non_type(nodes);

                if !keep {
                    for node in nodes.iter() {
                        in_group.remove(&node.id());
                    }
                }

                keep
            });
        }

        let mut result = Result::default();

        let mut connected_nodes = BTreeSet::new();
        for (from, to, label) in graph.all_edges() {
            if !in_group.contains(&from) || !in_group.contains(&to) {
                continue;
            }

            connected_nodes.insert(from);
            connected_nodes.insert(to);

            result.edges.push(json!({
                "from": format!("node{from}"),
                "to": format!("node{to}"),
                "label": label,
            }));
        }

        // Remove unconnected nodes
        in_group.retain(|id| connected_nodes.contains(id));

        // Remove groups containing only function constants
        groups.retain(|_, (nodes, types)| {
            if nodes.len() != 1 {
                return true;
            }

            let node = nodes.iter().next().unwrap();
            let Some(Resolved { definitions, .. }) = db.get(node) else {
                return true;
            };

            let [definition] = definitions.as_slice() else {
                return true;
            };

            let Some(Defined(definition)) = db.get(definition) else {
                return true;
            };

            if let Definition::Variable(_) = definition {
                return true;
            }

            !matches!(
                types.as_slice(),
                [ConstructedType {
                    tag: ConstructedTypeTag::Function,
                    ..
                }],
            )
        });

        for node in reachable_nodes.into_keys() {
            if !in_group.contains(&node.id()) {
                continue;
            }

            let mut span = serde_json::json!(db.span(&node));
            span["source"] = db.span(&node).source.into();

            result.nodes.push(json!({
                "id": format!("node{}", node.id()),
                "span": span,
            }));
        }

        for (mut nodes, labels) in groups.into_values() {
            nodes.retain(|node| in_group.contains(&node.id()));

            if nodes.is_empty() {
                continue;
            }

            let mut labels = labels
                .into_iter()
                .map(|ty| Type::Constructed(ty).to_string(db, true))
                .collect::<Vec<_>>();

            let conflict = if labels.is_empty() {
                labels.push(String::from("_"));
                true
            } else {
                labels.len() > 1
            };

            result.groups.push(json!({
                "nodes": nodes
                    .into_iter()
                    .map(|node| format!("node{}", node.id()))
                    .collect::<Vec<_>>(),
                "labels": labels,
                "conflict": conflict,
            }));
        }

        result
    }

    fn can_display(&self, db: &Db, node: &NodeRef) -> bool {
        (!node.is_hidden() || db.contains::<Instantiated>(node)) && !db.contains::<Defined>(node)
    }

    fn replacement_for(&self, node: &NodeRef) -> NodeRef {
        let mut node = node.clone();
        while let Some(replacement) = self.replacements.get(&node) {
            node = replacement.clone();
        }

        node
    }
}
