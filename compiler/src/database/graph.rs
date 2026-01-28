use crate::{
    database::{Db, NodeRef},
    nodes::{InstanceDefinitionNode, ResolvedConstantAssignment},
};
use petgraph::prelude::DiGraphMap;
use serde::Serialize;
use serde_json::json;
use std::collections::BTreeSet;

#[derive(Debug, Clone, Default)]
pub struct Graph {
    mask: im::OrdSet<NodeRef>,
    replacements: im::OrdMap<NodeRef, NodeRef>,
    edges: im::Vector<(NodeRef, NodeRef, &'static str)>,
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
    pub labels: Vec<String>,
}

impl Graph {
    pub fn set_mask(&mut self, nodes: impl IntoIterator<Item = NodeRef>) {
        self.mask = im::OrdSet::from_iter(nodes);
    }

    pub fn replace(&mut self, from: &NodeRef, to: &NodeRef) {
        self.replacements.insert(from.clone(), to.clone());
    }

    pub fn edge(&mut self, from: &NodeRef, to: &NodeRef, label: &'static str) {
        self.edges.push_back((from.clone(), to.clone(), label));
    }

    pub fn group(
        &mut self,
        nodes: impl IntoIterator<Item = NodeRef>,
        labels: impl IntoIterator<Item = String>,
    ) {
        self.groups.push_back(Group {
            nodes: Vec::from_iter(nodes),
            labels: Vec::from_iter(labels),
        });
    }

    pub fn serialize(&self, db: &Db) -> impl Serialize + use<> {
        #[derive(Debug, Default, Serialize)]
        struct Result {
            groups: Vec<serde_json::Value>,
            nodes: Vec<serde_json::Value>,
            edges: Vec<serde_json::Value>,
        }

        let mut result = Result::default();

        // Keep groups containing nodes reachable from a node in the mask

        let mut reachable_nodes = BTreeSet::new();
        let mut in_group = BTreeSet::new();
        let mut visited_groups = BTreeSet::new();
        let mut groups = Vec::<(BTreeSet<NodeRef>, Vec<String>)>::new();
        let mut graph = DiGraphMap::<usize, &str>::new();
        loop {
            let mut progress = false;

            for (index, group) in self.groups.iter().enumerate() {
                if visited_groups.contains(&index) {
                    continue;
                }

                let nodes = group
                    .nodes
                    .iter()
                    .map(|node| self.replacements.get(node).unwrap_or(node))
                    .filter(|node| displayable(db, node))
                    .cloned()
                    .collect::<BTreeSet<_>>();

                if let Some((existing_nodes, existing_labels)) =
                    groups.iter_mut().find(|(existing_nodes, _)| {
                        existing_nodes.intersection(&nodes).next().is_some()
                    })
                {
                    existing_nodes.extend(nodes.iter().cloned());

                    for label in &group.labels {
                        if !existing_labels.contains(label) {
                            existing_labels.push(label.clone());
                        }
                    }
                } else if nodes
                    .iter()
                    .any(|node| self.mask.contains(node) || reachable_nodes.contains(node))
                {
                    groups.push((nodes.clone(), group.labels.clone()));
                } else {
                    continue;
                }

                in_group.extend(nodes.iter().map(|node| node.id()));
                reachable_nodes.extend(nodes);
                visited_groups.insert(index);
                progress = true;
            }

            for (from, to, label) in &self.edges {
                let from = self.replacements.get(from).unwrap_or(from);
                let to = self.replacements.get(to).unwrap_or(to);

                if !reachable_nodes.contains(from) && !reachable_nodes.contains(to) {
                    continue;
                }

                if !displayable(db, from) || !displayable(db, to) {
                    continue;
                }

                reachable_nodes.insert(from.clone());
                reachable_nodes.insert(to.clone());

                if graph.add_edge(from.id(), to.id(), *label).is_none() {
                    progress = true;
                }
            }

            if !progress {
                break;
            }
        }

        let replaced = self
            .replacements
            .keys()
            .map(|node| node.id())
            .collect::<BTreeSet<_>>();

        // Remove replaced nodes
        in_group.retain(|node| !replaced.contains(node));

        // Remove unconnected nodes
        in_group.retain(|node| {
            graph
                .all_edges()
                .any(|(from, to, _)| from == *node || to == *node)
        });

        for node in reachable_nodes {
            if !in_group.contains(&node.id()) {
                continue;
            }

            result.nodes.push(json!({
                "id": format!("node{}", node.id()),
                "span": db.span(&node),
            }));
        }

        for (nodes, labels) in groups {
            result.groups.push(json!({
                "nodes": nodes
                    .into_iter()
                    .filter(|node| in_group.contains(&node.id()))
                    .map(|node| format!("node{}", node.id()))
                    .collect::<Vec<_>>(),
                "labels": labels,
            }));
        }

        for (from, to, label) in graph.all_edges() {
            if !in_group.contains(&from) || !in_group.contains(&to) {
                continue;
            }

            result.edges.push(json!({
                "from": format!("node{from}"),
                "to": format!("node{to}"),
                "label": label,
            }));
        }

        result
    }
}

fn displayable(db: &Db, node: &NodeRef) -> bool {
    if node.is_hidden() {
        return false;
    }

    // Hide constant/instance values
    if db.parent(node).is_some_and(|parent| {
        db.contains::<ResolvedConstantAssignment>(&parent)
            || parent.downcast_ref::<InstanceDefinitionNode>().is_some()
    }) {
        return false;
    }

    true
}
