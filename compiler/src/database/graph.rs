use crate::{
    database::{Db, NodeRef},
    typecheck::{ConstructedType, Instantiated, Type},
};
use petgraph::prelude::DiGraphMap;
use serde::Serialize;
use serde_json::json;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Default)]
pub struct Graph {
    mask: im::OrdSet<NodeRef>,
    replacements: im::OrdMap<NodeRef, NodeRef>, // global replacements
    instantiated: im::OrdMap<NodeRef, im::OrdMap<NodeRef, NodeRef>>, // replacements originating from a specific node
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
    pub labels: Vec<ConstructedType>,
}

impl Graph {
    pub fn set_mask(&mut self, nodes: impl IntoIterator<Item = NodeRef>) {
        self.mask = im::OrdSet::from_iter(nodes);
    }

    pub fn replace(&mut self, node: &NodeRef, replacement: &NodeRef) {
        self.replacements.insert(node.clone(), replacement.clone());
    }

    pub fn instantiate(&mut self, source: &NodeRef, node: &NodeRef, instantiated: &NodeRef) {
        self.instantiated
            .entry(source.clone())
            .or_default()
            .insert(node.clone(), instantiated.clone());
    }

    pub fn edge(&mut self, from: &NodeRef, to: &NodeRef, label: &'static str) {
        self.edges.push_back((from.clone(), to.clone(), label));
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
        const MAX_ITERATIONS: usize = 4;

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

        let mut reachable_instantiated = Vec::new();
        let mut in_group = BTreeSet::new();
        let mut finished_groups = BTreeSet::new();
        let mut groups = BTreeMap::<usize, (BTreeSet<NodeRef>, Vec<ConstructedType>)>::new();
        let mut graph = DiGraphMap::<usize, &str>::new();
        for _ in 0..MAX_ITERATIONS {
            let mut progress = false;

            for node in reachable_nodes.keys() {
                if let Some(instantiated) = self.instantiated.get(node)
                    && !reachable_instantiated.iter().any(|&existing| {
                        std::ptr::eq(existing as *const _, instantiated as *const _)
                    })
                {
                    reachable_instantiated.push(instantiated);
                }
            }

            for (index, group) in self.groups.iter().enumerate() {
                if finished_groups.contains(&index) {
                    continue;
                }

                let mut nodes = group
                    .nodes
                    .iter()
                    .filter(|node| self.can_display(db, node))
                    .map(|node| self.replacement_for(node, &reachable_instantiated))
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

            for (from, to, label) in &self.edges {
                let from = self.replacement_for(from, &reachable_instantiated);
                let to = self.replacement_for(to, &reachable_instantiated);

                if !reachable_nodes.contains_key(&to) {
                    continue;
                }

                if !self.can_display(db, &from) || !self.can_display(db, &to) {
                    continue;
                }

                reachable_nodes.entry(from.clone()).or_insert(None);
                reachable_nodes.entry(to.clone()).or_insert(None);

                if graph.add_edge(from.id(), to.id(), *label).is_none() {
                    progress = true;
                }
            }

            if !progress {
                break;
            }
        }

        // If there are non-generic types present, remove the groups with only type parameters

        let contains_non_generic = |types: &[ConstructedType]| {
            types.is_empty() || types.iter().any(|ty| ty.instantiate.is_none())
        };

        if groups
            .values()
            .any(|(_, types)| contains_non_generic(types))
        {
            groups.retain(|_, (nodes, types)| {
                let keep = contains_non_generic(types);

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

        for node in reachable_nodes.into_keys() {
            if !in_group.contains(&node.id()) {
                continue;
            }

            let mut span = serde_json::json!(db.span(&node));
            span["source"] = db.span(&node).as_node_source().into();

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

            if labels.is_empty() {
                labels.push(String::from("_"));
            }

            result.groups.push(json!({
                "nodes": nodes
                    .into_iter()
                    .map(|node| format!("node{}", node.id()))
                    .collect::<Vec<_>>(),
                "labels": labels,
            }));
        }

        result
    }

    fn can_display(&self, db: &Db, node: &NodeRef) -> bool {
        !node.is_hidden() || db.contains::<Instantiated>(node)
    }

    fn replacement_for<'a: 'b, 'b>(
        &'a self,
        node: &NodeRef,
        instantiated: &[&'b im::OrdMap<NodeRef, NodeRef>],
    ) -> NodeRef {
        let replacements = [&self.replacements]
            .into_iter()
            .chain(instantiated.iter().copied());

        let mut node = node.clone();
        for entry in replacements {
            while let Some(replacement) = entry.get(&node) {
                node = replacement.clone();
            }
        }

        node
    }
}
