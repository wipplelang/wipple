use crate::database::{Db, NodeRef};
use petgraph::{
    prelude::{DiGraphMap, UnGraphMap},
    visit::Bfs,
};
use serde::Serialize;
use serde_json::json;
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Debug, Clone, Default)]
pub struct Graph {
    mask: HashSet<usize>,
    nodes: BTreeMap<NodeRef, usize>,
    keys: HashMap<usize, NodeRef>,
    edges: DiGraphMap<usize, &'static str>,
    groups: im::Vector<Group>,
    replacements: im::OrdMap<NodeRef, NodeRef>,
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
        self.mask = nodes
            .into_iter()
            .filter_map(|node| self.key(&node))
            .collect();
    }

    pub fn replace(&mut self, from: &NodeRef, to: &NodeRef) {
        self.replacements.insert(from.clone(), to.clone());
    }

    fn key(&mut self, node: &NodeRef) -> Option<usize> {
        let node = self.replacements.get(node).unwrap_or(node);
        if node.is_hidden() {
            return None;
        }

        let next = self.nodes.len();
        let key = *self.nodes.entry(node.clone()).or_insert(next);
        self.keys.insert(key, node.clone());
        Some(key)
    }

    fn try_key(&self, node: &NodeRef) -> Option<usize> {
        let node = self.replacements.get(node).unwrap_or(node);

        if node.is_hidden() {
            return None;
        }

        self.nodes.get(node).copied()
    }

    pub fn edge(&mut self, from: &NodeRef, to: &NodeRef, label: &'static str) {
        let Some(from) = self.key(from) else {
            return;
        };

        let Some(to) = self.key(to) else {
            return;
        };

        self.edges.add_edge(from, to, label);
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
        #[derive(Default, Serialize)]
        struct Result {
            groups: Vec<serde_json::Value>,
            nodes: Vec<serde_json::Value>,
            edges: Vec<serde_json::Value>,
        }

        let mut result = Result::default();

        // Keep groups containing nodes reachable from a node in the mask

        let reachable_from_mask = {
            let edges_undirected =
                UnGraphMap::<usize, &'static str>::from_edges(self.edges.all_edges());

            move |key: usize| {
                let mut found = false;
                let mut bfs = Bfs::new(&edges_undirected, key);
                while let Some(other) = bfs.next(&edges_undirected) {
                    if self.mask.contains(&other) {
                        found = true;
                        break;
                    }
                }

                found
            }
        };

        let mut reachable_keys = HashSet::new();
        let groups = self
            .groups
            .iter()
            .filter_map(|group| {
                let keys = group
                    .nodes
                    .iter()
                    .filter_map(|node| self.try_key(node))
                    .collect::<HashSet<_>>();

                let reachable = keys.iter().copied().any(&reachable_from_mask);

                if reachable {
                    reachable_keys.extend(keys.iter().copied());
                    Some((keys, group.labels.clone()))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let mut graph = DiGraphMap::<usize, &'static str>::new();
        for (from, to, label) in self.edges.all_edges() {
            if !reachable_keys.contains(&from) && !reachable_keys.contains(&to) {
                continue;
            }

            let from_node = self.keys.get(&from).unwrap();
            let to_node = self.keys.get(&to).unwrap();

            // Don't add edges between hidden and non-hidden nodes
            if from_node.is_hidden() && !to_node.is_hidden() {
                continue;
            }

            let Some(from) = self.try_key(from_node) else {
                continue;
            };

            let Some(to) = self.try_key(to_node) else {
                continue;
            };

            graph.add_edge(from, to, *label);
        }

        for key in &reachable_keys {
            let node = self.keys.get(key).unwrap();

            result.nodes.push(json!({
                "id": format!("node{key}"),
                "span": db.span(node),
            }));
        }

        for (keys, labels) in groups {
            result.groups.push(json!({
                "nodes": keys
                    .into_iter()
                    .map(|key| format!("node{key}"))
                    .collect::<Vec<_>>(),
                "labels": labels,
            }));
        }

        for (from, to, label) in graph.all_edges() {
            if !reachable_keys.contains(&from) || !reachable_keys.contains(&to) {
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
