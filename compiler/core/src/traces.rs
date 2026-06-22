use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{AnyConstraintTrace, ConstraintConsequence},
        groups::Typed,
    },
    visit::definitions::Defined,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Default)]
pub struct Traces {
    pub traces: Vec<TracesEntry>,
    pub edges: Vec<(usize, Option<usize>)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracesEntry {
    pub trace: AnyConstraintTrace,
    pub consequences: Vec<ConstraintConsequence>,
}

impl TracesEntry {
    pub fn new(trace: AnyConstraintTrace) -> Self {
        TracesEntry {
            trace,
            consequences: Default::default(),
        }
    }
}

impl Db {
    pub fn traces_for(&self, primary_node: Node, nodes: impl IntoIterator<Item = Node>) -> Traces {
        let mut traces = self.traces.clone();

        let filter = |node: Node| {
            // Exclude definitions because otherwise, all uses of the definition
            // are included in the trace
            !self.contains::<Defined>(node)
        };

        let group_nodes = self
            .get(primary_node)
            .and_then(|Typed(group)| group.as_ref())
            .map(|group| group.nodes.clone())
            .unwrap_or_default();

        let mut nodes = nodes
            .into_iter()
            .chain(group_nodes)
            .filter(|&node| filter(node))
            .map(|node| (node, BTreeSet::new()))
            .collect::<BTreeMap<_, _>>();

        let mut edges = Vec::new();

        let mut result = Vec::new();
        let mut added = BTreeSet::new();
        loop {
            let mut progress = false;

            for (trace_index, entry) in traces.iter_mut().enumerate() {
                if added.contains(&trace_index) {
                    continue;
                }

                let trace_nodes = entry.trace.nodes(self);

                let to_indices = trace_nodes
                    .iter()
                    .filter(|&&node| filter(node))
                    .filter_map(|node| nodes.get(node).cloned())
                    .collect::<BTreeSet<_>>();

                if to_indices.is_empty() {
                    continue;
                }

                added.insert(trace_index);

                let trace_primary_node = *entry.trace.source_node.get_or_insert(trace_nodes[0]);

                if self.is_hidden(trace_primary_node) {
                    continue;
                }

                let mut from_index = None;
                if entry.trace.allow_hidden_nodes()
                    || trace_nodes.iter().all(|node| !self.is_hidden(*node))
                {
                    let index = result.len();
                    from_index = Some(index);
                    if trace_primary_node == primary_node {
                        // Connect the trace with the final error
                        edges.push((index, None));
                    }

                    result.push((trace_index, trace_primary_node, entry.clone()));
                }

                for node in trace_nodes {
                    nodes.entry(node).or_default().insert(from_index);

                    // Use grouped nodes as a fallback
                    if let Some(Typed(Some(group))) = self.get(node) {
                        for &node in group.nodes.iter() {
                            if filter(node) {
                                let indices = nodes.entry(node).or_default();

                                if indices.is_empty() {
                                    indices.insert(from_index);
                                }
                            }
                        }
                    }
                }

                if let Some(from_index) = from_index {
                    for to_index in to_indices.into_iter().flatten().flatten() {
                        edges.push((from_index, Some(to_index)));
                    }

                    // Apply consequences from influencing traces
                    for &other_index in &entry.trace.from {
                        for (index, _, other_entry) in &mut result {
                            if *index == other_index {
                                other_entry.consequences.extend(entry.consequences.clone());
                            }
                        }
                    }
                }

                progress = true;
            }

            if !progress {
                break;
            }
        }

        edges.sort();
        edges.dedup();

        // If there are no edges to the error, connect it with the first trace
        if !result.is_empty() && !edges.iter().any(|(_, to)| to.is_none()) {
            edges.push((0, None));
        }

        Traces {
            traces: result.into_iter().map(|(_, _, entry)| entry).collect(),
            edges,
        }
    }
}

impl Traces {
    pub fn nodes(&self, db: &Db) -> impl Iterator<Item = Node> {
        self.traces
            .iter()
            .flat_map(|entry| entry.trace.clone().nodes(db))
    }
}
