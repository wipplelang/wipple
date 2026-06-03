use crate::CompileResult;
use std::collections::BTreeSet;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct Graph {
    pub groups: Vec<GraphGroup>,
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct GraphGroup {
    pub nodes: Vec<String>,
    pub labels: Vec<GraphLabel>,
    pub conflict: bool,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct GraphNode {
    pub id: String,
    pub span: GraphSpan,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct GraphLocation {
    pub index: usize,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct GraphSpan {
    pub start: GraphLocation,
    pub end: GraphLocation,
    pub source: String,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct GraphLabel {
    pub kind: Option<String>,
    pub display: String,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct GraphEdge {
    pub from: String,
    pub to: String,
    pub label: String,
}

#[wasm_bindgen]
impl CompileResult {
    pub fn graph(&self) -> Graph {
        let mask = self
            .db
            .owned_nodes()
            .filter(|&node| !self.db.is_hidden(node))
            .collect::<BTreeSet<_>>();

        convert_graph(self.db.graph.build(&self.db, &mask))
    }
}

pub fn convert_graph(graph: wipple_core::graph::Graph) -> Graph {
    Graph {
        groups: graph
            .groups
            .into_iter()
            .map(|group| GraphGroup {
                nodes: group.nodes,
                labels: group
                    .labels
                    .into_iter()
                    .map(|label| GraphLabel {
                        kind: label.kind,
                        display: label.display,
                    })
                    .collect(),
                conflict: group.conflict,
            })
            .collect(),
        nodes: graph
            .nodes
            .into_iter()
            .map(|node| GraphNode {
                id: node.id,
                span: GraphSpan {
                    start: GraphLocation {
                        index: node.span.start.index,
                    },
                    end: GraphLocation {
                        index: node.span.end.index,
                    },
                    source: node.span.source.to_string(),
                },
            })
            .collect(),
        edges: graph
            .edges
            .into_iter()
            .map(|edge| GraphEdge {
                from: edge.from,
                to: edge.to,
                label: edge.label,
            })
            .collect(),
    }
}
