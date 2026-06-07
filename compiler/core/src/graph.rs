use crate::{
    db::{Db, Node},
    facts::{GraphType, Syntax},
    span::Span,
    typecheck::{
        groups::update_type,
        instantiate::Instantiated,
        ty::{ConstructedTy, Ty, TyTag},
    },
    visit::definitions::{Defined, VariableDefinition},
};
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Write},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
struct GraphBuilderEdge {
    from: Node,
    to: Node,
    label: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphBuilderGroup {
    pub nodes: Vec<Node>,
    pub tys: Vec<ConstructedTy>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GraphBuilder {
    replacements: BTreeMap<Node, Node>,
    edges: Vec<GraphBuilderEdge>,
    pub groups: Vec<GraphBuilderGroup>,
}

impl GraphBuilder {
    pub fn replace(&mut self, node: Node, replacement: Node) {
        self.replacements.insert(node, replacement);
    }

    pub fn edge(&mut self, from: Node, to: Node, label: impl Into<String>) {
        self.edges.push(GraphBuilderEdge {
            from,
            to,
            label: label.into(),
        });
    }

    pub fn group(&mut self, nodes: Vec<Node>, tys: Vec<ConstructedTy>) {
        self.groups.push(GraphBuilderGroup { nodes, tys });
    }

    pub fn build(&self, db: &Db, mask: &BTreeSet<Node>) -> Graph {
        const MAX_ITERATIONS: usize = 10;

        let mut reachable_nodes = mask
            .iter()
            .map(|&node| (node, None::<usize>))
            .collect::<BTreeMap<_, _>>();

        let mut in_group = BTreeSet::new();
        let mut finished_groups = BTreeSet::new();
        let mut groups = BTreeMap::<usize, (BTreeSet<Node>, Vec<ConstructedTy>)>::new();
        let mut edges = BTreeSet::<GraphBuilderEdge>::new();
        for _ in 0..MAX_ITERATIONS {
            let mut progress = false;

            for (index, group) in self.groups.iter().enumerate() {
                if finished_groups.contains(&index) {
                    continue;
                }

                let mut nodes = group
                    .nodes
                    .iter()
                    .copied()
                    .filter(|&node| self.can_display(db, node))
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

                let tys = group
                    .tys
                    .iter()
                    .map(
                        |ty| match update_type(db, &Ty::Constructed(ty.clone()), true) {
                            Ty::Constructed(ty) => ty,
                            _ => unreachable!(),
                        },
                    )
                    .collect::<Vec<_>>();

                if let Some((existing_nodes, existing_tys)) = groups.get_mut(&index) {
                    if *existing_nodes == nodes {
                        finished_groups.insert(index);
                    } else {
                        existing_nodes.extend(nodes.iter().cloned());
                        progress = true;
                    }

                    for ty in tys {
                        if !existing_tys.contains(&ty) {
                            existing_tys.push(ty);
                        }
                    }
                } else {
                    groups.insert(index, (nodes.clone(), tys));
                    progress = true;
                }

                in_group.extend(nodes.iter().flat_map(|&node| self.id(db, node)));
                reachable_nodes.extend(nodes.into_iter().map(|node| (node, Some(index))));
            }

            for edge in &self.edges {
                let from = self.replacement_for(edge.from);
                let to = self.replacement_for(edge.to);
                if !reachable_nodes.contains_key(&from) && !reachable_nodes.contains_key(&to) {
                    continue;
                }

                if !self.can_display(db, from) || !self.can_display(db, to) {
                    continue;
                }

                reachable_nodes.entry(from).or_insert(None);
                reachable_nodes.entry(to).or_insert(None);

                if edges.insert(GraphBuilderEdge {
                    from,
                    to,
                    label: edge.label.clone(),
                }) {
                    progress = true;
                }
            }

            if !progress {
                break;
            }
        }

        // Remove groups containing only types
        let contains_non_type =
            |nodes: &BTreeSet<Node>| nodes.iter().any(|&node| !db.contains::<GraphType>(node));
        if groups.values().any(|(nodes, _)| contains_non_type(nodes)) {
            groups.retain(|_, (nodes, _)| {
                let keep = contains_non_type(nodes);

                if !keep {
                    for node in nodes.iter().flat_map(|&node| self.id(db, node)) {
                        in_group.remove(&node);
                    }
                }

                keep
            });
        }

        let mut result = Graph::default();

        let mut connected_nodes = BTreeSet::new();
        for edge in edges {
            let Some(from) = self.id(db, edge.from) else {
                continue;
            };

            let Some(to) = self.id(db, edge.to) else {
                continue;
            };

            if !in_group.contains(&from) || !in_group.contains(&to) {
                continue;
            }

            connected_nodes.insert(from.clone());
            connected_nodes.insert(to.clone());

            result.edges.push(GraphEdge {
                from,
                to,
                label: edge.label,
            });
        }

        // Remove unconnected nodes
        in_group.retain(|id| connected_nodes.contains(id));

        for node in reachable_nodes.into_keys() {
            let Some(id) = self.id(db, node) else {
                continue;
            };

            if !in_group.contains(&id) {
                continue;
            }

            let Some(span) = db.get(node).map(|Syntax(syntax)| db.ast(syntax).span(db)) else {
                continue;
            };

            result.nodes.push(GraphNode {
                id,
                span: span.clone(),
            });
        }

        for (mut nodes, labels) in groups.into_values() {
            nodes.retain(|node| {
                let Some(id) = self.id(db, *node) else {
                    return false;
                };

                in_group.contains(&id)
            });

            if nodes.is_empty() {
                continue;
            }

            let mut labels = labels
                .into_iter()
                .map(|ty| GraphLabel {
                    kind: match ty.tag {
                        TyTag::Function | TyTag::Block => Some(String::from("function")),
                        _ => None,
                    },
                    display: Ty::Constructed(ty).display(db, true),
                })
                .collect::<Vec<_>>();

            let conflict = if labels.is_empty() {
                labels.push(GraphLabel {
                    kind: None,
                    display: String::from("_"),
                });
                true
            } else {
                labels.len() > 1
            };

            result.groups.push(GraphGroup {
                nodes: nodes
                    .into_iter()
                    .filter_map(|node| self.id(db, node))
                    .collect(),
                labels,
                conflict,
            })
        }

        result
    }

    fn id(&self, _db: &Db, node: Node) -> Option<String> {
        Some(format!(
            "node_{}",
            node.id()
                .to_string()
                .replace(|c: char| !c.is_alphanumeric(), "_")
        ))
    }

    fn can_display(&self, db: &Db, node: Node) -> bool {
        (!db.is_hidden(node) || db.contains::<Instantiated>(node))
            && db.get(node).is_none_or(|Defined(definition)| {
                // Allow variables, but not other definitions
                definition.downcast_ref::<VariableDefinition>().is_some()
            })
    }

    fn replacement_for(&self, mut node: Node) -> Node {
        while let Some(replacement) = self.replacements.get(&node) {
            node = *replacement;
        }

        node
    }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct Graph {
    pub groups: Vec<GraphGroup>,
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
}

#[derive(Debug, Clone, Serialize)]
pub struct GraphGroup {
    pub nodes: Vec<String>,
    pub labels: Vec<GraphLabel>,
    pub conflict: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct GraphNode {
    pub id: String,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct GraphEdge {
    pub from: String,
    pub to: String,
    pub label: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct GraphLabel {
    pub kind: Option<String>,
    pub display: String,
}

impl Graph {
    pub fn write_dot(&self, mut w: impl Write) -> fmt::Result {
        writeln!(w, "digraph G {{")?;
        writeln!(w, "  node [shape=box, fontname=monospace]")?;

        for node in &self.nodes {
            writeln!(
                w,
                "  {} [label={}]",
                node.id,
                serde_json::to_string(node.span.source.as_str()).unwrap()
            )?;
        }

        for edge in &self.edges {
            writeln!(
                w,
                "  {} -> {} [label={}]",
                edge.from,
                edge.to,
                serde_json::to_string(&edge.label).unwrap()
            )?;
        }

        writeln!(w, "}}")?;

        Ok(())
    }
}
