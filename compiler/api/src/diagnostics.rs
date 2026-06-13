use crate::{
    CompileResult,
    graph::{Graph, convert_graph},
};
use std::collections::{BTreeMap, BTreeSet};
use wasm_bindgen::prelude::*;
use wipple_core::{
    db::Node,
    default_filter,
    facts::Syntax,
    render::RenderSegment,
    typecheck::{
        groups::{Group, Typed, update_type},
        instantiate::Instantiated,
        ty::Ty,
    },
};
use wipple_feedback::{FeedbackLocation, collect_feedback};

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub locations: Vec<DiagnosticLocation>,
    pub groups: usize,
    pub message: String,
    pub traces: Vec<DiagnosticTrace>,
    pub trace_edges: Vec<DiagnosticTraceEdge>,
    pub graph: Option<Graph>,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticGroup {
    pub labels: Vec<String>,
    pub locations: Vec<DiagnosticLocation>,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticLocation {
    pub start: usize,
    pub end: usize,
    pub group: i32,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct DiagnosticLine {
    pub source: String,
    pub locations: Vec<DiagnosticLocation>,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct DiagnosticTrace {
    pub index: usize,
    pub location: DiagnosticLocation,
    pub message: String,
    pub consequences: Vec<String>,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct DiagnosticTraceEdge {
    pub from: usize,
    pub to: Option<usize>,
}

#[wasm_bindgen]
impl CompileResult {
    #[wasm_bindgen]
    pub fn groups(&self) -> Vec<DiagnosticGroup> {
        let mut groups = BTreeMap::<Vec<Node>, (usize, DiagnosticGroup)>::new();

        for node in self.db.owned_nodes() {
            let Some(span) = self
                .db
                .get(node)
                .map(|Syntax(syntax)| syntax.get(&self.db).span(&self.db))
            else {
                continue;
            };

            let Some(Typed(Some(group))) = self.db.get(node) else {
                continue;
            };

            if !default_filter(&self.db, node) || span.path.as_str() != self.path {
                continue;
            }

            let group_index = groups
                .iter()
                .position(|(nodes, _)| nodes == &group.nodes)
                .unwrap_or(groups.len());

            let location = DiagnosticLocation {
                start: span.start.index,
                end: span.end.index,
                group: group_index as i32,
            };

            if let Some((_, group)) = groups.get_mut(&group.nodes) {
                group.locations.push(location);
            } else {
                let mut labels = group
                    .tys
                    .iter()
                    .map(|ty| {
                        let (ty, _) = update_type(&self.db, &Ty::Constructed(ty.clone()), None);

                        ty.display(&self.db, None, true)
                    })
                    .collect::<Vec<_>>();

                if labels.is_empty() {
                    labels.push(String::from("_"))
                }

                groups.insert(
                    group.nodes.clone(),
                    (
                        group_index,
                        DiagnosticGroup {
                            labels,
                            locations: vec![location],
                        },
                    ),
                );
            }
        }

        groups.into_iter().map(|(_, (_, group))| group).collect()
    }

    #[wasm_bindgen]
    pub fn diagnostics(&self) -> Option<Vec<Diagnostic>> {
        let filter = default_filter;

        let items = collect_feedback(&self.db, filter, |item| {
            filter(&self.db, item.location.primary)
        });

        if items.is_empty() {
            return None;
        }

        let diagnostics = items
            .into_iter()
            .map(|item| {
                let (groups, locations) = self.collect_locations(&item.location);

                let mut mask = BTreeSet::from([item.location.primary]);
                mask.extend(item.location.secondary.iter().copied());

                let feedback = item.display(&self.db, |db, segment| {
                    let (label, node) = match segment {
                        RenderSegment::Node(node) => (segment.plain_text(db), *node),
                        RenderSegment::Link(label, node) => (label.clone(), *node),
                        _ => return segment.markdown(db, false),
                    };

                    mask.insert(node);

                    if let Some(Typed(Some(group))) = db.get(node)
                        && let Some(index) =
                            groups.iter().position(|other| other.nodes == group.nodes)
                    {
                        format!("<code data-group=\"{index}\">{label}</code>")
                    } else {
                        format!("<code>{label}</code>")
                    }
                });

                mask.extend(feedback.nodes);

                let graph = item
                    .show_graph
                    .then(|| convert_graph(self.db.graph.build(&self.db, &mask)));

                Diagnostic {
                    locations,
                    groups: groups.len(),
                    message: feedback.message,
                    traces: feedback
                        .traces
                        .into_iter()
                        .filter_map(|(index, (node, message, consequences))| {
                            let span = self
                                .db
                                .get(node)
                                .map(|Syntax(syntax)| syntax.get(&self.db).span(&self.db))?;

                            let location = DiagnosticLocation {
                                start: span.start.index,
                                end: span.end.index,
                                group: -1,
                            };

                            Some(DiagnosticTrace {
                                index,
                                location,
                                message,
                                consequences,
                            })
                        })
                        .collect(),
                    trace_edges: feedback
                        .trace_edges
                        .into_iter()
                        .map(|(from, to)| DiagnosticTraceEdge { from, to })
                        .collect(),
                    graph,
                }
            })
            .collect::<Vec<_>>();

        Some(diagnostics)
    }
}

impl CompileResult {
    fn collect_locations(
        &self,
        location: &FeedbackLocation,
    ) -> (Vec<Group>, Vec<DiagnosticLocation>) {
        let mut groups = Vec::<Group>::new();
        let mut locations = Vec::<DiagnosticLocation>::new();
        for node in [location.primary]
            .into_iter()
            .chain(location.secondary.iter().copied())
        {
            let Some(span) = self
                .db
                .get(node)
                .map(|Syntax(syntax)| syntax.get(&self.db).span(&self.db))
            else {
                continue;
            };

            if span.path != self.path {
                continue;
            }

            // Hide non-instantiated, hidden nodes
            if self.db.is_hidden(node) && !self.db.contains::<Instantiated>(node) {
                continue;
            }

            let group_index = self
                .db
                .get(node)
                .and_then(|Typed(group)| group.as_ref())
                .map_or(-1, |group| {
                    match groups.iter().position(|other| other.nodes == group.nodes) {
                        Some(index) => index as i32,
                        None => {
                            let index = groups.len();
                            groups.push(group.clone());
                            index as i32
                        }
                    }
                });

            let location = DiagnosticLocation {
                start: span.start.index,
                end: span.end.index,
                group: group_index,
            };

            if !locations.contains(&location) {
                locations.push(location);
            }
        }

        (groups, locations)
    }
}
