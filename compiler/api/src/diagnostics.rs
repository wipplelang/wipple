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
    typecheck::{groups::Group, groups::Typed, instantiate::Instantiated},
};
use wipple_feedback::{FeedbackLocation, collect_feedback};
use wipple_syntax::file::File as FileSyntax;

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub locations: Vec<DiagnosticLocation>,
    pub lines: Vec<DiagnosticLine>,
    pub groups: usize,
    pub message: String,
    pub graph: Option<Graph>,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticGroup {
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

#[wasm_bindgen]
impl CompileResult {
    #[wasm_bindgen]
    pub fn groups(&self) -> Vec<DiagnosticGroup> {
        let mut groups = BTreeMap::<Vec<Node>, (usize, Vec<DiagnosticLocation>)>::new();

        for node in self.db.owned_nodes() {
            let Some(span) = self.db.get(node).map(|Syntax(syntax)| syntax.span()) else {
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

            if let Some((_, locations)) = groups.get_mut(&group.nodes) {
                locations.push(location);
            } else {
                groups.insert(group.nodes.clone(), (group_index, vec![location]));
            }
        }

        groups
            .into_iter()
            .map(|(_, (_, locations))| DiagnosticGroup { locations })
            .collect()
    }

    #[wasm_bindgen]
    pub fn diagnostics(&self) -> Option<Vec<Diagnostic>> {
        let items = collect_feedback(&self.db, |item| {
            default_filter(&self.db, item.location.primary)
        });

        if items.is_empty() {
            return None;
        }

        let diagnostics = items
            .into_iter()
            .map(|item| {
                let (groups, locations, lines) = self.collect_lines(&item.location);

                let mut mask = BTreeSet::from([item.location.primary]);
                mask.extend(item.location.secondary.iter().copied());

                let (message, nodes) = item.display(&self.db, |db, segment| {
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

                mask.extend(nodes);

                let graph = item
                    .show_graph
                    .then(|| convert_graph(self.db.graph.build(&self.db, &mask)));

                Diagnostic {
                    locations,
                    lines,
                    groups: groups.len(),
                    message,
                    graph,
                }
            })
            .collect::<Vec<_>>();

        Some(diagnostics)
    }
}

impl CompileResult {
    fn collect_lines(
        &self,
        location: &FeedbackLocation,
    ) -> (Vec<Group>, Vec<DiagnosticLocation>, Vec<DiagnosticLine>) {
        #[derive(Debug, Clone)]
        struct LineInfo {
            start: usize,
            end: usize,
            offset: usize,
            nodes: BTreeSet<Node>,
        }

        let mut groups = Vec::<Group>::new();
        let mut locations = Vec::<DiagnosticLocation>::new();
        let mut lines = Vec::<(DiagnosticLine, LineInfo)>::new();
        for node in [location.primary]
            .into_iter()
            .chain(location.secondary.iter().copied())
        {
            let Some(span) = self.db.get::<Syntax>(node).map(|syntax| syntax.span()) else {
                continue;
            };

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

            if span.path == self.path {
                let location = DiagnosticLocation {
                    start: span.start.index,
                    end: span.end.index,
                    group: group_index,
                };

                if !locations.contains(&location) {
                    locations.push(location);
                }

                continue;
            }

            let convert_offset = |offset: usize| DiagnosticLocation {
                start: span.start.column - 1 + offset,
                end: span.end.column - 1 + offset,
                group: group_index,
            };

            if let Some(index) = lines
                .iter()
                .position(|(_, line)| span.start.line >= line.start && span.end.line <= line.end)
            {
                lines[index].1.nodes.insert(node);

                let location = convert_offset(lines[index].1.offset);

                if !lines[index].0.locations.contains(&location) {
                    lines[index].0.locations.push(location);
                }

                continue;
            }

            let Some(source) = self.db.owned_nodes().find_map(|file_node| {
                let syntax = self.db.get::<Syntax>(file_node)?;
                syntax.0.downcast_ref::<FileSyntax>()?;

                let file_span = syntax.span();
                (file_span.path == span.path).then_some(file_span.source.as_str())
            }) else {
                continue;
            };

            // Seek to the `span` location
            let mut offset = 0;
            let source = source
                .lines()
                .enumerate()
                .skip(span.start.line - 1)
                .take(span.end.line - span.start.line + 1)
                .inspect(|(index, line)| {
                    if *index < span.start.line {
                        offset += line.len() + 1
                    }
                })
                .map(|(_, line)| line)
                .collect::<Vec<_>>()
                .join("\n");

            lines.push((
                DiagnosticLine {
                    source,
                    locations: vec![convert_offset(offset)],
                },
                LineInfo {
                    start: span.start.line,
                    end: span.end.line,
                    offset,
                    nodes: BTreeSet::from([node]),
                },
            ));
        }

        // Delete lines that have no groups
        lines.retain(|(line, _)| !line.locations.iter().all(|location| location.group == -1));

        (
            groups,
            locations,
            lines.into_iter().map(|(line, _)| line).collect(),
        )
    }
}
