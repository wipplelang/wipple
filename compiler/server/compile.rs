use crate::{File, InputMetadata, compile};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::{collections::BTreeSet, sync::Arc};
use wipple::{
    codegen::{self, CodegenCtx},
    database::{Db, NodeRef, RenderConfig},
    feedback::{FeedbackItem, collect_feedback},
    nodes::FileNode,
    typecheck::{Group, Typed},
};

const INPUT_PATH: &str = "input";
const PRELUDE: &str = include_str!("../runtime/runtime.js");

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Request {
    #[serde(flatten)]
    metadata: InputMetadata,
    code: String,
}

pub async fn handle(request: Request) -> anyhow::Result<serde_json::Value> {
    let (mut db, files) = compile(
        &[File {
            path: String::from(INPUT_PATH),
            code: request.code,
        }],
        request.metadata.library.as_deref(),
    )
    .await?;

    let feedback = collect_feedback(&db, |_| true);

    if !feedback.is_empty() {
        return Ok(json!({
            "diagnostics": feedback
                .into_iter()
                .map(|item| convert_feedback(&mut db, item))
                .collect::<Vec<_>>(),
        }));
    }

    let codegen = CodegenCtx::new(
        &mut db,
        "index.js",
        codegen::Options {
            prelude: PRELUDE,
            module: true,
        },
    );

    colored::control::set_override(false);
    let script = codegen.to_string(&files)?;
    colored::control::unset_override();

    Ok(json!({ "executable": script }))
}

fn convert_feedback(db: &mut Db, item: FeedbackItem) -> serde_json::Value {
    let (groups, locations, lines) = collect_lines(db, &item.location);

    let group_count = groups.len();

    db.render_with(RenderConfig::new(move |db, value, f| {
        if let Some(node) = value.link()
            && let Some(Typed { group: Some(group) }) = db.get(node)
            && let Some(index) = groups.iter().position(|other| Arc::ptr_eq(&group, other))
        {
            write!(f, "<code data-group=\"{}\">", index)?;
            value.write(f, db)?;
            write!(f, "</code>")?;

            return Ok(());
        }

        write!(f, "`")?;
        value.write(f, db)?;
        write!(f, "`")?;

        Ok(())
    }));

    let mut message = String::new();
    (item.write)(db, &mut message);

    let graph = item
        .show_graph
        .then(|| db.filtered_graph([item.location.0.clone()]).serialize(db));

    json!({
        "locations": locations,
        "lines": lines,
        "groups": group_count,
        "message": message,
        "graph": graph,
    })
}

fn collect_lines(
    db: &Db,
    location: &(NodeRef, BTreeSet<NodeRef>),
) -> (Vec<Arc<Group>>, serde_json::Value, serde_json::Value) {
    #[derive(PartialEq, Eq, Serialize)]
    struct Location {
        start: usize,
        end: usize,
        group: i32,
    }

    #[derive(Serialize)]
    struct Line {
        source: String,
        locations: Vec<Location>,
        #[serde(skip)]
        start: usize,
        #[serde(skip)]
        end: usize,
        #[serde(skip)]
        offset: usize,
        #[serde(skip)]
        nodes: BTreeSet<NodeRef>,
    }

    let mut groups = Vec::<Arc<Group>>::new();
    let mut locations = Vec::<Location>::new();
    let mut lines = Vec::<Line>::new();
    for node in [&location.0].into_iter().chain(&location.1) {
        let mut group_index = -1;
        if let Some(Typed { group: Some(group) }) = db.get(node) {
            if let Some(index) = groups.iter().position(|other| Arc::ptr_eq(&group, other)) {
                group_index = index as i32;
            } else {
                group_index = groups.len() as i32;
                groups.push(group);
            }
        }

        let span = db.span(node);

        if span.path == INPUT_PATH {
            let location = Location {
                start: span.start.index,
                end: span.end.index,
                group: group_index,
            };

            if !locations.contains(&location) {
                locations.push(location);
            }

            continue;
        }

        let convert_offset = |offset: usize| Location {
            start: span.start.column - 1 + offset,
            end: span.end.column - 1 + offset,
            group: group_index,
        };

        if let Some(existing) = lines
            .iter_mut()
            .find(|line| span.start.line >= line.start && span.end.line <= line.end)
        {
            existing.nodes.insert(node.clone());

            let location = convert_offset(existing.offset);
            if !existing.locations.contains(&location) {
                existing.locations.push(location);
            }

            continue;
        }

        let Some(source) = db.iter_nodes().find_map(|file_node| {
            file_node.downcast_ref::<FileNode>()?;

            let file_span = db.span(&file_node);

            if file_span.path != span.path {
                return None;
            }

            Some(file_span.source)
        }) else {
            continue;
        };

        // Seek to the `span` location and trim comments
        let mut offset = 0;
        let source = source
            .lines()
            .skip(span.start.line - 1)
            .take(span.end.line - span.start.line + 1)
            .skip_while(|line| {
                if !line.trim_start().starts_with("--") {
                    return false;
                }

                offset += line.len() + 1;

                true
            })
            .collect::<Vec<_>>()
            .join("\n");

        lines.push(Line {
            source,
            locations: vec![convert_offset(offset)],
            start: span.start.line,
            end: span.end.line,
            offset,
            nodes: BTreeSet::from([node.clone()]),
        });
    }

    // Delete lines that have no groups
    lines.retain(|line| line.locations.iter().any(|location| location.group != -1));

    (
        groups,
        serde_json::to_value(locations).unwrap(),
        serde_json::to_value(lines).unwrap(),
    )
}
