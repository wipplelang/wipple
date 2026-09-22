use std::{
    collections::{BTreeMap, BTreeSet},
    ops::{Deref, DerefMut},
};
use wipple_core::{
    db::{Db, Node},
    facts::Description,
    render::{Comments, Render, RenderCtx, RenderSegment},
    typecheck::{
        constraints::ConstraintConsequence, instantiate::InstantiatedTypes,
        solver::DirectlyGroupedWith,
    },
    util::get_links,
    visit::{Resolved, definitions::Defined},
};

pub struct FeedbackWriter<'a> {
    ctx: RenderCtx<'a>,
    traces: Vec<(Node, RenderCtx<'a>, Vec<RenderCtx<'a>>)>,
}

impl<'a> FeedbackWriter<'a> {
    pub fn new(filter: &'a dyn Fn(&Db, Node) -> bool, relevant: Vec<Node>) -> Self {
        FeedbackWriter {
            ctx: RenderCtx::new(filter, relevant),
            traces: Default::default(),
        }
    }
}

impl<'a> Deref for FeedbackWriter<'a> {
    type Target = RenderCtx<'a>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

impl<'a> DerefMut for FeedbackWriter<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ctx
    }
}

impl FeedbackWriter<'_> {
    pub fn singular_plural(&mut self, n: usize, singular: &str, plural: &str) {
        if n == 1 {
            self.ctx.string(format!("{n} {singular}"));
        } else {
            self.ctx.string(format!("{n} {plural}"));
        }
    }

    pub fn ordinal(&mut self, n: usize) {
        let suffix = match n % 10 {
            1 => "st",
            2 => "nd",
            3 => "rd",
            _ => "th",
        };

        self.ctx.string(format!("{n}{suffix}"));
    }

    pub fn trace(&mut self, db: &Db, node: Node) {
        #[derive(Debug)]
        struct TraceTree<'a> {
            trace: Option<&'a BTreeMap<Node, Vec<ConstraintConsequence>>>,
            comments: Option<Comments>,
            relevant: Vec<Node>,
            children: BTreeMap<Node, TraceTree<'a>>,
        }

        fn collect_traces<'a>(
            db: &'a Db,
            node: Node,
            seen: &mut BTreeSet<Node>,
        ) -> Option<TraceTree<'a>> {
            if !seen.insert(node) {
                return None;
            }

            // Hide instances/anonymous definitions in traces
            if db
                .get(node)
                .is_some_and(|Defined(definition)| definition.name().is_none())
            {
                return None;
            }

            let trace = db.traces.get(&node);

            let mut relevant = Vec::new();

            let instantiated_nodes = db
                .get(node)
                .map(|InstantiatedTypes(instantiated)| instantiated.values().copied())
                .unwrap_or_default();

            relevant.extend(instantiated_nodes);

            let comments = db
                .get(node)
                .and_then(|Resolved { definitions, .. }| definitions.first().copied())
                .and_then(|definition_node| {
                    relevant.insert(0, definition_node);

                    let Defined(definition) = db.get(definition_node)?;

                    if definition.comments().is_empty() {
                        return None;
                    }

                    let links = get_links(db, definition_node, node);

                    for link in links.values() {
                        relevant.push(link.node);
                        relevant.extend(link.related.iter().copied());
                    }

                    Some(Comments {
                        definition: node,
                        comments: definition.comments().to_vec(),
                        links: links.clone(),
                    })
                })
                .or_else(|| db.get(node).map(|Description(comments)| comments.clone()));

            if let Some(trace) = trace {
                relevant.extend(trace.keys().copied());
            }

            // Don't traverse into other definitions
            if db.get::<Defined>(node).is_none()
                && let Some(DirectlyGroupedWith(nodes)) = db.get(node)
            {
                relevant.extend(nodes);
            }

            let children = relevant
                .iter()
                .filter_map(|&child| Some((child, collect_traces(db, child, seen)?)))
                .collect::<BTreeMap<_, _>>();

            Some(TraceTree {
                trace,
                comments,
                relevant,
                children,
            })
        }

        fn traverse_traces(
            writer: &mut FeedbackWriter<'_>,
            db: &Db,
            node: Node,
            tree: &TraceTree<'_>,
        ) {
            let mut node_ctx = RenderCtx::new(writer.ctx.filter, writer.ctx.relevant.clone());
            node_ctx.with_relevant(&tree.relevant, |ctx| {
                if let Some(comments) = &tree.comments {
                    ctx.comments(db, comments);
                }
            });

            let node_ctx = (!node_ctx.is_empty()).then_some(node_ctx);

            let consequence_ctxs = tree.trace.map_or_default(|trace| {
                trace
                    .values()
                    .flatten()
                    .filter_map(|consequence| {
                        let mut ctx =
                            RenderCtx::new(writer.ctx.filter, writer.ctx.relevant.clone());
                        ctx.with_relevant(&tree.relevant, |ctx| consequence.render_into(db, ctx));
                        (!ctx.is_empty()).then_some(ctx)
                    })
                    .collect::<Vec<_>>()
            });

            if let Some(node_ctx) = node_ctx {
                let source_node = tree
                    .comments
                    .as_ref()
                    .map_or(node, |comments| comments.definition);

                writer
                    .traces
                    .push((source_node, node_ctx, consequence_ctxs));
            }

            for (&node, tree) in &tree.children {
                traverse_traces(writer, db, node, tree);
            }
        }

        if let Some(tree) = collect_traces(db, node, &mut BTreeSet::new()) {
            // TODO: Preserve tree structure instead of flattening here?
            traverse_traces(self, db, node, &tree);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Feedback {
    pub message: String,
    pub traces: Vec<(Node, String, Vec<String>)>,
    pub nodes: BTreeSet<Node>,
}

impl FeedbackWriter<'_> {
    pub fn finish(
        self,
        db: &Db,
        mut render_segment: impl FnMut(&Db, &RenderSegment) -> String,
    ) -> Feedback {
        let (message, mut nodes) = self.ctx.finish(db, &mut render_segment);

        let traces = self
            .traces
            .into_iter()
            .map(|(node, node_ctx, consequence_ctxs)| {
                let (message, trace_nodes) = node_ctx.finish(db, &mut render_segment);
                nodes.extend(trace_nodes);

                let consequences = consequence_ctxs
                    .into_iter()
                    .map(|ctx| {
                        let (message, trace_nodes) = ctx.finish(db, &mut render_segment);
                        nodes.extend(trace_nodes);
                        message
                    })
                    .collect::<Vec<_>>();

                (node, message, consequences)
            })
            .collect::<Vec<_>>();

        Feedback {
            message,
            traces,
            nodes,
        }
    }
}
