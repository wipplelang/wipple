use std::{
    collections::{BTreeMap, BTreeSet},
    ops::{Deref, DerefMut},
};
use wipple_core::{
    db::{Db, Node},
    render::{Render, RenderCtx, RenderSegment},
    traces::Traces,
    typecheck::instantiate::Instantiated,
};

pub struct FeedbackWriter<'a> {
    ctx: RenderCtx<'a>,
    traces: Option<Vec<(Node, RenderCtx<'a>, Vec<RenderCtx<'a>>)>>,
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

    pub fn traces(&mut self, db: &Db, traces: &Traces) {
        let mut result = Vec::new();
        let mut seen = BTreeSet::new();
        let mut linked = BTreeSet::new();
        let mut indices = BTreeMap::new();
        for (trace_index, entry) in traces.traces.iter().enumerate() {
            let nodes = entry.trace.clone().nodes(db);

            let Some(&node) = nodes.first() else {
                continue;
            };

            if !self.ctx.filter(db, node) || !seen.insert(node) {
                continue;
            }

            let mut ctx = RenderCtx::new(self.ctx.filter, self.ctx.relevant.clone());
            ctx.with_relevant(&nodes, |ctx| entry.trace.render_into(db, ctx));

            if !ctx.is_empty() {
                let index = indices.len();
                indices.insert(trace_index, index);

                linked.extend(
                    ctx.nodes()
                        .filter(|&node| !db.contains::<Instantiated>(node)),
                );

                result.push((node, ctx, entry));
            }
        }

        let result = result
            .into_iter()
            .filter_map(|(node, ctx, entry)| {
                let nodes = entry.trace.clone().nodes(db);

                let mut consequence_ctxs = Vec::new();
                for consequence in &entry.consequences {
                    if !linked.contains(&consequence.node()) {
                        continue;
                    }

                    let mut consequence_ctx =
                        RenderCtx::new(self.ctx.filter, self.ctx.relevant.clone());
                    consequence_ctx.with_relevant(&nodes, |ctx| consequence.render_into(db, ctx));
                    if !consequence_ctx.is_empty() {
                        consequence_ctxs.push(consequence_ctx);
                    }
                }

                if entry.trace.require_consequences() && consequence_ctxs.is_empty() {
                    return None;
                }

                Some((node, ctx, consequence_ctxs))
            })
            .collect::<Vec<_>>();

        self.traces = Some(result);
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

        let traces = self.traces.unwrap_or_default();

        let traces = traces
            .into_iter()
            .map(|(node, ctx, consequence_ctxs)| {
                let (message, trace_nodes) = ctx.finish(db, &mut render_segment);
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
