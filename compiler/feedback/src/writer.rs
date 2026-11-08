use std::{
    collections::BTreeSet,
    ops::{Deref, DerefMut},
};
use wipple_core::{
    db::{Db, Node},
    render::{RenderCtx, RenderOptions, RenderSegment},
    typecheck::groups::Prefer,
};
use wipple_queries::{Trace, TraceEntry};

pub struct FeedbackWriter<'a> {
    ctx: RenderCtx<'a>,
    traces: Vec<FeedbackWriterTraceEntry<'a>>,
}

impl<'a> FeedbackWriter<'a> {
    pub fn new(filter: impl Fn(&Db, Node) -> bool + 'a) -> Self {
        FeedbackWriter {
            ctx: RenderCtx::new(filter),
            traces: Default::default(),
        }
    }

    pub fn with_options(mut self, options: RenderOptions) -> Self {
        self.ctx.options = options;
        self
    }

    pub fn inherit(writer: &FeedbackWriter<'a>) -> Self {
        FeedbackWriter {
            ctx: RenderCtx::inherit(&writer.ctx),
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
}

pub struct FeedbackWriterTraceEntry<'a> {
    pub node: Node,
    pub is_primary: bool,
    pub node_ctx: RenderCtx<'a>,
    pub consequence_ctxs: Vec<RenderCtx<'a>>,
}

impl<'a> FeedbackWriter<'a> {
    pub fn extend_trace(&mut self, db: &Db, trace: &Trace) {
        for entry in trace.0.iter() {
            let entry = self.trace_entry(db, entry);
            self.traces.push(entry);
        }
    }

    pub fn trace_entry(&mut self, db: &Db, entry: &TraceEntry) -> FeedbackWriterTraceEntry<'a> {
        let mut node_ctx = RenderCtx::inherit(&self.ctx);
        node_ctx.with_relevant(&entry.relevant, Prefer::DirectType, |ctx| {
            ctx.comments(db, &entry.comments);
        });

        let consequence_ctxs = entry
            .consequences
            .iter()
            .filter_map(|consequence| {
                let mut ctx = RenderCtx::inherit(&self.ctx);
                ctx.with_relevant(&entry.relevant, Prefer::DirectType, |ctx| {
                    if consequence.should_render(db, ctx) {
                        ctx.list("and", |ctx, list| {
                            consequence.render_into_list(db, ctx, list);
                        });
                        ctx.string(".");
                    }
                });
                (!ctx.is_empty()).then_some(ctx)
            })
            .collect::<Vec<_>>();

        FeedbackWriterTraceEntry {
            node: entry.comments.node,
            is_primary: entry.is_primary,
            node_ctx,
            consequence_ctxs,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Feedback {
    pub message: String,
    pub traces: Vec<FeedbackTrace>,
    pub nodes: BTreeSet<Node>,
}

#[derive(Debug, Clone)]
pub struct FeedbackTrace {
    pub node: Node,
    pub is_primary: bool,
    pub message: String,
    pub consequences: Vec<String>,
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
            .map(|trace| {
                let (message, trace_nodes) = trace.node_ctx.finish(db, &mut render_segment);
                nodes.extend(trace_nodes);

                let consequences = trace
                    .consequence_ctxs
                    .into_iter()
                    .map(|ctx| {
                        let (message, trace_nodes) = ctx.finish(db, &mut render_segment);
                        nodes.extend(trace_nodes);
                        message
                    })
                    .collect::<Vec<_>>();

                FeedbackTrace {
                    node: trace.node,
                    is_primary: trace.is_primary,
                    message,
                    consequences,
                }
            })
            .collect::<Vec<_>>();

        Feedback {
            message,
            traces,
            nodes,
        }
    }
}
