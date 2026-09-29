use std::{
    collections::BTreeSet,
    ops::{Deref, DerefMut},
};
use wipple_core::{
    db::{Db, Node},
    render::{RenderCtx, RenderOptions, RenderSegment},
    typecheck::groups::Prefer,
};
use wipple_queries::Trace;

#[derive(Debug, Default)]
pub struct FeedbackWriter {
    ctx: RenderCtx,
    traces: Vec<FeedbackWriterTrace>,
}

impl FeedbackWriter {
    pub fn with_options(options: RenderOptions) -> Self {
        FeedbackWriter {
            ctx: RenderCtx::with_options(options),
            traces: Default::default(),
        }
    }
}

impl Deref for FeedbackWriter {
    type Target = RenderCtx;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

impl DerefMut for FeedbackWriter {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ctx
    }
}

impl FeedbackWriter {
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

#[derive(Debug)]
struct FeedbackWriterTrace {
    node: Node,
    is_primary: bool,
    node_ctx: RenderCtx,
    consequence_ctxs: Vec<RenderCtx>,
}

impl FeedbackWriter {
    pub fn extend_trace(&mut self, db: &Db, trace: &Trace) {
        for entry in &trace.0 {
            let mut node_ctx = RenderCtx::with_options(self.options.clone());
            node_ctx.with_relevant(&entry.relevant, Prefer::DirectType, |ctx| {
                ctx.comments(db, &entry.comments);
            });

            let consequence_ctxs = entry
                .consequences
                .iter()
                .filter_map(|consequence| {
                    let mut ctx = RenderCtx::with_options(self.options.clone());
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

            self.traces.push(FeedbackWriterTrace {
                node: entry.comments.node,
                is_primary: entry.is_primary,
                node_ctx,
                consequence_ctxs,
            });
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

impl FeedbackWriter {
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
