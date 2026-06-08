use std::{
    collections::BTreeSet,
    ops::{Deref, DerefMut},
    sync::Arc,
};
use wipple_core::{
    db::{Db, Node},
    render::{RenderCtx, RenderSegment},
    typecheck::constraints::ConstraintTrace,
};

pub struct FeedbackWriter {
    ctx: RenderCtx,
    filter: Arc<dyn Fn(&Db, Node) -> bool>,
}

impl FeedbackWriter {
    pub fn new(filter: Arc<dyn Fn(&Db, Node) -> bool>) -> Self {
        FeedbackWriter {
            ctx: RenderCtx::default(),
            filter,
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
    pub fn filter(&self, db: &Db, node: Node) -> bool {
        (self.filter)(db, node)
    }

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

    pub fn traces(&mut self, db: &Db, traces: &[Box<dyn ConstraintTrace>]) {
        let mut seen = BTreeSet::new();
        for trace in traces {
            if let Some(&node) = trace.clone().nodes(db).first()
                && (!self.filter(db, node) || !seen.insert(node))
            {
                continue;
            }

            let mut ctx = RenderCtx::default();
            trace.render_into(db, &mut ctx);

            if !ctx.is_empty() {
                self.line_break();
                self.string("-  ");
                self.ctx.extend([ctx]);
            }
        }
    }
}

impl FeedbackWriter {
    pub fn finish(
        self,
        db: &Db,
        render_segment: impl FnMut(&Db, &RenderSegment) -> String,
    ) -> (String, BTreeSet<Node>) {
        self.ctx.finish(db, render_segment)
    }
}
