use std::{
    collections::BTreeSet,
    ops::{Deref, DerefMut},
};
use wipple_core::{
    db::{Db, Node},
    render::{RenderCtx, RenderSegment},
    typecheck::constraints::ConstraintTrace,
};

#[derive(Default)]
pub struct FeedbackWriter(RenderCtx);

impl Deref for FeedbackWriter {
    type Target = RenderCtx;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FeedbackWriter {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FeedbackWriter {
    pub fn singular_plural(&mut self, n: usize, singular: &str, plural: &str) {
        if n == 1 {
            self.0.string(format!("{n} {singular}"));
        } else {
            self.0.string(format!("{n} {plural}"));
        }
    }

    pub fn ordinal(&mut self, n: usize) {
        let suffix = match n % 10 {
            1 => "st",
            2 => "nd",
            3 => "rd",
            _ => "th",
        };

        self.0.string(format!("{n}{suffix}"));
    }

    pub fn traces(&mut self, db: &Db, traces: &[Box<dyn ConstraintTrace>]) {
        let mut seen = BTreeSet::new();
        for trace in traces {
            if let Some(&node) = trace.clone().nodes(db).first()
                && !seen.insert(node)
            {
                continue;
            }

            let mut ctx = RenderCtx::default();
            trace.render_into(db, &mut ctx);

            if !ctx.is_empty() {
                self.line_break();
                self.string("-  ");
                self.0.extend([ctx]);
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
        self.0.finish(db, render_segment)
    }
}
