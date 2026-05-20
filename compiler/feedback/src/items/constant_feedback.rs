use crate::{FeedbackCtx, FeedbackRank};
use wipple_queries::missing_constant_value;

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("missing-constant-value")
        .query(missing_constant_value)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" is missing a value.");
            writer.line_break();
            writer.string("Try defining a value for this constant using ");
            writer.code(":");
            writer.string(" on the following line.");
        })
        .register();
}
