use crate::{FeedbackCtx, FeedbackRank};
use wipple_queries::fact;
use wipple_syntax::statements::instance_definition::{ExtraInstanceValue, MissingInstanceValue};

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("missing-instance-value")
        .query(fact::<MissingInstanceValue>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" is missing a value.");
            writer.line_break();
            writer.string("Try adding a value for this instance using ");
            writer.code(":");
            writer.string(".");
        })
        .register();

    ctx.feedback("extra-instance-value")
        .query(fact::<ExtraInstanceValue>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, _, _| {
            writer.string("This instance doesn't need a value because it is marked with ");
            writer.code("error");
            writer.string(".");
            writer.line_break();
            writer.string("Remove this code.");
        })
        .register();
}
