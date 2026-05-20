use crate::{FeedbackCtx, FeedbackRank};
use wipple_core::visit::attributes::{
    DuplicateAttribute, ExtraAttributeValue, MissingAttributeValue,
};
use wipple_queries::fact;

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("extra-attribute-value")
        .query(fact::<ExtraAttributeValue>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" doesn't accept a value.");
            writer.line_break();
            writer.string("Try removing the value from this attribute.");
        })
        .register();

    ctx.feedback("duplicate-attribute")
        .query(fact::<DuplicateAttribute>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" is defined more than once.");
            writer.line_break();
            writer.string("Try removing this attribute.");
        })
        .register();

    ctx.feedback("missing-attribute-value")
        .query(fact::<MissingAttributeValue>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" is missing a value.");
            writer.line_break();
            writer.string("Try adding a value to this attribute using ");
            writer.code(":");
            writer.string(".");
        })
        .register();
}
