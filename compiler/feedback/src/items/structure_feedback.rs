use crate::{FeedbackCtx, FeedbackRank};
use wipple_core::typecheck::ty::Ty;
use wipple_queries::{fact, unknown_field};
use wipple_syntax::expressions::structure_expression::{DuplicateField, ExtraField, MissingField};

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("missing-field")
        .query(fact::<MissingField>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, MissingField(name)| {
            writer.node(node);
            writer.string(" is missing a pattern for the field ");
            writer.code(name);
            writer.string(".");
            writer.line_break();
            writer.string("Try adding a pattern for this field using ");
            writer.code(":");
            writer.string(".");
        })
        .register();

    ctx.feedback("extra-field")
        .query(fact::<ExtraField>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, _, extra| {
            writer.code(&extra.0);
            writer.string(" isn't a field on this structure.");
            writer.line_break();
            writer.string("Double-check your spelling.");
        })
        .register();

    ctx.feedback("duplicate-field")
        .query(fact::<DuplicateField>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" is defined more than once.");
            writer.line_break();
            writer.string("Try removing this field.");
        })
        .register();

    ctx.feedback("unknown-field")
        .query(unknown_field)
        .rank(|_| FeedbackRank::Bounds)
        .display(|db, writer, _, (field, value)| {
            writer.node(*value);
            writer.string(" does not have a field named ");
            writer.code(field.as_str());
            writer.string(".");
            writer.line_break();
            writer.string("Double-check the definition of ");
            writer.ty(db, &Ty::Node(*value), Some(*value), true);
            writer.string(".");
        })
        .register();
}
