use crate::{FeedbackCtx, FeedbackRank};
use wipple_queries::fact;
use wipple_syntax::expressions::format_expression::{ExtraFormatInput, MissingFormatInputs};

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("missing-format-inputs")
        .query(fact::<MissingFormatInputs>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, MissingFormatInputs(count)| {
            writer.node(node);
            writer.string(" needs ");
            writer.singular_plural(*count, "more input", "more inputs");
            writer.string(".");
            writer.line_break();
            writer.string("Try adding code after the string.");
        })
        .register();

    ctx.feedback("extra-format-input")
        .query(fact::<ExtraFormatInput>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" isn't used in the format string.");
            writer.line_break();
            writer.string("Try removing this input or add another ");
            writer.code("_");
            writer.string(" placeholder.");
        })
        .register();
}
