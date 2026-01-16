use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{ExtraFormatInput, MissingFormatInputs},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "missing-format-inputs",
        FeedbackRank::Syntax,
        queries::fact::<MissingFormatInputs>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, MissingFormatInputs(count))| {
            writer.write_node(node);
            writer.write_string(" needs ");
            writer.write_number(*count, "more input", "more inputs");
            writer.write_string(".");
            writer.write_break();
            writer.write_string("Try adding code after the string.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "extra-format-input",
        FeedbackRank::Syntax,
        queries::fact::<ExtraFormatInput>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, _)| {
            writer.write_node(node);
            writer.write_string(" isn't used in the format string.");
            writer.write_break();
            writer.write_string("Try removing this input or add another ");
            writer.write_code("_");
            writer.write_string(" placeholder.");
        },
    ));
}
