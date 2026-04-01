use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{ExtraFormatInput, MissingFormatInputs},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "missing-format-inputs",
        queries::fact::<MissingFormatInputs>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, MissingFormatInputs(count))| {
            w.write_node(node);
            w.write_string(" needs ");
            w.write_number(*count, "more input", "more inputs");
            w.write_string(".");
            w.write_break();
            w.write_string("Try adding code after the string.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "extra-format-input",
        queries::fact::<ExtraFormatInput>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" isn't used in the format string.");
            w.write_break();
            w.write_string("Try removing this input or add another ");
            w.write_code("_");
            w.write_string(" placeholder.");
        },
    ));
}
