use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{ExtraElement, InvalidSetPattern},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "nested-set-pattern",
        FeedbackRank::Syntax,
        queries::fact::<InvalidSetPattern>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, _)| {
            writer.write_node(node);
            writer.write_string(" cannot be used inside another pattern.");
            writer.write_break();
            writer.write_code("set");
            writer
                .write_string(" can only be used immediately before a variable assignment using ");
            writer.write_code(":");
            writer.write_string(".");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "extra-element",
        FeedbackRank::Syntax,
        queries::fact::<ExtraElement>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, _)| {
            writer.write_node(node);
            writer.write_string(
                " can't be used here because this code matches against a marker type, not a variant.",
            );
            writer.write_break();
            writer.write_string("Try removing this element.");
        },
    ));
}
