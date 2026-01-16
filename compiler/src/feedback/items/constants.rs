use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::MissingConstantValue,
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "missing-constant-value",
        FeedbackRank::Syntax,
        queries::fact::<MissingConstantValue>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, _)| {
            writer.write_node(node);
            writer.write_string(" is missing a value.");
            writer.write_break();
            writer.write_string("Try defining a value for this constant using ");
            writer.write_code(":");
            writer.write_string(" on the following line.");
        },
    ));
}
