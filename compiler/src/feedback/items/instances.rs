use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{ExtraInstanceValue, MissingInstanceValue},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "missing-instance-value",
        FeedbackRank::Syntax,
        queries::fact::<MissingInstanceValue>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, _)| {
            writer.write_node(node);
            writer.write_string(" is missing a value.");
            writer.write_break();
            writer.write_string("Try adding a value for this instance using ");
            writer.write_code(":");
            writer.write_string(".");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "extra-instance-value",
        FeedbackRank::Syntax,
        queries::fact::<ExtraInstanceValue>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, _| {
            writer.write_string("This instance doesn't need a value because it is marked with ");
            writer.write_code("[error]");
            writer.write_string(".");
            writer.write_break();
            writer.write_string("Remove this code.");
        },
    ));
}
