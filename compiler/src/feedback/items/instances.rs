use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{ExtraInstanceValue, MissingInstanceValue},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "missing-instance-value",
        queries::fact::<MissingInstanceValue>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" is missing a value.");
            w.write_break();
            w.write_string("Try adding a value for this instance using ");
            w.write_code(":");
            w.write_string(".");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "extra-instance-value",
        queries::fact::<ExtraInstanceValue>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, _| {
            w.write_string("This instance doesn't need a value because it is marked with ");
            w.write_code("[error]");
            w.write_string(".");
            w.write_break();
            w.write_string("Remove this code.");
        },
    ));
}
