use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::MissingConstantValue,
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "missing-constant-value",
        queries::fact::<MissingConstantValue>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" is missing a value.");
            w.write_break();
            w.write_string("Try defining a value for this constant using ");
            w.write_code(":");
            w.write_string(" on the following line.");
        },
    ));
}
