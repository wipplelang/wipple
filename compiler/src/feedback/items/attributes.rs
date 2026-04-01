use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{DuplicateAttributeValue, ExtraAttributeValue, MissingAttributeValue},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "extra-attribute-value",
        queries::fact::<ExtraAttributeValue>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" doesn't accept a value.");
            w.write_break();
            w.write_string("Try removing the value from this attribute.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "duplicate-attribute",
        queries::fact::<DuplicateAttributeValue>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" is defined more than once.");
            w.write_break();
            w.write_string("Try removing this attribute.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "missing-attribute-value",
        queries::fact::<MissingAttributeValue>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" is missing a value.");
            w.write_break();
            w.write_string("Try adding a value to this attribute using ");
            w.write_code(":");
            w.write_string(".");
        },
    ));
}
