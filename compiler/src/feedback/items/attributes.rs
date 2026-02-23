use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{DuplicateAttributeValue, ExtraAttributeValue, MissingAttributeValue},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "extra-attribute-value",
        FeedbackRank::Syntax,
        queries::fact::<ExtraAttributeValue>,
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
        FeedbackRank::Syntax,
        queries::fact::<DuplicateAttributeValue>,
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
        FeedbackRank::Syntax,
        queries::fact::<MissingAttributeValue>,
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
