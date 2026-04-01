use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{DuplicateField, ExtraField, MissingField},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "missing-field",
        queries::fact::<MissingField>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, MissingField(field))| {
            w.write_node(node);
            w.write_string(" is missing a pattern for the field ");
            w.write_code(field);
            w.write_string(".");
            w.write_break();
            w.write_string("Try adding a pattern for this field using ");
            w.write_code(":");
            w.write_string(".");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "extra-field",
        queries::fact::<ExtraField>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (_, ExtraField(field))| {
            w.write_code(field);
            w.write_string(" isn't a field on this structure.");
            w.write_break();
            w.write_string("Double-check your spelling.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "duplicate-field",
        queries::fact::<DuplicateField>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" is defined more than once.");
            w.write_break();
            w.write_string("Try removing this field.");
        },
    ));
}
