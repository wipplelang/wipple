use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{DuplicateField, ExtraField, MissingField},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "missing-field",
        FeedbackRank::Syntax,
        queries::fact::<MissingField>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, MissingField(field))| {
            writer.write_node(node);
            writer.write_string(" is missing a pattern for the field ");
            writer.write_code(field);
            writer.write_string(".");
            writer.write_break();
            writer.write_string("Try adding a pattern for this field using ");
            writer.write_code(":");
            writer.write_string(".");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "extra-field",
        FeedbackRank::Syntax,
        queries::fact::<ExtraField>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (_, ExtraField(field))| {
            writer.write_code(field);
            writer.write_string(" isn't a field on this structure.");
            writer.write_break();
            writer.write_string("Double-check your spelling.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "duplicate-field",
        FeedbackRank::Syntax,
        queries::fact::<DuplicateField>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, _)| {
            writer.write_node(node);
            writer.write_string(" is defined more than once.");
            writer.write_break();
            writer.write_string("Try removing this field.");
        },
    ));
}
