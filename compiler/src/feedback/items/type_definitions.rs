use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{DuplicateFieldDefinition, DuplicateVariantDefinition},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "duplicate-field-definition",
        FeedbackRank::Syntax,
        queries::fact::<DuplicateFieldDefinition>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, _)| {
            writer.write_node(node);
            writer.write_string(" is already defined in this type.");
            writer.write_break();
            writer.write_string("Try renaming this field.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "duplicate-variant-definition",
        FeedbackRank::Syntax,
        queries::fact::<DuplicateVariantDefinition>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, _)| {
            writer.write_node(node);
            writer.write_string(" is already defined in this type.");
            writer.write_break();
            writer.write_string("Try renaming this variant.");
        },
    ));
}
