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
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" is already defined in this type.");
            w.write_break();
            w.write_string("Try renaming this field.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "duplicate-variant-definition",
        FeedbackRank::Syntax,
        queries::fact::<DuplicateVariantDefinition>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" is already defined in this type.");
            w.write_break();
            w.write_string("Try renaming this variant.");
        },
    ));
}
