use crate::{FeedbackCtx, FeedbackRank};
use wipple_queries::fact;
use wipple_syntax::statements::type_definition::{
    DuplicateFieldDefinition, DuplicateVariantDefinition,
};

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("duplicate-field-definition")
        .query(fact::<DuplicateFieldDefinition>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" is already defined in this type.");
            writer.line_break();
            writer.string("Try renaming this field.");
        })
        .register();

    ctx.feedback("duplicate-variant-definition")
        .query(fact::<DuplicateVariantDefinition>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" is already defined in this type.");
            writer.line_break();
            writer.string("Try renaming this variant.");
        })
        .register();
}
