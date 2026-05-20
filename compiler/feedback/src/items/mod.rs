mod attribute_feedback;
mod bound_feedback;
mod constant_feedback;
mod format_feedback;
mod instance_feedback;
mod name_feedback;
mod pattern_feedback;
mod placeholder_feedback;
mod structure_feedback;
mod syntax_feedback;
mod type_definition_feedback;
mod type_feedback;

use crate::FeedbackCtx;

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    attribute_feedback::register(ctx);
    bound_feedback::register(ctx);
    constant_feedback::register(ctx);
    format_feedback::register(ctx);
    instance_feedback::register(ctx);
    name_feedback::register(ctx);
    pattern_feedback::register(ctx);
    placeholder_feedback::register(ctx);
    structure_feedback::register(ctx);
    syntax_feedback::register(ctx);
    type_definition_feedback::register(ctx);
    type_feedback::register(ctx);
}
