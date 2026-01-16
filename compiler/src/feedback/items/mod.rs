mod attributes;
mod bounds;
mod constants;
mod format;
mod functions;
mod instances;
mod names;
mod patterns;
mod placeholders;
mod structures;
mod syntax;
mod type_definitions;
mod types;

use crate::feedback::FeedbackCtx;

pub fn register(ctx: &mut FeedbackCtx) {
    attributes::register(ctx);
    bounds::register(ctx);
    constants::register(ctx);
    format::register(ctx);
    functions::register(ctx);
    instances::register(ctx);
    names::register(ctx);
    patterns::register(ctx);
    placeholders::register(ctx);
    structures::register(ctx);
    syntax::register(ctx);
    type_definitions::register(ctx);
    types::register(ctx);
}
