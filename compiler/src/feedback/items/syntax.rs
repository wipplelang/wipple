use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    queries,
    syntax::ParseError,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "syntax-error",
        FeedbackRank::Syntax,
        queries::fact::<ParseError>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (_, error)| {
            writer.write_string(&error.message);

            if let Some(committed) = &error.committed {
                writer.write_string(" ");
                writer.write_string(committed);
            }

            writer.write_string(".");

            if let Some(reason) = &error.reason {
                writer.write_break();
                writer.write_string(reason);
            }

            writer.write_break();
            writer.write_string("Check your spelling.");
        },
    ));
}
