use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    queries,
    syntax::ParseError,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "syntax-error",
        queries::fact::<ParseError>,
        |_| FeedbackRank::Syntax,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (_, error)| {
            w.write_string(&error.message);

            if let Some(committed) = &error.committed {
                w.write_string(" ");
                w.write_string(committed);
            }

            w.write_string(".");

            if let Some(reason) = &error.reason {
                w.write_break();
                w.write_string(reason);
            }

            w.write_break();
            w.write_string("Check your spelling.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "unused-block",
        queries::unused_block,
        |_| FeedbackRank::Syntax,
        |node| (node.clone(), BTreeSet::new()),
        |w, node| {
            w.write_node(node);
            w.write_string(" is on its own line and will never run.");
            w.write_break();
            w.write_string("Did you mean to put the opening ");
            w.write_code("{");
            w.write_string(" on the line above?");
        },
    ));
}
