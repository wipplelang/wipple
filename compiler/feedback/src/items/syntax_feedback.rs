use crate::{FeedbackCtx, FeedbackRank};
use wipple_queries::{syntax_error, unused_block};

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("syntax-error")
        .query(syntax_error)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, _, error| {
            writer.string(&error.message);

            if let Some(committed) = &error.committed {
                writer.string(" ");
                writer.string(committed);
            }

            writer.string(".");

            if let Some(reason) = &error.reason {
                writer.line_break();
                writer.string(reason);
            }

            writer.line_break();
            writer.string("Check your spelling.");
        })
        .register();

    ctx.feedback("unused-block")
        .query(unused_block)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" is on its own line and will never run.");
            writer.line_break();
            writer.string("Did you mean to put the opening ");
            writer.code("{");
            writer.string(" on the line above?");
        })
        .register();
}
