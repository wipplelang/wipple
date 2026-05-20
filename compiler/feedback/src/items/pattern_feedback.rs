use crate::{FeedbackCtx, FeedbackRank};
use wipple_core::visit::exhaustiveness::{MatchTree, MissingPatterns};
use wipple_queries::fact;
use wipple_syntax::patterns::{ExtraElement, InvalidOrPattern, InvalidSetPattern};

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("nested-or-pattern")
        .query(fact::<InvalidOrPattern>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" cannot be used inside another pattern.");
            writer.line_break();
            writer.code("or");
            writer.string(" can only be used immediately within a ");
            writer.code("when");
            writer.string(" arm.");
        })
        .register();

    ctx.feedback("nested-set-pattern")
        .query(fact::<InvalidSetPattern>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, reason| match reason {
            InvalidSetPattern::Nested => {
                writer.node(node);
                writer.string(" cannot be used inside another pattern.");
                writer.line_break();
                writer.code("set");
                writer.string(" can only be used immediately before a variable assignment using ");
                writer.code(":");
                writer.string(".");
            }
            InvalidSetPattern::Immutable(variable) => {
                writer.node(*variable);
                writer.string(" cannot be changed.");
                writer.line_break();
                writer.string("Try copying ");
                writer.node(*variable);
                writer.string(" into a new variable before using ");
                writer.code("set");
                writer.string(".");
            }
        })
        .register();

    ctx.feedback("extra-element")
        .query(fact::<ExtraElement>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(
                " can't be used here because this code matches against a marker type, not a variant.",
            );
            writer.line_break();
            writer.string("Try removing this element.");
        })
        .register();

    ctx.feedback("missing-patterns")
        .query(fact::<MissingPatterns>)
        .rank(|_| FeedbackRank::Exhaustiveness)
        .display(|db, writer, node, missing| {
            if matches!(missing.0.as_slice(), [MatchTree::Wildcard]) {
                writer.node(node);
                writer.string(" could be a different possible value that isn't covered here.");
                writer.line_break();
                writer.string("Try assigning it to a variable or ");
                writer.code("_");
                writer.string(".");
            } else {
                writer.node(node);
                writer.string(" could be ");
                writer.write_list("or", |list| {
                    for tree in &missing.0 {
                        list.add(move |writer| writer.render(db, tree));
                    }
                });
                writer.string(", but these patterns are missing.");
                writer.line_break();
                writer.string("Try adding patterns to cover these cases using ");
                writer.code("or");
                writer.string(", ");
                writer.code("when");
                writer.string(", or a variable.");
            }
        })
        .register();
}
