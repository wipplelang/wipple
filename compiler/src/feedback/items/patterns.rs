use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{ExtraElement, InvalidOrPattern, InvalidSetPattern},
    queries,
    visit::MissingPatterns,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "nested-or-pattern",
        FeedbackRank::Syntax,
        queries::fact::<InvalidOrPattern>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" cannot be used inside another pattern.");
            w.write_break();
            w.write_code("or");
            w.write_string(" can only be used immediately within a ");
            w.write_code("when");
            w.write_string(" arm.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "nested-set-pattern",
        FeedbackRank::Syntax,
        queries::fact::<InvalidSetPattern>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(" cannot be used inside another pattern.");
            w.write_break();
            w.write_code("set");
            w.write_string(" can only be used immediately before a variable assignment using ");
            w.write_code(":");
            w.write_string(".");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "extra-element",
        FeedbackRank::Syntax,
        queries::fact::<ExtraElement>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, _)| {
            w.write_node(node);
            w.write_string(
                " can't be used here because this code matches against a marker type, not a variant.",
            );
            w.write_break();
            w.write_string("Try removing this element.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "missing-patterns",
        FeedbackRank::Exhaustiveness,
        queries::fact::<MissingPatterns>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (node, MissingPatterns(missing))| {
            w.write_node(node);
            w.write_string(" could be ");

            w.write_list("or", 3, |list| {
                for tree in missing {
                    list.add(|w| {
                        w.write_match_tree(tree);
                    });
                }
            });

            w.write_string(", but these patterns are missing.");
            w.write_break();
            w.write_string("Try adding patterns to cover these cases using ");
            w.write_code("or");
            w.write_string(", ");
            w.write_code("when");
            w.write_string(", or a variable.");
        },
    ));
}
