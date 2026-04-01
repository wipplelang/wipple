use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "unresolved",
        |ctx, f| {
            queries::unresolved(ctx, &mut |(name, suggestion)| {
                f((ctx.node.clone(), name, suggestion))
            });
        },
        |_| FeedbackRank::Names,
        |(node, _, suggestion)| (node.clone(), BTreeSet::from_iter(suggestion.clone())),
        |w, (_, name, suggestion)| {
            w.write_string("Can't find ");
            w.write_code(name);
            w.write_string(".");
            w.write_break();

            if let Some(suggestion) = suggestion {
                w.write_string("Did you mean ");
                w.write_node(suggestion);
                w.write_string("?");
            } else {
                w.write_string("Double-check your spelling.");
            }
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "ambiguous",
        queries::ambiguous,
        |_| FeedbackRank::Names,
        |(node, _, _)| (node.clone(), BTreeSet::new()),
        |w, (_, name, definitions)| {
            w.write_code(name);
            w.write_string(" could refer to ");

            if !definitions.is_empty() {
                w.write_list("or", 3, |list| {
                    for node in definitions {
                        list.add(|w| w.write_node(node));
                    }
                });
            }

            w.write_string(".");
            w.write_break();
            w.write_string("Rename the extra definitions.");
        },
    ));
}
