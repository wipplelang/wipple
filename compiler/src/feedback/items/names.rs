use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "unresolved",
        FeedbackRank::Names,
        |ctx, f| {
            queries::unresolved(ctx, &mut |name| f((ctx.node.clone(), name)));
        },
        |(node, _)| (node.clone(), BTreeSet::new()),
        |w, (_, name)| {
            w.write_string("Can't find ");
            w.write_code(name);
            w.write_string(".");
            w.write_break();
            w.write_string("Double-check your spelling.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "ambiguous",
        FeedbackRank::Names,
        queries::ambiguous,
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
