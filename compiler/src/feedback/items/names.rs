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
        |writer, (_, name)| {
            writer.write_string("Can't find ");
            writer.write_code(name);
            writer.write_string(".");
            writer.write_break();
            writer.write_string("Double-check your spelling.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "ambiguous",
        FeedbackRank::Names,
        queries::ambiguous,
        |(node, _, _)| (node.clone(), BTreeSet::new()),
        |writer, (_, name, definitions)| {
            writer.write_code(name);
            writer.write_string(" could refer to ");

            if !definitions.is_empty() {
                writer.write_list("or", 3, |list| {
                    for node in definitions {
                        list.add(|writer| writer.write_node(node));
                    }
                });
            }

            writer.write_string(".");
            writer.write_break();
            writer.write_string("Rename the extra definitions.");
        },
    ));
}
