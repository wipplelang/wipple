use crate::{FeedbackCtx, FeedbackLocation, FeedbackRank};
use std::collections::BTreeSet;
use wipple_queries::{ambiguous, unresolved};

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("unresolved")
        .query(unresolved)
        .rank(|_| FeedbackRank::Names)
        .location(|node, (_, suggestion)| FeedbackLocation {
            primary: node,
            secondary: suggestion
                .as_ref()
                .map(|(node, _)| BTreeSet::from([*node]))
                .unwrap_or_default(),
        })
        .display(|_db, writer, _, (name, suggestion)| {
            writer.string("Can't find ");
            writer.code(name.to_string());
            writer.string(".");
            writer.line_break();

            if let Some((node, name)) = suggestion {
                writer.string("Did you mean ");
                writer.link(name.to_string(), *node);
                writer.string("?");
            } else {
                writer.string("Double-check your spelling.");
            }
        })
        .register();

    ctx.feedback("ambiguous")
        .query(ambiguous)
        .rank(|_| FeedbackRank::Names)
        .display(|_db, writer, _, (_, name, definitions)| {
            writer.code(name.to_string());
            writer.string(" could refer to ");
            writer.list("or", |list| {
                for &definition in *definitions {
                    list.add(move |writer| writer.node(definition));
                }
            });
            writer.string(".");
            writer.line_break();
            writer.string("Rename the extra definitions.");
        })
        .register();
}
