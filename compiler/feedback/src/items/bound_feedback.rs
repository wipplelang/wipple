use crate::{FeedbackCtx, FeedbackLocation, FeedbackRank};
use std::collections::BTreeSet;
use wipple_queries::{error_instances, unresolved_bounds};

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("unresolved-bound")
        .query(unresolved_bounds)
        .rank(|_| FeedbackRank::Bounds)
        .display(|db, writer, node, (bound, traces)| {
            writer.node(node);
            writer.string(" requires the instance ");
            writer.render(db, *bound);
            writer.string(", but this instance isn't defined.");
            writer.line_break();
            writer.string("Double-check that these types are correct.");

            writer.traces(db, traces);
        })
        .register();

    ctx.feedback("error-instance")
        .query(error_instances)
        .rank(|error| {
            if error.is_default {
                FeedbackRank::CustomDefault
            } else {
                FeedbackRank::Custom
            }
        })
        .location(|node, error| {
            let mut secondary = BTreeSet::new();

            for link in error.comments.links.values() {
                secondary.insert(link.node);
                secondary.extend(link.related.iter().copied());
            }

            secondary.extend(error.comments.nodes.iter().copied());

            FeedbackLocation {
                primary: node,
                secondary,
            }
        })
        .show_graph()
        .display(|db, writer, _, error| {
            writer.comments(db, &error.comments);
            writer.traces(db, &error.traces);
        })
        .register();
}
