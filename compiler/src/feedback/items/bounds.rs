use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    queries::{self, QueriedComments},
    typecheck::{Bound, Constraint, Group, Typed},
};
use std::{collections::BTreeSet, sync::Arc};

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(RegisteredFeedback::new(
        "unresolved-bound",
        FeedbackRank::Bounds,
        queries::unresolved_bound,
        |bound| (bound.source_node.clone(), BTreeSet::new()),
        |writer, bound| {
            writer.write_node(&bound.source_node);
            writer.write_string(" requires the instance ");
            writer.write_bound(bound);
            writer.write_string(", but this instance isn't defined.");
            writer.write_break();
            writer.write_string("Double-check that these types are correct.");
        },
    ));

    #[derive(Debug)]
    struct ErrorInstanceData {
        bound: Bound,
        group: Arc<Group>,
        comments: QueriedComments,
        trace: Vec<Box<dyn Constraint>>,
    }

    ctx.register(
        RegisteredFeedback::new(
            "error-instance",
            FeedbackRank::Custom,
            |ctx, f| {
                let Some(Typed { group: Some(group) }) = ctx.db.get(&ctx.node) else {
                    return;
                };

                queries::error_instance(ctx, &mut |data| {
                    f(ErrorInstanceData {
                        bound: data.bound,
                        group: group.clone(),
                        comments: data.comments,
                        trace: data.trace,
                    });
                });
            },
            |data| {
                let primary = data.bound.source_node.clone();

                let related = data
                    .comments
                    .links
                    .values()
                    .flat_map(|link| [&link.node].into_iter().chain(&link.related))
                    .chain(&data.comments.nodes)
                    .chain(&data.group.nodes)
                    .cloned()
                    .collect::<BTreeSet<_>>();

                (primary, related)
            },
            |writer, data| {
                writer.write_comments(&data.comments);

                if !data.trace.is_empty() {
                    writer.write_break();
                    for constraint in &data.trace {
                        writer.write_constraint("\n\n  -  ", constraint.as_ref());
                    }
                }
            },
        )
        .show_graph(),
    );
}
