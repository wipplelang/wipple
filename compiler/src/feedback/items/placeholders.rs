use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    queries,
};
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(
        RegisteredFeedback::new(
            "placeholder",
            FeedbackRank::Placeholders,
            queries::placeholder,
            |(node, _)| (node.clone(), BTreeSet::new()),
            |writer, (_, ty)| {
                if let Some(ty) = ty {
                    writer.write_string("Found a placeholder of type ");
                    writer.write_type(ty.clone());
                    writer.write_string(".");
                } else {
                    writer.write_string("Found a placeholder.");
                }

                writer.write_break();

                if let Some(ty) = ty {
                    writer.write_string("Add a ");
                    writer.write_type(ty.clone());
                    writer.write_string(" value here before running your program.");
                } else {
                    writer.write_string("Add a value here before running your program.");
                }
            },
        )
        .show_graph(),
    );
}
