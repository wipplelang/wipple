use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    queries,
};

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(
        RegisteredFeedback::new(
            "placeholder",
            FeedbackRank::Placeholders,
            queries::placeholder,
            |(node, others, _)| (node.clone(), others.iter().cloned().collect()),
            |w, (_, _, ty)| {
                if let Some(ty) = ty {
                    w.write_string("Found a placeholder of type ");
                    w.write_type(ty.clone());
                    w.write_string(".");
                } else {
                    w.write_string("Found a placeholder.");
                }

                w.write_break();

                if let Some(ty) = ty {
                    w.write_string("Add a ");
                    w.write_type(ty.clone());
                    w.write_string(" value here before running your program.");
                } else {
                    w.write_string("Add a value here before running your program.");
                }
            },
        )
        .show_graph(),
    );
}
