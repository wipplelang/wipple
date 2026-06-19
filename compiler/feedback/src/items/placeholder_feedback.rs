use crate::{FeedbackCtx, FeedbackLocation, FeedbackRank};
use std::collections::BTreeSet;
use wipple_core::typecheck::ty::Ty;
use wipple_queries::placeholder;

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("placeholder")
        .query(placeholder)
        .rank(|_| FeedbackRank::Placeholders)
        .location(|_, (node, others, _)| FeedbackLocation {
            primary: *node,
            secondary: others.iter().copied().collect::<BTreeSet<_>>(),
        })
        .show_graph()
        .display(|db, writer, _, (_, _, ty)| {
            if let Some(ty) = *ty {
                writer.string("Found a placeholder of type ");
                writer.ty(db, &Ty::Constructed(ty.clone()), true);
                writer.string(".");
            } else {
                writer.string("Found a placeholder.");
            }

            writer.line_break();

            if let Some(ty) = *ty {
                writer.string("Add a ");
                writer.ty(db, &Ty::Constructed(ty.clone()), true);
                writer.string(" value here before running your program.");
            } else {
                writer.string("Add a value here before running your program.");
            }
        })
        .register();
}
