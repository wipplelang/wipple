use crate::{FeedbackCtx, FeedbackLocation, FeedbackRank};
use wipple_core::typecheck::{groups::Prefer, ty::Ty};
use wipple_queries::{
    TypeConflictSummarySuffix, conflicting_types, fact, incomplete_type, unknown_type,
};
use wipple_syntax::{
    checks::instances::OverlappingInstances,
    types::{ExtraType, MissingTypes},
};

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("conflicting-types")
        .query(conflicting_types)
        .rank(|data| {
            if data.source.is_none() && data.is_primary {
                FeedbackRank::DirectConflicts
            } else {
                FeedbackRank::IndirectConflicts
            }
        })
        .location(|_, data| {
            let mut primary = data.source.unwrap_or(data.from);

            if let Some(summary) = &data.summary {
                primary = summary.entry.comments.node;
            }

            FeedbackLocation {
                primary,
                secondary: data.group.clone(),
            }
        })
        .show_graph()
        .display(|db, writer, _, data| {
            if let Some(summary) = &data.summary {
                writer.with_relevant(&summary.entry.relevant, Prefer::DirectType, |writer| {
                    writer.comments(db, &summary.entry.comments);

                    match &summary.suffix {
                        TypeConflictSummarySuffix::FunctionInput {
                            function,
                            parameter,
                            argument,
                        } => {
                            writer.string(" But ");
                            writer.node(*function);
                            writer.string(" accepts a ");
                            writer.ty(db, &Ty::Node(*parameter), true);
                            writer.string(" for the input ");
                            writer.node(*parameter);
                            writer.string(", not a ");
                            writer.ty(db, &Ty::Node(*argument), true);
                            writer.string(".");
                        }
                    }
                });
            } else {
                if let Some(source) = data.source {
                    writer.string("In ");
                    writer.node(source);
                    writer.string(", ");
                }

                writer.node(data.from);
                writer.string(" is a ");
                writer.list("or a", |_, list| {
                    for ty in &data.tys {
                        let ty = ty.clone();
                        list.add(move |writer| {
                            writer.ty(db, &Ty::Constructed(ty), true);
                        });
                    }
                });
                writer.string(", but it can only be one of these.");

                if data.related.len() > 1 {
                    writer.line_break();
                    writer.node(data.from);
                    writer.string(" must be the same type as ");
                    writer.list("and", |_, list| {
                        for &node in &data.related {
                            list.add(move |writer| writer.node(node));
                        }
                    });
                    writer.string("; double-check these.");
                }
            }

            writer.extend_trace(db, &data.trace);
        })
        .register();

    ctx.feedback("incomplete-type")
        .query(incomplete_type)
        .rank(|_| FeedbackRank::Unknown)
        .location(|_, (node, _)| FeedbackLocation::from(*node))
        .show_graph()
        .display(|db, writer, _, (node, ty)| {
            writer.string("Missing information for the type of ");
            writer.node(*node);
            writer.string(".");
            writer.line_break();
            writer.string("Wipple determined this code is a ");
            writer.ty(db, &Ty::Constructed((*ty).clone()), true);
            writer.string(", but it needs some more information for the ");
            writer.code("_");
            writer.string(" placeholders.");
        })
        .register();

    ctx.feedback("unknown-type")
        .query(unknown_type)
        .rank(|_| FeedbackRank::Unknown)
        .show_graph()
        .display(|_db, writer, node, _| {
            writer.string("Could not determine the type of ");
            writer.node(node);
            writer.string(".");
            writer.line_break();
            writer.string(
                "Wipple needs to know the type of this code before running it. Try using a function or assigning it to a variable.",
            );
        })
        .register();

    ctx.feedback("missing-type")
        .query(fact::<MissingTypes>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, MissingTypes(parameters)| {
            writer.node(node);

            if let &[parameter] = parameters.as_slice() {
                writer.string(" is missing a type for ");
                writer.node(parameter);
            } else {
                writer.string(" is missing types for ");
                writer.list("and", |_, list| {
                    for &parameter in parameters {
                        list.add(move |writer| writer.node(parameter));
                    }
                });
            }

            writer.string(".");
            writer.line_break();
            writer.string("Try adding another type here, or double-check your parentheses.");
        })
        .register();

    ctx.feedback("extra-type")
        .query(fact::<ExtraType>)
        .rank(|_| FeedbackRank::Syntax)
        .display(|_db, writer, node, _| {
            writer.node(node);
            writer.string(" doesn't match any parameter of this type.");
            writer.line_break();
            writer.string("Try removing this type, or double-check your parentheses.");
        })
        .register();

    ctx.feedback("conflicting-instances")
        .query(fact::<OverlappingInstances>)
        .rank(|_| FeedbackRank::Bounds)
        .display(|_db, writer, node, OverlappingInstances(instances)| {
            writer.node(node);
            writer.string(" has multiple overlapping instances: ");
            writer.list("and", |_, list| {
                for &instance in instances {
                    list.add(move |writer| writer.node(instance));
                }
            });
            writer.string(".");
            writer.line_break();
            writer.string(
                "Only one of these instances can be defined at a time. Try making your instance more specific.",
            );
        })
        .register();
}
