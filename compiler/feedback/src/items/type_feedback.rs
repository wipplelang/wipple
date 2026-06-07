use crate::{FeedbackCtx, FeedbackLocation, FeedbackRank};
use std::collections::{BTreeMap, BTreeSet};
use wipple_core::{
    db::Node,
    facts::Syntax,
    typecheck::{constraints::ty_constraint::TyConstraint, instantiate::Instantiated, ty::Ty},
};
use wipple_queries::{conflicting_types, fact, incomplete_type, unknown_type};
use wipple_syntax::{
    checks::instances::OverlappingInstances,
    types::{ExtraType, MissingTypes, type_parameter::TypeParameter},
};

pub fn register(ctx: &mut FeedbackCtx<'_>) {
    ctx.feedback("conflicting-types")
        .query(conflicting_types)
        .rank(|data| {
            if data.source.is_some() {
                FeedbackRank::IndirectConflicts
            } else {
                FeedbackRank::DirectConflicts
            }
        })
        .location(|_, data| {
            let mut secondary = data.nodes.iter().copied().collect::<BTreeSet<_>>();

            if data.source.is_some_and(|source| data.from != source) {
                secondary.insert(data.from);
            }

            FeedbackLocation {
                primary: data.source.unwrap_or(data.from),
                secondary,
            }
        })
        .show_graph()
        .display(|db, writer, _, data| {
            if let Some(source) = data.source {
                writer.string("In ");
                writer.node(source);
                writer.string(", ");
            }

            let has_type_constraint = data
                .trace
                .iter()
                .any(|constraint| constraint.downcast_ref::<TyConstraint>().is_some());

            if has_type_constraint && data.nodes.len() == 1 && data.tys.len() == 2 {
                writer.node(data.from);
                writer.string(" isn't a ");
                writer.ty(db, &Ty::Constructed(data.tys[1].clone()), true);
                writer.string(".");
                writer.line_break();
            } else {
                writer.node(data.from);
                writer.string(" is a ");
                writer.write_list("or a", |list| {
                    for ty in &data.tys {
                        let ty = ty.clone();
                        list.add(move |writer| writer.ty(db, &Ty::Constructed(ty), true));
                    }
                });
                writer.string(", but it can only be one of these.");
            }

            let from_span = db
                .get(data.from)
                .map(|Syntax(syntax)| db.ast(syntax).span(db).clone());

            let nodes = data
                .nodes
                .iter()
                .copied()
                .filter(|node| {
                    db.get(*node).map(|Syntax(syntax)| db.ast(syntax).span(db))
                        != from_span.as_ref()
                })
                .collect::<Vec<_>>();

            if nodes.len() > 1 {
                writer.line_break();
                writer.node(data.from);
                writer.string(" must be the same type as ");

                // Only render visible nodes
                let mut seen = BTreeSet::new();
                let nodes = nodes
                    .into_iter()
                    .filter_map(|node| {
                        let Syntax(syntax) = db.get(node)?;
                        seen.insert(db.ast(syntax).span(db)).then_some(node)
                    })
                    .collect::<Vec<_>>();

                writer.write_list("and", |list| {
                    for node in nodes {
                        list.add(move |writer| writer.node(node));
                    }
                });
                writer.string("; double-check these.");
            }

            writer.constraints(db, &data.trace);

            let mut parameters_by_name = BTreeMap::<String, Vec<(Node, Node)>>::new();
            for &node in &data.nodes {
                let Some(instantiated) = db.get::<Instantiated>(node) else {
                    continue;
                };

                let Some(Syntax(syntax)) = db.get::<Syntax>(instantiated.from) else {
                    continue;
                };

                let Some(parameter) = db.ast(syntax).downcast_ref::<TypeParameter>() else {
                    continue;
                };

                parameters_by_name
                    .entry(parameter.name.to_string())
                    .or_default()
                    .push((instantiated.source_node, instantiated.from));
            }

            for entries in parameters_by_name.values() {
                let Some(&(source, from)) = entries.first() else {
                    continue;
                };

                if entries.len() <= 1 {
                    continue;
                }

                writer.line_break();
                writer.string("-  ");
                writer.node(from);
                writer.string(" must have the same type everywhere in this use of ");
                writer.node(source);
                writer.string(".");
            }
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
            writer.string("Wipple determined this code is ");
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
                writer.write_list("and", |list| {
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
            writer.write_list("and", |list| {
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
