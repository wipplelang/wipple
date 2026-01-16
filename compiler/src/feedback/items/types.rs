use crate::{
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::{ExtraType, MissingTypes, TypeParameterNode},
    queries,
    typecheck::{Instantiated, TypeConstraint},
    visit::OverlappingInstances,
};
use itertools::Itertools;
use std::collections::BTreeSet;

pub fn register(ctx: &mut FeedbackCtx) {
    ctx.register(
        RegisteredFeedback::new(
            "conflicting-types",
            FeedbackRank::Conflicts,
            queries::conflicting_types,
            |data| {
                let mut related = data.nodes.iter().cloned().collect::<BTreeSet<_>>();

                if let Some(source) = &data.source
                    && data.from != *source
                {
                    related.insert(data.from.clone());
                }

                (data.source.as_ref().unwrap_or(&data.from).clone(), related)
            },
            |writer, data| {
                if let Some(source) = &data.source {
                    writer.write_string("In ");
                    writer.write_node(source);
                    writer.write_string(", ");
                }

                let type_constraint = data
                    .trace
                    .iter()
                    .find_map(|constraint| constraint.as_ref().downcast_ref::<TypeConstraint>());

                // Use a traditional expected/found message if there are only 2 types
                if type_constraint.is_some() && data.nodes.len() == 2 && data.types.len() == 2 {
                    writer.write_node(&data.from);
                    writer.write_string(" isn't a ");
                    writer.write_type(data.types[1].clone());
                    writer.write_string(".");
                    writer.write_break();
                } else {
                    writer.write_node(&data.from);
                    writer.write_string(" is a ");

                    writer.write_list("or a", 3, |list| {
                        for ty in &data.types {
                            list.add(move |writer| writer.write_type(ty.clone()));
                        }
                    });

                    writer.write_string(", but it can only be one of these.");
                }

                let nodes = data
                    .nodes
                    .iter()
                    .filter(|node| !writer.db.have_equal_spans(node, &data.from))
                    .cloned()
                    .collect::<Vec<_>>();

                if nodes.len() > 1 {
                    writer.write_break();
                    writer.write_node(&data.from);
                    writer.write_string(" must be the same type as ");
                    writer.write_list("and", 3, |list| {
                        for node in nodes {
                            list.add(move |writer| writer.write_node(&node));
                        }
                    });
                    writer.write_string("; double-check these.");
                }

                if !data.trace.is_empty() {
                    for constraint in &data.trace {
                        writer.write_constraint("\n\n  -  ", constraint.as_ref());
                    }
                }

                let mut parameters_by_count = data
                    .nodes
                    .iter()
                    .filter_map(|node| {
                        let Instantiated {
                            definition, from, ..
                        } = writer.db.get::<Instantiated>(node)?;

                        from.downcast_ref::<TypeParameterNode>().map(|parameter| {
                            (definition.clone(), from.clone(), parameter.name.clone())
                        })
                    })
                    .into_group_map_by(|(_, _, name)| name.clone())
                    .into_iter()
                    .collect::<Vec<_>>();

                parameters_by_count.sort_by_key(|(name, _)| name.clone());

                for (_, entries) in parameters_by_count {
                    if entries.len() > 1 {
                        let (definition, node, _) = entries.first().unwrap();
                        writer.write_string("\n\n  -  ");
                        writer.write_node(node);
                        writer.write_string(" must have the same type everywhere in this use of ");
                        writer.write_node(definition);
                        writer.write_string(".");
                    }
                }
            },
        )
        .show_graph(),
    );

    ctx.register(
        RegisteredFeedback::new(
            "incomplete-type",
            FeedbackRank::Unknown,
            queries::incomplete_type,
            |(node, _)| (node.clone(), BTreeSet::new()),
            |writer, (node, ty)| {
                writer.write_string("Missing information for the type of ");
                writer.write_node(node);
                writer.write_string(".");

                writer.write_break();
                writer.write_string("Wipple determined this code is ");
                writer.write_type(ty.clone());
                writer.write_string(", but it needs some more information for the ");
                writer.write_code("_");
                writer.write_string(" placeholders.");
            },
        )
        .show_graph(),
    );

    ctx.register(
        RegisteredFeedback::new(
            "unknown-type",
            FeedbackRank::Unknown,
            queries::unknown_type,
            |node| (node.clone(), BTreeSet::new()),
            |writer, node| {
                writer.write_string("Could not determine the type of ");
                writer.write_node(node);
                writer.write_string(".");

                writer.write_break();
                writer.write_string("Wipple needs to know the type of this code before running it. Try using a function or assigning it to a variable.");
            },
        )
        .show_graph(),
    );

    ctx.register(RegisteredFeedback::new(
        "missing-type",
        FeedbackRank::Syntax,
        queries::fact::<MissingTypes>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, MissingTypes(parameters))| {
            writer.write_node(node);

            if let Some((parameter, [])) = parameters.split_first() {
                writer.write_string(" is missing a type for ");
                writer.write_node(parameter);
            } else {
                writer.write_string(" is missing types for ");
                writer.write_list("and", 3, |list| {
                    for parameter in parameters {
                        list.add(move |writer| writer.write_node(parameter));
                    }
                });
            }

            writer.write_string(".");

            writer.write_break();
            writer.write_string("Try adding another type here, or double-check your parentheses.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "extra-type",
        FeedbackRank::Syntax,
        queries::fact::<ExtraType>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, _)| {
            writer.write_node(node);
            writer.write_string(" doesn't match any parameter of this type.");
            writer.write_break();
            writer.write_string("Try removing this type, or double-check your parentheses.");
        },
    ));

    ctx.register(RegisteredFeedback::new(
        "conflicting-instances",
        FeedbackRank::Bounds,
        queries::fact::<OverlappingInstances>,
        |(node, _)| (node.clone(), BTreeSet::new()),
        |writer, (node, OverlappingInstances(instances))| {
            writer.write_node(node);
            writer.write_string(" has multiple overlapping instances: ");
            writer.write_list("and", 3, |list| {
                for instance in instances {
                    list.add(move |writer| writer.write_node(instance));
                }
            });
            writer.write_string(".");

            writer.write_break();
            writer.write_string("Only one of these instances can be defined at a time. Try making your instance more specific.");
        },
    ));
}
