use crate::QueryCtx;
use std::{
    collections::{BTreeMap, BTreeSet},
    ops::ControlFlow,
};
use wipple_core::{
    db::{Db, Node},
    facts::{Description, description_comments},
    render::Comments,
    typecheck::{
        resolved::ResolvedBounds, constraints::ConstraintConsequence, groups::Typed,
        instantiate::InstantiatedTypes, solver::TypeDependsOn,
    },
    util::{get_links, linked_nodes_for, section},
    visit::{DefinitionConstraints, Resolved, definitions::Defined},
};

#[derive(Debug, Clone)]
pub struct TraceEntry {
    pub is_primary: bool,
    pub comments: Comments,
    pub consequences: Vec<ConstraintConsequence>,
    pub relevant: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Trace(pub Vec<TraceEntry>);

impl Trace {
    pub fn collect(ctx: &QueryCtx<'_>, node: Node) -> Trace {
        let mut entries = Vec::new();
        collect_traces(
            ctx,
            node,
            node,
            &mut |_, node| ctx.filter(node),
            &mut BTreeSet::new(),
            &mut entries,
        );

        let mut seen_consequences = Vec::new();
        for entry in &mut entries {
            // Remove top-level duplicate consequences (nested consequences are
            // deduplicated by `elaborate_consequences`)
            entry.consequences.retain(|consequence| {
                if seen_consequences.contains(consequence) {
                    false
                } else {
                    seen_consequences.push(consequence.clone());
                    true
                }
            });

            entry.relevant.sort();
        }

        Trace(entries)
    }
}

fn collect_traces(
    db: &Db,
    node: Node,
    primary_node: Node,
    filter: &mut dyn FnMut(&Db, Node) -> bool,
    seen_nodes: &mut BTreeSet<Node>,
    entries: &mut Vec<TraceEntry>,
) {
    if !filter(db, node) || !seen_nodes.insert(node) {
        return;
    }

    let definitions = db
        .get(node)
        .map_or_default(|Resolved { definitions, .. }| definitions.iter().copied())
        .chain(db.get(node).map_or_default(|ResolvedBounds(bounds)| {
            bounds
                .values()
                .flatten()
                .map(|bound| bound.instance.node)
                .collect::<Vec<_>>()
        }));

    let mut comments = vec![(false, None, Comments::empty_for(node))];
    let mut consequences = BTreeMap::<Node, Vec<Comments>>::new();

    for definition_node in definitions {
        let Some(Defined(definition)) = db.get(definition_node) else {
            continue;
        };

        let is_primary = definition.name().is_some();

        let links = get_links(db, definition_node, node, &mut *filter);

        if let Some(section) = section(definition.comments(), None) {
            comments.push((
                is_primary,
                None,
                Comments {
                    node,
                    comments: section.to_vec(),
                    links: links.clone(),
                },
            ));
        }

        if let Some(section) = section(
            definition.comments(),
            Some(description_comments::CONSEQUENCES),
        ) {
            consequences.entry(node).or_default().push(Comments {
                node,
                comments: section.to_vec(),
                links,
            });
        }
    }

    if let Some(description) = db.get::<Description>(node) {
        if let Some(section) = [&description.data_flow, &description.summary]
            .into_iter()
            .find(|entry| !entry.is_empty())
        {
            comments.extend(section.iter().map(|entry| {
                (
                    entry.is_primary,
                    entry.matching_node,
                    entry.comments.clone(),
                )
            }));
        }

        for entry in &description.consequences {
            consequences
                .entry(node)
                .or_default()
                .push(entry.comments.clone());
        }
    }

    let mut relevant = Vec::new();

    for (is_primary, matching_node, comments) in comments {
        if matching_node.is_some_and(|node| node != primary_node) {
            continue;
        }

        let mut entry_relevant = vec![comments.node];

        // Add all the values from the use site, in case there is no comment
        if let Some(InstantiatedTypes(substitutions)) = db.get(node) {
            for &parameter in substitutions.keys() {
                if let Some(linked_nodes) = linked_nodes_for(db, parameter, node, &mut *filter) {
                    entry_relevant.extend(linked_nodes);
                }
            }
        }

        for link in comments.links.values() {
            entry_relevant.extend(link.nodes());
            entry_relevant.extend(link.related.iter().copied());
        }

        let mut consequences = consequences
            .get(&node)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(ConstraintConsequence::Custom)
            .collect::<Vec<_>>();

        if db.get::<DefinitionConstraints>(node).is_none() {
            entry_relevant.extend(db.consequences.relevant(node));
            consequences.extend(db.consequences.responsible_for(node).cloned());
        } else {
            // Don't traverse into other generic definitions
        }

        consequences =
            elaborate_consequences(db, consequences, &mut Vec::new(), &mut entry_relevant)
                .collect();

        relevant.extend(entry_relevant.iter().copied());

        consequences.retain(|consequence| {
            consequence
                .relevant_nodes()
                .into_iter()
                .all(|node| filter(db, node))
        });

        if filter(db, comments.node) && !comments.comments.is_empty() {
            entries.push(TraceEntry {
                is_primary,
                comments,
                consequences,
                relevant: Vec::from_iter(entry_relevant),
            });
        }
    }

    for child in relevant {
        collect_traces(db, child, primary_node, filter, seen_nodes, entries);
    }
}

fn elaborate_consequences(
    db: &Db,
    consequences: impl IntoIterator<Item = ConstraintConsequence>,
    seen_consequences: &mut Vec<ConstraintConsequence>,
    relevant: &mut Vec<Node>,
) -> impl Iterator<Item = ConstraintConsequence> {
    let consequences = consequences.into_iter().flat_map(move |consequence| {
        if seen_consequences.contains(&consequence) {
            return Vec::new();
        }

        seen_consequences.push(consequence.clone());

        relevant.extend(consequence.relevant_nodes());

        let mut dependents = Vec::new();
        if let ConstraintConsequence::Ty(node, _) = consequence {
            let group = db.get(node).and_then(|Typed(group)| group.as_ref());

            let group_nodes = group.map_or_default(|group| group.nodes().collect());
            let intersect = move |nodes: &BTreeSet<_>| !nodes.is_disjoint(&group_nodes);

            db.for_each_fact::<_, ()>(false, &mut |db, other, TypeDependsOn(dependencies)| {
                if intersect(dependencies)
                    && let Some(Typed(Some(group))) = db.get(other)
                    && let Some((_, ty)) = group.representative_types(Some(node)).into_iter().next()
                {
                    let consequence = ConstraintConsequence::Ty(other, ty.clone());
                    dependents.push(consequence);
                }

                ControlFlow::Continue(())
            });

            let elaborated =
                elaborate_consequences(db, dependents.drain(..), seen_consequences, relevant)
                    .collect::<Vec<_>>();

            dependents.extend(elaborated);
        }

        [consequence]
            .into_iter()
            .chain(dependents)
            .collect::<Vec<_>>()
    });

    let mut seen_grouped_nodes = BTreeSet::new();
    consequences.filter(move |consequence| seen_grouped_nodes.insert(consequence.relevant_nodes()))
}
