use crate::QueryCtx;
use std::{collections::BTreeSet, ops::ControlFlow};
use wipple_core::{
    db::{Db, Node},
    facts::Description,
    render::Comments,
    typecheck::{
        constraints::ConstraintConsequence,
        groups::{Prefer, Typed, update_type},
        instantiate::InstantiatedTypes,
        solver::TypeDependsOn,
        ty::Ty,
    },
    util::{get_links, linked_nodes_for},
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
            &mut |_, node| ctx.filter(node),
            &mut BTreeSet::new(),
            &mut entries,
        );

        // Remove top-level duplicate consequences (nested consequences are
        // deduplicated by `elaborate_consequences`)
        let mut seen_consequences = BTreeSet::new();
        for entry in &mut entries {
            entry
                .consequences
                .retain(|consequence| seen_consequences.insert(consequence.clone()));
        }

        Trace(entries)
    }
}

fn collect_traces(
    db: &Db,
    node: Node,
    filter: &mut dyn FnMut(&Db, Node) -> bool,
    seen_nodes: &mut BTreeSet<Node>,
    entries: &mut Vec<TraceEntry>,
) {
    if !filter(db, node) || !seen_nodes.insert(node) {
        return;
    }

    let definitions = db
        .get(node)
        .map_or_default(|Resolved { definitions, .. }| definitions.iter().copied());

    let comments = definitions
        .filter_map(|definition_node| {
            let Defined(definition) = db.get(definition_node)?;

            let is_primary = definition.name().is_some();

            let links = get_links(db, definition_node, node, &mut *filter);

            Some((
                is_primary,
                Comments {
                    node,
                    comments: definition.comments().to_vec(),
                    links: links.clone(),
                },
            ))
        })
        .chain(db.get(node).into_iter().flat_map(|Description(entries)| {
            entries
                .iter()
                .map(|entry| (entry.is_primary, entry.comments.clone()))
        }))
        .chain([(false, Comments::empty_for(node))])
        .collect::<Vec<_>>();

    let mut relevant = BTreeSet::new();

    for (is_primary, comments) in comments {
        let mut entry_relevant = BTreeSet::from([comments.node]);

        for link in comments.links.values() {
            entry_relevant.extend(link.nodes());
            entry_relevant.extend(link.related.iter().copied());
        }

        // Add all the values from the use site, in case there is no comment
        if let Some(InstantiatedTypes(substitutions)) = db.get(node) {
            for &parameter in substitutions.keys() {
                if let Some(linked_nodes) = linked_nodes_for(db, parameter, node, &mut *filter) {
                    entry_relevant.extend(linked_nodes);
                }
            }
        }

        let mut consequences = if db.get::<DefinitionConstraints>(node).is_none() {
            entry_relevant.extend(db.consequences.relevant(node));
            db.consequences.responsible_for(node).cloned().collect()
        } else {
            // Don't traverse into other generic definitions
            Vec::new()
        };

        consequences =
            elaborate_consequences(db, consequences, &mut BTreeSet::new(), &mut entry_relevant)
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
                consequences,
                comments,
                is_primary,
                relevant: Vec::from_iter(entry_relevant),
            });
        }
    }

    for child in relevant {
        collect_traces(db, child, filter, seen_nodes, entries);
    }
}

fn elaborate_consequences(
    db: &Db,
    consequences: impl IntoIterator<Item = ConstraintConsequence>,
    seen: &mut BTreeSet<ConstraintConsequence>,
    relevant: &mut BTreeSet<Node>,
) -> impl Iterator<Item = ConstraintConsequence> {
    let consequences = consequences.into_iter().flat_map(|consequence| {
        relevant.extend(consequence.relevant_nodes());

        let mut dependents = Vec::new();
        if let ConstraintConsequence::Ty(node, _) = consequence {
            let group = db.get(node).and_then(|Typed(group)| group.as_ref());

            let group_nodes = group.map_or_default(|group| group.nodes().collect());
            let intersect = move |nodes: &BTreeSet<_>| !nodes.is_disjoint(&group_nodes);

            db.for_each_fact::<_, ()>(&mut |db, other, TypeDependsOn(dependencies)| {
                if intersect(dependencies)
                    && let Ty::Constructed(ty) =
                        update_type(db, &Ty::Node(other), &[node], Prefer::RepresentativeType)
                {
                    let consequence = ConstraintConsequence::Ty(other, ty.clone());
                    if seen.insert(consequence.clone()) {
                        dependents.push(consequence);
                    }
                }

                ControlFlow::Continue(())
            });

            let elaborated = elaborate_consequences(db, dependents.drain(..), seen, relevant)
                .collect::<Vec<_>>();

            dependents.extend(elaborated);
        }

        [consequence]
            .into_iter()
            .chain(dependents)
            .collect::<Vec<_>>()
    });

    let mut seen_grouped_nodes = BTreeSet::new();

    consequences.filter(move |consequence| {
        consequence
            .relevant_nodes()
            .into_iter()
            .any(|node| seen_grouped_nodes.insert(node))
    })
}
