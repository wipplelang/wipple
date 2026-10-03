use crate::QueryCtx;
use std::{collections::BTreeSet, marker::PhantomData, ops::ControlFlow};
use wipple_core::{
    db::{Db, Node},
    facts::Description,
    render::Comments,
    typecheck::{
        constraints::ConstraintConsequence,
        groups::{Prefer, Typed, update_type},
        solver::TypeDependsOn,
        ty::Ty,
    },
    util::get_links,
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
pub struct Trace<'a>(pub Vec<TraceEntry>, PhantomData<&'a ()>);

impl<'a> Trace<'a> {
    pub fn collect(ctx: &QueryCtx<'a>, node: Node) -> Trace<'a> {
        let mut entries = Vec::new();
        collect_traces(
            ctx,
            node,
            &mut |_, node| ctx.filter(node),
            &mut BTreeSet::new(),
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

        Trace(entries, PhantomData)
    }
}

fn collect_traces(
    db: &Db,
    node: Node,
    filter: &mut dyn FnMut(&Db, Node) -> bool,
    seen_nodes: &mut BTreeSet<Node>,
    seen_consequences: &mut BTreeSet<ConstraintConsequence>,
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

        let mut consequences = if db.get::<DefinitionConstraints>(comments.node).is_none() {
            entry_relevant.extend(db.consequences.relevant(node));
            db.consequences.responsible_for(node).cloned().collect()
        } else {
            // Don't traverse into other generic definitions
            Vec::new()
        };

        consequences =
            elaborate_consequences(db, consequences, seen_consequences, &mut entry_relevant)
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
        collect_traces(db, child, filter, seen_nodes, seen_consequences, entries);
    }
}

fn elaborate_consequences(
    db: &Db,
    consequences: impl IntoIterator<Item = ConstraintConsequence>,
    seen: &mut BTreeSet<ConstraintConsequence>,
    relevant: &mut BTreeSet<Node>,
) -> impl Iterator<Item = ConstraintConsequence> {
    consequences.into_iter().flat_map(|consequence| {
        relevant.extend(consequence.relevant_nodes());

        let mut dependents = Vec::new();
        if let ConstraintConsequence::Ty(node, ref ty) = consequence {
            let group = db.get(node).and_then(|Typed(group)| group.as_ref());

            let group_nodes = group.map_or_default(|group| group.nodes().collect::<Vec<_>>());

            for &other in &group_nodes {
                if group.unwrap().get_tys(other).is_empty() {
                    let consequence = ConstraintConsequence::Ty(other, ty.clone());
                    if seen.insert(consequence.clone()) {
                        dependents.push(consequence);
                    }
                }
            }

            let intersect = {
                let group_nodes = BTreeSet::from_iter(group_nodes.iter().copied());
                move |nodes: &BTreeSet<_>| !nodes.is_disjoint(&group_nodes)
            };

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
    })
}
