use crate::QueryCtx;
use std::{collections::BTreeSet, ops::ControlFlow};
use wipple_core::{
    db::{Db, Node},
    facts::Description,
    render::Comments,
    typecheck::{
        constraints::ConstraintConsequence,
        groups::{NodeRank, Prefer, Typed, update_type},
        instantiate::Instantiated,
        solver::TypeDependsOn,
        ty::{ConstructedTy, Ty},
    },
    util::get_links,
    visit::{DefinitionConstraints, Resolved, definitions::Defined},
};

pub fn has_type<'a>(db: &QueryCtx<'a>, node: Node) -> Option<&'a ConstructedTy> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    group.tys().next()
}

pub fn in_group(db: &QueryCtx<'_>, node: Node) -> impl Iterator<Item = Node> {
    let Some(Typed(Some(group))) = db.get(node) else {
        return Default::default();
    };

    group.nodes().collect::<Vec<_>>().into_iter()
}

#[derive(Debug, Clone)]
pub struct ConflictingTypes {
    pub source: Option<Node>,
    pub from: Node,
    pub related: BTreeSet<Node>,
    pub group: BTreeSet<Node>,
    pub tys: Vec<ConstructedTy>,
    pub trace: Trace,
}

pub fn conflicting_types(db: &QueryCtx<'_>, node: Node) -> Option<ConflictingTypes> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    if group.tys().count() <= 1 {
        return None;
    }

    if group.get_rank(node) > group.min_rank() {
        return None;
    }

    let source = db
        .get::<Instantiated>(node)
        .map(|instantiated| instantiated.source_node);

    let mut related = group.nodes().collect::<BTreeSet<_>>();
    related.remove(&node);
    related.retain(|&node| group.get_rank(node) <= NodeRank::Inherited);
    related.retain(|&node| db.filter(node));

    Some(ConflictingTypes {
        source,
        from: node,
        related,
        group: group.nodes().collect(),
        tys: group.tys().cloned().collect(),
        trace: trace(db, node),
    })
}

pub fn incomplete_type<'a>(db: &QueryCtx<'a>, node: Node) -> Option<(Node, &'a ConstructedTy)> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    let mut tys = group.tys();
    let ty = tys.next()?;

    if tys.next().is_some() {
        return None;
    }

    if ty.children.iter().any(|&ty| {
        db.get(ty)
            .and_then(|Typed(group)| group.as_ref())
            .is_some_and(|group| group.tys().next().is_none())
    }) {
        return Some((node, ty));
    }

    None
}

pub fn unknown_type(db: &QueryCtx<'_>, node: Node) -> bool {
    let Some(Typed(group)) = db.get(node) else {
        return false;
    };

    let Some(group) = group else {
        return true;
    };

    group.tys().next().is_none()
}

#[derive(Debug, Clone)]
pub struct TraceEntry {
    pub is_primary: bool,
    pub comments: Comments,
    pub consequences: Vec<ConstraintConsequence>,
    pub relevant: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Trace(pub Vec<TraceEntry>);

pub fn trace(db: &QueryCtx<'_>, node: Node) -> Trace {
    fn collect_traces(
        db: &QueryCtx<'_>,
        node: Node,
        filter: &mut dyn FnMut(&Db, Node) -> bool,
        seen_nodes: &mut BTreeSet<Node>,
        seen_consequences: &mut Vec<ConstraintConsequence>,
        entries: &mut Vec<TraceEntry>,
    ) {
        if !seen_nodes.insert(node) {
            return;
        }

        let definitions = db
            .get(node)
            .map_or_default(|Resolved { definitions, .. }| definitions.iter().copied());

        let mut relevant = Vec::new();

        let comments = definitions
            .filter_map(|definition_node| {
                relevant.push(definition_node);

                let Defined(definition) = db.get(definition_node)?;

                let is_primary = definition.name().is_some();

                let links = get_links(db, definition_node, node, &mut *filter);

                for link in links.values() {
                    relevant.extend(link.nodes());
                    relevant.extend(link.related.iter().copied());
                }

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
            .chain([(true, Comments::empty_for(node))])
            .collect::<Vec<_>>();

        for (is_primary, comments) in comments {
            let mut entry_relevant = Vec::new();

            let mut consequences = if db.get::<DefinitionConstraints>(comments.node).is_none() {
                db.consequences
                    .get(&comments.node)
                    .map_or_default(|consequences| {
                        consequences
                            .iter()
                            .flat_map(|(&relevant, consequences)| {
                                entry_relevant.push(relevant);
                                consequences.iter().cloned()
                            })
                            .collect()
                    })
            } else {
                // Don't traverse into other generic definitions
                Vec::new()
            };

            let mut consequence_filter = |consequence: &ConstraintConsequence| {
                consequence
                    .relevant_nodes()
                    .into_iter()
                    .all(|node| filter(db, node))
            };

            consequences = elaborate_consequences(
                db,
                consequences,
                &mut consequence_filter,
                seen_consequences,
                &mut entry_relevant,
            )
            .collect();

            relevant.extend_from_slice(&entry_relevant);

            entry_relevant.sort();

            if !comments.comments.is_empty() {
                entries.push(TraceEntry {
                    consequences,
                    comments,
                    is_primary,
                    relevant: entry_relevant,
                });
            }
        }

        for child in relevant {
            collect_traces(db, child, filter, seen_nodes, seen_consequences, entries);
        }
    }

    let mut entries = Vec::new();
    collect_traces(
        db,
        node,
        &mut |_, node| db.filter(node),
        &mut BTreeSet::new(),
        &mut Vec::new(),
        &mut entries,
    );

    // Remove top-level duplicate consequences (nested consequences are
    // deduplicated by `elaborate_consequences`)
    let mut seen_consequences = Vec::new();
    for entry in &mut entries {
        entry.consequences.retain(|consequence| {
            if seen_consequences.contains(consequence) {
                return false;
            }

            seen_consequences.push(consequence.clone());
            true
        });
    }

    Trace(entries)
}

fn elaborate_consequences(
    db: &Db,
    consequences: impl IntoIterator<Item = ConstraintConsequence>,
    filter: &mut dyn FnMut(&ConstraintConsequence) -> bool,
    seen: &mut Vec<ConstraintConsequence>,
    relevant: &mut Vec<Node>,
) -> impl Iterator<Item = ConstraintConsequence> {
    consequences.into_iter().flat_map(|consequence| {
        relevant.extend(consequence.relevant_nodes());

        let include_directly = filter(&consequence);

        match consequence {
            ConstraintConsequence::Ty(node, ty, mut dependents) => {
                let mut insert =
                    |dependent: ConstraintConsequence, seen: &mut Vec<ConstraintConsequence>| {
                        if !seen.contains(&dependent) {
                            seen.push(dependent.clone());
                            dependents.push(dependent);
                        }
                    };

                let group = db.get(node).and_then(|Typed(group)| group.as_ref());

                let group_nodes = group.map_or_default(|group| group.nodes().collect::<Vec<_>>());

                for &other in &group_nodes {
                    if group.unwrap().get_tys(other).is_empty() {
                        insert(
                            ConstraintConsequence::Ty(other, ty.clone(), Vec::new()),
                            seen,
                        );
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
                        insert(
                            ConstraintConsequence::Ty(other, ty.clone(), Vec::new()),
                            seen,
                        );
                    }

                    ControlFlow::Continue(())
                });

                dependents =
                    elaborate_consequences(db, dependents, &mut *filter, seen, relevant).collect();

                if !include_directly {
                    dependents
                } else {
                    vec![ConstraintConsequence::Ty(node, ty, dependents)]
                }
            }
            consequence if include_directly => {
                seen.push(consequence.clone());
                vec![consequence]
            }
            _ => Vec::new(),
        }
    })
}
