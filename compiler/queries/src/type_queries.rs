use crate::QueryCtx;
use std::collections::BTreeSet;
use wipple_core::{
    db::{Db, Node},
    facts::{Description, Syntax},
    render::Comments,
    typecheck::{
        bounds::ResolvedBounds,
        constraints::ConstraintConsequence,
        groups::{NodeRank, Typed},
        instantiate::{Instantiated, InstantiatedTypes},
        solver::DirectlyGroupedWith,
        ty::ConstructedTy,
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
    pub level: usize,
}

#[derive(Debug, Clone)]
pub struct Trace(pub Vec<TraceEntry>);

pub fn trace(db: &QueryCtx<'_>, node: Node) -> Trace {
    fn collect_traces(
        db: &QueryCtx<'_>,
        node: Node,
        filter: &mut dyn FnMut(&Db, Node) -> bool,
        seen: &mut BTreeSet<Node>,
        entries: &mut Vec<TraceEntry>,
        level: usize,
    ) {
        if !seen.insert(node) {
            return;
        }

        let definitions = db
            .get(node)
            .map_or_default(|Resolved { definitions, .. }| definitions.iter().copied());

        let resolved_bounds = db.get(node).map_or_default(|ResolvedBounds(bounds)| {
            bounds
                .values()
                .flat_map(|bound| bound.as_ref().ok())
                .filter(|bound| !bound.instance.is_from_bound)
                .map(|bound| bound.temporary)
                .collect::<Vec<_>>()
        });

        let mut relevant = Vec::new();

        let comments = definitions
            .chain(resolved_bounds)
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
            let mut entry_relevant = relevant.clone();

            let mut consequences = Vec::new();
            if db.get::<DefinitionConstraints>(comments.node).is_some() {
                // Don't traverse into other generic definitions
            } else {
                consequences.extend(db.consequences.get(&comments.node).map_or_default(
                    |consequences| {
                        entry_relevant.extend(consequences.keys().copied());
                        consequences.values().flatten().cloned()
                    },
                ));

                let instantiated_nodes = db
                    .get(comments.node)
                    .map(|InstantiatedTypes(instantiated)| instantiated.values().copied())
                    .unwrap_or_default();

                entry_relevant.extend(instantiated_nodes);

                if let Some(DirectlyGroupedWith(nodes)) = db.get(comments.node) {
                    entry_relevant.extend(nodes);
                }
            }

            entry_relevant.sort_by_key(|&node| {
                db.get::<Syntax>(node)
                    .map(|Syntax(syntax)| syntax.get(db).span(db))
            });

            relevant.extend_from_slice(&entry_relevant);

            consequences.retain(|consequence| {
                consequence
                    .relevant_nodes()
                    .into_iter()
                    .all(|node| filter(db, node))
            });

            consequences.sort_by_key(|consequence| consequence.sort_key());

            entries.push(TraceEntry {
                consequences,
                comments,
                is_primary,
                relevant: entry_relevant,
                level,
            });
        }

        for child in relevant {
            collect_traces(db, child, filter, seen, entries, level + 1);
        }
    }

    let mut entries = Vec::new();
    collect_traces(
        db,
        node,
        &mut |_, node| db.filter(node),
        &mut BTreeSet::new(),
        &mut entries,
        0,
    );

    // Sorting by node index effectively sorts in compilation order, which we
    // want for traces
    entries.sort_by_key(|entry| entry.comments.node);

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
