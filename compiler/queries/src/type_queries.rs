use crate::{QueryCtx, Trace, TraceEntry};
use itertools::{Either, Itertools};
use std::collections::BTreeSet;
use wipple_core::{
    db::Node,
    typecheck::{
        constraints::ConstraintConsequence,
        groups::{NodeRank, Prefer, Typed, representative_types_of},
        instantiate::Instantiated,
        ty::{ConstructedTy, TyTag},
    },
    visit::exhaustiveness::MatchedBy,
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
    pub is_primary: bool,
    pub conflicting: BTreeSet<Node>,
    pub group: BTreeSet<Node>,
    pub tys: Vec<ConstructedTy>,
    pub inferred_tys: Vec<ConstructedTy>,
    pub trace: Trace,
    pub primary_trace_entry: Option<TraceEntry>,
    pub summary: Option<TypeConflictSummary>,
}

pub fn conflicting_types<'a>(db: &QueryCtx<'a>, node: Node) -> Vec<ConflictingTypes> {
    let Some(Typed(Some(group))) = db.get(node) else {
        return Vec::new();
    };

    if group.tys().count() <= 1 {
        return Vec::new();
    }

    let is_primary = group.get_rank(node) == group.min_rank();

    let source = db
        .get::<Instantiated>(node)
        .map(|instantiated| instantiated.source_node);

    let mut conflicting = group.nodes().collect::<BTreeSet<_>>();

    let mut seen_tys = BTreeSet::new();
    conflicting.retain(|&node| {
        db.filter(node) && group.get_tys(node).iter().any(|ty| seen_tys.insert(ty))
    });

    let min_rank = conflicting
        .iter()
        .map(|&node| group.get_rank(node))
        .min()
        .unwrap_or_default();

    conflicting.retain(|&node| group.get_rank(node) == min_rank);

    let trace = Trace::collect(db, node);

    let mut summaries = Vec::new();
    collect_summaries(db, node, &trace, &mut summaries);
    // TODO: Sort summaries

    let (tys, inferred_tys) = group.entries().partition_map(|(_, rank, ty)| {
        if rank == NodeRank::Inherited {
            // If the node directly has a type but a rank of `Inherited`, that
            // means it's a literal
            Either::Left(ty.clone())
        } else {
            Either::Right(ty.clone())
        }
    });

    let primary_trace_entry = trace
        .0
        .iter()
        .find(|entry| {
            if !entry.is_primary || entry.comments.node != node {
                return false;
            }

            let relevant_nodes = entry
                .consequences
                .iter()
                .flat_map(|consequence| consequence.relevant_nodes())
                .collect::<BTreeSet<_>>();

            relevant_nodes.contains(&node) && relevant_nodes.is_superset(&conflicting)
        })
        .cloned();

    let base = ConflictingTypes {
        source,
        from: node,
        is_primary,
        conflicting,
        group: group.nodes().collect(),
        tys,
        inferred_tys,
        trace,
        primary_trace_entry,
        summary: None,
    };

    if summaries.is_empty() {
        vec![base]
    } else {
        summaries
            .into_iter()
            .map(|summary| {
                let mut feedback = base.clone();
                feedback.summary = Some(summary);
                feedback
            })
            .collect()
    }
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
pub struct TypeConflictSummary {
    pub entry: TraceEntry,
    pub suffix: TypeConflictSummarySuffix,
}

#[derive(Debug, Clone)]
pub enum TypeConflictSummarySuffix {
    FunctionArity {
        function: Node,
        expected: usize,
        actual: usize,
    },
}

fn collect_summaries(
    db: &QueryCtx<'_>,
    node: Node,
    trace: &Trace,
    summaries: &mut Vec<TypeConflictSummary>,
) {
    collect_function_arity_summaries(db, node, trace, summaries);
}

fn collect_function_arity_summaries(
    db: &QueryCtx<'_>,
    node: Node,
    trace: &Trace,
    summaries: &mut Vec<TypeConflictSummary>,
) {
    for entry in trace.0.iter() {
        for consequence in &entry.consequences {
            if let ConstraintConsequence::Ty(other, ref function_ty) = *consequence
                && other == node
                && let TyTag::Function = function_ty.tag
            {
                let Some((_, call_inputs)) = function_ty.children.split_first() else {
                    continue;
                };

                each_function_node(db, entry, |function_node, function_ty| {
                    let Some((_, function_inputs)) = function_ty.children.split_first() else {
                        return;
                    };

                    if function_inputs.len() == call_inputs.len() {
                        return;
                    }

                    summaries.push(TypeConflictSummary {
                        entry: entry.clone(),
                        suffix: TypeConflictSummarySuffix::FunctionArity {
                            function: function_node,
                            expected: function_inputs.len(),
                            actual: call_inputs.len(),
                        },
                    });
                });
            }
        }
    }
}

fn each_function_node(
    db: &QueryCtx<'_>,
    entry: &TraceEntry,
    mut f: impl FnMut(Node, &ConstructedTy),
) {
    for &(mut function_node) in &entry.relevant {
        let Some(Typed(Some(group))) = db.get(function_node) else {
            continue;
        };

        if let Some(representative) = group
            .nodes()
            .find(|&node| group.get_rank(node) == group.min_rank())
        {
            function_node = representative;
        }

        // Prefer using the function's name
        if let Some(MatchedBy(name)) = db.get(function_node) {
            function_node = *name;
        }

        let Some((_, function_ty)) =
            representative_types_of(db, function_node, &[], Prefer::DirectType)
                .into_iter()
                .next()
        else {
            continue;
        };

        if function_ty.tag != TyTag::Function {
            continue;
        }

        f(function_node, function_ty);
    }
}
