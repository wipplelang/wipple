use crate::{QueryCtx, Trace, TraceEntry};
use std::collections::BTreeMap;
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
pub struct ConflictingTypes<'a> {
    pub source: Option<Node>,
    pub from: Node,
    pub is_primary: bool,
    pub related: BTreeSet<Node>,
    pub group: BTreeSet<Node>,
    pub tys: Vec<ConstructedTy>,
    pub trace: Trace<'a>,
    pub summary: Option<TypeConflictSummary>,
}

pub fn conflicting_types<'a>(db: &QueryCtx<'a>, node: Node) -> Option<ConflictingTypes<'a>> {
    let Typed(Some(group)) = db.get(node)? else {
        return None;
    };

    if group.tys().count() <= 1 {
        return None;
    }

    let is_primary = group.get_rank(node) == group.min_rank();

    let source = db
        .get::<Instantiated>(node)
        .map(|instantiated| instantiated.source_node);

    let mut related = group.nodes().collect::<BTreeSet<_>>();
    related.remove(&node);
    related.retain(|&node| group.get_rank(node) <= NodeRank::Inherited);
    related.retain(|&node| db.filter(node));

    let trace = Trace::collect(db, node);

    let mut summaries = Vec::new();
    collect_summaries(db, node, &trace, &mut summaries);
    // TODO: Sort summaries

    Some(ConflictingTypes {
        source,
        from: node,
        is_primary,
        related,
        group: group.nodes().collect(),
        tys: group.tys().cloned().collect(),
        trace,
        summary: summaries.into_iter().next(),
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
pub struct TypeConflictSummary {
    pub entry: TraceEntry,
    pub suffix: TypeConflictSummarySuffix,
}

#[derive(Debug, Clone)]
pub enum TypeConflictSummarySuffix {
    FunctionInput {
        function: Node,
        parameter: Node,
        argument: Node,
    },
}

fn collect_summaries(
    db: &QueryCtx<'_>,
    node: Node,
    trace: &Trace<'_>,
    summaries: &mut Vec<TypeConflictSummary>,
) {
    collect_function_input_summaries(db, node, trace, summaries);
}

fn collect_function_input_summaries(
    db: &QueryCtx<'_>,
    node: Node,
    trace: &Trace<'_>,
    summaries: &mut Vec<TypeConflictSummary>,
) {
    for entry in trace.0.iter() {
        for consequence in &entry.consequences {
            if let ConstraintConsequence::Group(left, right) = *consequence
                && (left == node || right == node)
            {
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

                    if group.get_rank(function_node) >= NodeRank::Type {
                        continue;
                    }

                    let Some(function_ty) =
                        representative_types_of(db, function_node, &[], Prefer::DirectType)
                            .into_iter()
                            .next()
                    else {
                        continue;
                    };

                    if function_ty.tag != TyTag::Function {
                        continue;
                    }

                    let Some((_, inputs)) = function_ty.children.split_first() else {
                        continue;
                    };

                    let function_inputs = inputs
                        .iter()
                        .copied()
                        .map(|node| {
                            db.get(node)
                                .and_then(|Typed(group)| group.as_ref())
                                .into_iter()
                                .flat_map(|group| {
                                    group.entries().map(|(node, rank, _)| (node, rank))
                                })
                                .collect::<BTreeMap<_, _>>()
                        })
                        .collect::<Vec<_>>();

                    let as_parameter = |node: Node| {
                        function_inputs
                            .iter()
                            .enumerate()
                            .find_map(|(index, inputs)| Some((index, *inputs.get(&node)?)))
                    };

                    let is_annotated = |rank: NodeRank| rank >= NodeRank::Annotated;

                    let Some((left_index, left_rank)) = as_parameter(left) else {
                        continue;
                    };

                    let Some((right_index, right_rank)) = as_parameter(right) else {
                        continue;
                    };

                    if left_index != right_index {
                        continue;
                    }

                    let left_is_annotated = is_annotated(left_rank);
                    let right_is_annotated = is_annotated(right_rank);

                    let (parameter, argument) = if left_is_annotated && right_is_annotated {
                        continue;
                    } else if left_is_annotated {
                        (left, right)
                    } else if right_is_annotated {
                        (right, left)
                    } else {
                        continue;
                    };

                    summaries.push(TypeConflictSummary {
                        entry: entry.clone(),
                        suffix: TypeConflictSummarySuffix::FunctionInput {
                            function: function_node,
                            parameter,
                            argument,
                        },
                    });
                }
            }
        }
    }
}
