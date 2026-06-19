use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{
            AnyConstraintTrace, Constraint, ConstraintKind, ConstraintTrace, RunResult, Solver,
        },
        instantiate::InstantiateCtx,
        solver::SubstitutionsKey,
    },
    visit::DefinitionConstraints,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstantiateConstraint {
    pub source_node: Node,
    pub bound_path: Vec<Node>,
    pub definition: Node,
    pub substitutions: SubstitutionsKey,
    pub traces: Vec<AnyConstraintTrace>,
}

impl InstantiateConstraint {
    pub fn new(source_node: Node, definition: Node, substitutions: SubstitutionsKey) -> Self {
        InstantiateConstraint {
            source_node,
            bound_path: Vec::new(),
            definition,
            substitutions,
            traces: Vec::new(),
        }
    }

    pub fn with_trace(mut self, trace: impl ConstraintTrace) -> Self {
        self.traces.push(AnyConstraintTrace::new(trace));
        self
    }
}

#[typetag::serde]
impl Constraint for InstantiateConstraint {
    fn kind(&self) -> ConstraintKind {
        ConstraintKind::Ty
    }

    fn node(&self) -> Node {
        self.source_node
    }

    fn traces_mut(&mut self) -> &mut Vec<AnyConstraintTrace> {
        &mut self.traces
    }

    fn instantiate(
        &self,
        db: &mut Db,
        solver: &mut Solver,
        ctx: &mut InstantiateCtx,
    ) -> Option<Box<dyn Constraint>> {
        let (mut nodes, mut parameters) = solver.get_substitutions(self.substitutions).clone();

        ctx.instantiate_nodes(db, solver, &mut nodes);
        ctx.instantiate_parameters(db, solver, &mut parameters);

        let substitutions = solver.insert_substitutions(nodes, parameters);

        Some(Box::new(InstantiateConstraint {
            source_node: ctx.source_node,
            bound_path: ctx.bound_path.clone(),
            definition: self.definition,
            substitutions,
            traces: ctx.instantiate_traces(db, solver, &self.traces),
        }))
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        // NOTE: Types are *not* applied before instantiating; we have access to
        // all related nodes/constraints here, which together will form better
        // groups

        let DefinitionConstraints(constraints) =
            db.get(self.definition).cloned().unwrap_or_default();

        let mut ctx = InstantiateCtx {
            definition: self.definition,
            source_node: self.source_node,
            bound_path: self.bound_path.clone(),
            substitutions: self.substitutions,
        };

        let mut instantiated_constraints = constraints
            .into_iter()
            .flat_map(|constraint| constraint.instantiate(db, solver, &mut ctx))
            .collect::<Vec<_>>();

        let mut traces = self.traces.clone();
        for trace in &mut traces {
            trace.from.extend(solver.active_traces.clone());
        }

        for constraint in &mut instantiated_constraints {
            constraint.traces_mut().extend(traces.iter().cloned());
        }

        RunResult::Insert(instantiated_constraints)
    }
}
