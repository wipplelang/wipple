use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{Constraint, ConstraintTrace, RunResult, Solver},
        instantiate::InstantiateCtx,
        solver::SubstitutionsKey,
    },
    visit::DefinitionConstraints,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstantiateConstraint {
    pub source_node: Node,
    pub definition: Node,
    pub substitutions: SubstitutionsKey,
    pub trace: Option<Box<dyn ConstraintTrace>>,
}

#[typetag::serde]
impl Constraint for InstantiateConstraint {
    fn node(&self) -> Node {
        self.source_node
    }

    fn trace(&self) -> Option<Box<dyn ConstraintTrace>> {
        self.trace.clone()
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
            definition: self.definition,
            substitutions,
            trace: ctx.instantiate_trace(db, solver, &self.trace),
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
            substitutions: self.substitutions,
        };

        let instantiated_constraints = constraints
            .into_iter()
            .flat_map(|constraint| constraint.instantiate(db, solver, &mut ctx))
            .collect::<Vec<_>>();

        RunResult::Insert(instantiated_constraints)
    }
}
