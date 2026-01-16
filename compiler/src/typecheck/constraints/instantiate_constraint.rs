use crate::{
    database::NodeRef,
    typecheck::{
        Constraint, ConstraintCtx, ConstraintInfo, ConstraintResult, InstantiateContext,
        Replacements, Substitutions,
    },
    visit::DefinitionConstraints,
};

#[derive(Debug, Clone)]
pub struct Instantation {
    pub source_node: NodeRef,
    pub definition: NodeRef,
    pub replacements: Replacements,
    pub substitutions: Substitutions,
}

#[derive(Debug, Clone)]
pub struct InstantiateConstraint {
    info: ConstraintInfo,
    pub instantiation: Instantation,
}

impl InstantiateConstraint {
    pub fn new(instantiation: Instantation) -> Box<dyn Constraint> {
        Box::new(InstantiateConstraint {
            info: ConstraintInfo::new(instantiation.source_node.clone()),
            instantiation,
        })
    }
}

impl Constraint for InstantiateConstraint {
    fn info(&self) -> &ConstraintInfo {
        &self.info
    }

    fn info_mut(&mut self) -> &mut ConstraintInfo {
        &mut self.info
    }

    fn instantiate(&self, ctx: &mut InstantiateContext<'_>) -> Box<dyn Constraint> {
        let new_replacements = self
            .instantiation
            .replacements
            .entries()
            .map(|(node, replacement)| (node, ctx.instantiate_node(&replacement)))
            .collect();

        let new_substitutions = self
            .instantiation
            .substitutions
            .entries()
            .map(|(parameter, substitution)| (parameter, ctx.instantiate_type(&substitution)))
            .collect();

        InstantiateConstraint::new(Instantation {
            source_node: ctx.source_node.clone(),
            definition: self.instantiation.definition.clone(),
            replacements: new_replacements,
            substitutions: new_substitutions,
        })
    }

    fn run(&mut self, ctx: &mut ConstraintCtx<'_, '_>) -> ConstraintResult {
        // NOTE: Types are *not* applied before instantiating; we have access to
        // all related nodes/constraints here, which together will form better
        // groups

        let DefinitionConstraints(definition_constraints) = ctx
            .db
            .get(&self.instantiation.definition)
            .unwrap_or_default();

        let mut ctx = InstantiateContext {
            db: ctx.db,
            definition: self.instantiation.definition.clone(),
            source_node: self.instantiation.source_node.clone(),
            replacements: self.instantiation.replacements.clone(),
            substitutions: self.instantiation.substitutions.clone(),
        };

        ConstraintResult::Insert(
            definition_constraints
                .into_iter()
                .filter(|constraint| constraint.info().instantiate)
                .map(|constraint| constraint.instantiate(&mut ctx))
                .collect(),
        )
    }
}
