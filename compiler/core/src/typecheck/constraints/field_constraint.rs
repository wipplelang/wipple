use crate::{
    db::{Db, Node},
    span::Str,
    typecheck::{
        constraints::{
            Constraint, ConstraintKind, RunResult, Solver,
            instantiate_constraint::InstantiateConstraint,
        },
        instantiate::InstantiateCtx,
        resolved::ResolvedField,
        ty::{Ty, TyTag},
    },
    visit::{
        definitions::{Defined, TypeDefinition},
        exhaustiveness::StructureFields,
    },
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldConstraint {
    pub node: Node,
    pub value: Node,
    pub field: Str,
}

impl FieldConstraint {
    pub fn new(node: Node, value: Node, field: Str) -> Self {
        FieldConstraint { node, value, field }
    }
}

#[typetag::serde]
impl Constraint for FieldConstraint {
    fn kind(&self) -> ConstraintKind {
        ConstraintKind::Bound
    }

    fn instantiate(
        &self,
        db: &mut Db,
        solver: &mut Solver,
        ctx: &mut InstantiateCtx,
    ) -> Option<Box<dyn Constraint>> {
        Some(Box::new(FieldConstraint {
            node: ctx.instantiate_node(db, solver, self.node),
            value: ctx.instantiate_node(db, solver, self.value),
            field: self.field.clone(),
        }))
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        let Ty::Constructed(ty) = solver.apply_ty(db, &Ty::Node(self.value)) else {
            return RunResult::Enqueue(vec![(self.node, self)]); // ambiguous; try again
        };

        if let TyTag::Named(definition_node) = ty.tag
            && let Some(definition) = db
                .get(definition_node)
                .and_then(|Defined(definition)| definition.downcast_ref::<TypeDefinition>())
            && let Some(&StructureFields {
                constructor: constructor_node,
                ref fields,
            }) = db.get(definition_node)
            && let Some((field_index, &(_, field_node))) = fields
                .iter()
                .enumerate()
                .find(|(_, (name, _))| *name == self.field)
        {
            let substitutions = solver.insert_substitutions(
                BTreeMap::from([(field_node, self.node)]),
                definition
                    .parameters
                    .iter()
                    .copied()
                    .zip(ty.children[0].iter().map(|&node| Ty::Node(node)))
                    .collect(),
            );

            db.insert(
                self.node,
                ResolvedField {
                    value: self.value,
                    index: Ok(field_index),
                },
            );

            RunResult::Insert(vec![(
                self.node,
                Box::new(InstantiateConstraint::new(
                    self.node,
                    constructor_node,
                    substitutions,
                )),
            )])
        } else {
            db.insert(
                self.node,
                ResolvedField {
                    value: self.value,
                    index: Err(self.field),
                },
            );

            RunResult::None
        }
    }
}
