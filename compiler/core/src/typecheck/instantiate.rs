use crate::{
    db::{Db, Fact, Node},
    facts::Syntax,
    render::{Render, RenderCtx},
    typecheck::{
        constraints::AnyConstraintTrace,
        groups::{Annotated, Typed},
        solver::{Solver, SubstitutionsKey},
        ty::{ConstructedTy, Ty, TyTag},
    },
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instantiated {
    pub definition: Node,
    pub from: Node,
    pub source_node: Node,
}

#[typetag::serde]
impl Fact for Instantiated {}

impl Render for Instantiated {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx<'_>) {
        ctx.string("instantiated from ");
        ctx.node(self.from);
        ctx.string(" in ");
        ctx.node(self.definition);
        ctx.string(" at ");
        ctx.node(self.source_node);
    }
}

#[derive(Debug, Clone)]
pub struct InstantiateCtx {
    pub definition: Node,
    pub source_node: Node,
    pub substitutions: SubstitutionsKey,
}

impl InstantiateCtx {
    pub fn instantiate_node(&mut self, db: &mut Db, solver: &mut Solver, node: Node) -> Node {
        solver.with_substitutions_mut(self.substitutions, |_, substitutions| {
            if let Some(replacement) = substitutions.nodes.get(&node) {
                return *replacement;
            }

            let replacement = db.node();
            db.hide(replacement);
            substitutions.nodes.insert(node, replacement);

            if let Some(Syntax(syntax)) = db.get(node) {
                db.insert(replacement, Syntax(syntax.clone()));
            }

            db.insert(replacement, Typed::default());

            db.insert(
                replacement,
                Instantiated {
                    definition: self.definition,
                    source_node: self.source_node,
                    from: node,
                },
            );

            if db.contains::<Annotated>(node) {
                db.insert(replacement, Annotated);
            }

            replacement
        })
    }

    pub fn instantiate_ty(&mut self, db: &mut Db, solver: &mut Solver, ty: &Ty) -> Ty {
        match solver.apply_ty(db, ty) {
            Ty::Node(other) => Ty::Node(self.instantiate_node(db, solver, other)),
            Ty::Constructed(constructed) => {
                self.instantiate_constructed_ty(db, solver, &constructed)
            }
        }
    }

    pub fn instantiate_constructed_ty(
        &mut self,
        db: &mut Db,
        solver: &mut Solver,
        ty: &ConstructedTy,
    ) -> Ty {
        let mut instantiated_ty = Ty::Constructed(ty.clone());
        if let TyTag::Parameter(parameter) = ty.tag {
            solver.with_substitutions_mut(self.substitutions, |_, substitutions| {
                if let Some(existing) = substitutions.parameters.get(&parameter) {
                    instantiated_ty = existing.clone();
                    return;
                }

                let node = db.node();
                db.hide(node);

                substitutions.parameters.insert(parameter, Ty::Node(node));

                if let Some(Syntax(syntax)) = db.get(parameter) {
                    db.insert(node, Syntax(syntax.clone()));
                }

                db.insert(node, Typed::default());

                db.insert(
                    node,
                    Instantiated {
                        definition: self.definition,
                        from: parameter,
                        source_node: self.source_node,
                    },
                );

                db.insert(node, Annotated);

                instantiated_ty = Ty::Node(node);
            });
        } else {
            for node in instantiated_ty.referenced_nodes_mut() {
                *node = self.instantiate_node(db, solver, *node);
            }
        }

        instantiated_ty
    }

    pub fn instantiate_nodes(
        &mut self,
        db: &mut Db,
        solver: &mut Solver,
        nodes: &mut BTreeMap<Node, Node>,
    ) {
        for node in nodes.values_mut() {
            *node = self.instantiate_node(db, solver, *node);
        }
    }

    pub fn instantiate_parameters(
        &mut self,
        db: &mut Db,
        solver: &mut Solver,
        parameters: &mut BTreeMap<Node, Ty>,
    ) {
        for ty in parameters.values_mut() {
            *ty = self.instantiate_ty(db, solver, ty);
        }
    }

    pub fn instantiate_traces(
        &mut self,
        db: &mut Db,
        solver: &mut Solver,
        traces: &[AnyConstraintTrace],
    ) -> Vec<AnyConstraintTrace> {
        traces
            .iter()
            .map(|trace| {
                let mut trace = trace.clone();

                for node in trace.nodes_mut() {
                    *node = self.instantiate_node(db, solver, *node);
                }

                trace.source_node.get_or_insert(self.source_node);

                trace
            })
            .collect()
    }
}
