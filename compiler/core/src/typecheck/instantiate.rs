use crate::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::{
        groups::Typed,
        solver::{Solver, SubstitutionsKey},
        ty::{Ty, TyTag},
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
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("instantiated from ");
        ctx.node(self.from);
        ctx.string(" in ");
        ctx.node(self.definition);
        ctx.string(" at ");
        ctx.node(self.source_node);
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct InstantiatedParameters(pub BTreeMap<Node, Ty>);

#[typetag::serde]
impl Fact for InstantiatedParameters {}

impl Render for InstantiatedParameters {}

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

            db.insert(replacement, Typed::default());

            db.insert(
                replacement,
                Instantiated {
                    definition: self.definition,
                    source_node: self.source_node,
                    from: node,
                },
            );

            replacement
        })
    }

    pub fn instantiate_ty(&mut self, db: &mut Db, solver: &mut Solver, ty: &Ty) -> Ty {
        ty.traverse(&mut |ty| match ty {
            Ty::Node(node) => Ty::Node(self.instantiate_node(db, solver, *node)),
            Ty::Constructed(ty) => {
                let TyTag::Parameter(parameter) = ty.tag else {
                    return Ty::Constructed(ty.clone());
                };

                solver.with_substitutions_mut(self.substitutions, |_, substitutions| {
                    if let Some(ty) = substitutions.parameters.get(&parameter) {
                        return ty.clone();
                    }

                    let ty = db.node();
                    db.hide(ty);

                    substitutions.parameters.insert(parameter, Ty::Node(ty));

                    db.insert(ty, Typed::default());

                    db.insert(
                        ty,
                        Instantiated {
                            definition: self.definition,
                            from: parameter,
                            source_node: self.source_node,
                        },
                    );

                    if substitutions.record_instantiated_parameters {
                        db.get_mut_or_default::<InstantiatedParameters>(self.source_node)
                            .0
                            .insert(parameter, Ty::Node(ty));
                    }

                    Ty::Node(ty)
                })
            }
        })
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
}
