use crate::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::{solver::SubstitutionsKey, ty::Ty},
    visit::definitions::{Defined, TraitDefinition},
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instance {
    pub node: Node,
    pub trait_node: Node,
    pub parameters: BTreeMap<Node, Ty>,
    pub is_from_bound: bool,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Instances(pub Vec<Instance>);

#[typetag::serde]
impl Fact for Instances {}

impl Render for Instances {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string(format!("has {} instances", self.0.len()));
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Bounds(pub Vec<(Node, Result<ResolvedBound, UnresolvedBound>)>);

#[typetag::serde]
impl Fact for Bounds {}

impl Render for Bounds {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
        if self.0.is_empty() {
            ctx.string("has no bounds");
        }

        ctx.string("has bound(s) ");

        for (index, (_, result)) in self.0.iter().enumerate() {
            if index > 0 {
                ctx.string(", ");
            }

            match result {
                Ok(bound) => {
                    ctx.render(db, &bound.instance);
                    ctx.string(" (resolved as ");
                    ctx.node(bound.instance.node);
                    ctx.string(")");
                }
                Err(bound) => {
                    ctx.render(db, bound);
                    ctx.string(" (unresolved)");
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedBound {
    pub instance: Instance,
    pub instance_parameters: BTreeMap<Node, Ty>,
    pub resolved_node: Node,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnresolvedBound {
    pub trait_node: Node,
    pub parameters: BTreeMap<Node, Ty>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bound {
    pub source_node: Node,
    pub bound_node: Node,
    pub trait_node: Node,
    pub target_node: Option<Node>,
    pub substitutions: SubstitutionsKey,
    pub is_optional: bool,
}

impl Render for Instance {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
        UnresolvedBound {
            trait_node: self.trait_node,
            parameters: self.parameters.clone(),
        }
        .render_into(db, ctx);
    }
}

impl Render for UnresolvedBound {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
        let trait_definition = db
            .get::<Defined>(self.trait_node)
            .unwrap()
            .0
            .downcast_ref::<TraitDefinition>()
            .unwrap();

        ctx.link(trait_definition.name.to_string(), self.trait_node);

        for parameter in &trait_definition.parameters {
            ctx.string(" ");

            if let Some(ty) = self.parameters.get(parameter) {
                ctx.ty(db, ty, false);
            } else {
                ctx.code("_");
            }
        }
    }
}
