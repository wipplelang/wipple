use crate::{
    database::{Db, Fact, NodeRef, Render},
    nodes::TraitDefinitionNode,
    typecheck::Substitutions,
};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct Instance {
    pub node: NodeRef,
    pub trait_node: NodeRef,
    pub substitutions: Substitutions,
    pub from_bound: bool,
    pub default: bool,
    pub error: bool,
}

#[derive(Debug, Clone, Default)]
pub struct Instances(pub Vec<Instance>);

impl Fact for Instances {}

impl Render for Instances {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "has instances")
    }
}

#[derive(Debug, Clone, Default)]
pub struct Bounds(pub BTreeMap<NodeRef, BoundsItem>);

impl Fact for Bounds {}

impl Render for Bounds {
    fn write(&self, w: &mut dyn std::fmt::Write, db: &Db) -> std::fmt::Result {
        if self.0.is_empty() {
            return write!(w, "has bound(s)");
        }

        write!(w, "has bound(s) ")?;

        for (index, (_, item)) in self.0.iter().enumerate() {
            if index > 0 {
                write!(w, ", ")?;
            }

            write!(w, "{}", db.render(&item.bound))?;

            write!(w, " (")?;
            if let Some(instance) = &item.instance {
                write!(w, "{}", db.render(&instance.instance_node))?;
            } else {
                write!(w, "unresolved")?;
            }
            write!(w, ")")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct BoundsItem {
    pub bound: Bound,
    pub instance: Option<BoundsItemInstance>,
}

#[derive(Debug, Clone)]
pub struct BoundsItemInstance {
    pub instance_node: NodeRef,
    pub resolved_node: NodeRef,
    pub from_bound: bool,
    pub error: bool,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub source_node: NodeRef,
    pub bound_node: NodeRef,
    pub trait_node: NodeRef,
    pub substitutions: Substitutions,
    pub optional: bool,
}

impl Bound {
    pub fn to_instance(&self, node: NodeRef) -> Instance {
        Instance {
            node,
            trait_node: self.trait_node.clone(),
            substitutions: self.substitutions.clone(),
            from_bound: true,
            default: false,
            error: false,
        }
    }
}

impl Render for Bound {
    fn write(&self, f: &mut dyn std::fmt::Write, db: &Db) -> std::fmt::Result {
        let trait_definition = self
            .trait_node
            .downcast_ref::<TraitDefinitionNode>()
            .expect("not a trait definition");

        write!(f, "{}", trait_definition.name)?;

        for parameter in &trait_definition.parameters {
            let ty = self
                .substitutions
                .get(parameter)
                .expect("missing substitution");

            write!(f, " ")?;

            ty.render(false).write(f, db)?;
        }

        Ok(())
    }
}
