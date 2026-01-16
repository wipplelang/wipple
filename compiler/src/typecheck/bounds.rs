use crate::{
    database::{Db, Fact, NodeRef, Render},
    nodes::TraitDefinitionNode,
    typecheck::Substitutions,
};

#[derive(Debug, Clone)]
pub struct Instance {
    pub node: NodeRef,
    pub trait_node: NodeRef,
    pub substitutions: Substitutions,
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
pub struct Bounds(pub Vec<BoundsItem>);

impl Fact for Bounds {}

impl Render for Bounds {
    fn write(&self, w: &mut dyn std::fmt::Write, db: &Db) -> std::fmt::Result {
        if self.0.is_empty() {
            return write!(w, "has bound(s)");
        }

        write!(w, "has bound(s) ")?;

        for (index, item) in self.0.iter().enumerate() {
            if index > 0 {
                write!(w, ", ")?;
            }

            write!(w, "{}", db.render(&item.bound))?;

            write!(w, " (")?;
            if let Some(instance) = &item.instance {
                write!(w, "{}", db.render(&instance.node))?;
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
    pub node: NodeRef,
    pub error: bool,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub source_node: NodeRef,
    pub trait_node: NodeRef,
    pub substitutions: Substitutions,
    pub optional: bool,
}

impl Bound {
    pub fn to_instance(&self) -> Instance {
        Instance {
            node: self.source_node.clone(),
            trait_node: self.trait_node.clone(),
            substitutions: self.substitutions.clone(),
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
