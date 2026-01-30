use crate::database::{Db, Fact, NodeRef, Render};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct Defined(pub Definition);

impl Fact for Defined {}

impl Render for Defined {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is a definition")
    }
}

#[derive(Debug, Clone)]
pub enum Definition {
    Variable(VariableDefinition),
    Constant(ConstantDefinition),
    Type(TypeDefinition),
    Trait(TraitDefinition),
    Instance(InstanceDefinition),
    TypeParameter(TypeParameterDefinition),
    MarkerConstructor(MarkerConstructorDefinition),
    StructureConstructor(StructureConstructorDefinition),
    VariantConstructor(VariantConstructorDefinition),
}

#[derive(Debug, Clone)]
pub struct VariableDefinition {
    pub name: String,
    pub node: NodeRef,
    pub value: NodeRef,
}

#[derive(Debug, Clone)]
pub struct ConstantDefinition {
    pub name: String,
    pub node: NodeRef,
    pub comments: Vec<String>,
    pub attributes: ConstantAttributes,
    pub value: Option<NodeRef>,
}

#[derive(Debug, Clone)]
pub struct ConstantAttributes {
    pub unit: bool,
}

impl ConstantAttributes {
    pub fn from_attributes(db: &mut Db, attributes: &[NodeRef]) -> Self {
        ConstantAttributes {
            unit: db.contains_name_attribute(attributes, "unit"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDefinition {
    pub name: String,
    pub node: NodeRef,
    pub comments: Vec<String>,
    pub attributes: TypeAttributes,
    pub parameters: Vec<NodeRef>,
}

#[derive(Debug, Clone)]
pub struct TypeAttributes {
    pub intrinsic: bool,
}

impl TypeAttributes {
    pub fn from_attributes(db: &mut Db, attributes: &[NodeRef]) -> Self {
        TypeAttributes {
            intrinsic: db.contains_name_attribute(attributes, "intrinsic"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TraitDefinition {
    pub name: String,
    pub node: NodeRef,
    pub comments: Vec<String>,
    pub attributes: TraitAttributes,
    pub parameters: Vec<NodeRef>,
}

#[derive(Debug, Clone)]
pub struct TraitAttributes {}

#[derive(Debug, Clone)]
pub struct InstanceDefinition {
    pub node: NodeRef,
    pub comments: Vec<String>,
    pub attributes: InstanceAttributes,
    pub value: Option<NodeRef>,
    pub trait_node: NodeRef,
}

#[derive(Debug, Clone)]
pub struct InstanceAttributes {
    pub default: bool,
    pub error: bool,
}

impl InstanceAttributes {
    pub fn from_attributes(db: &mut Db, attributes: &[NodeRef]) -> Self {
        InstanceAttributes {
            default: db.contains_name_attribute(attributes, "default"),
            error: db.contains_name_attribute(attributes, "error"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeParameterDefinition {
    pub name: String,
    pub node: NodeRef,
}

#[derive(Debug, Clone)]
pub struct MarkerConstructorDefinition {
    pub name: String,
    pub node: NodeRef,
    pub comments: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct StructureConstructorDefinition {
    pub name: String,
    pub node: NodeRef,
    pub comments: Vec<String>,
    pub fields: BTreeMap<String, NodeRef>,
}

#[derive(Debug, Clone)]
pub struct VariantConstructorDefinition {
    pub name: String,
    pub node: NodeRef,
    pub index: usize,
}

impl Definition {
    pub fn name(&self) -> Option<&str> {
        match self {
            Definition::Variable(definition) => Some(&definition.name),
            Definition::Constant(definition) => Some(&definition.name),
            Definition::Type(definition) => Some(&definition.name),
            Definition::Trait(definition) => Some(&definition.name),
            Definition::Instance(_) => None,
            Definition::TypeParameter(definition) => Some(&definition.name),
            Definition::MarkerConstructor(definition) => Some(&definition.name),
            Definition::StructureConstructor(definition) => Some(&definition.name),
            Definition::VariantConstructor(definition) => Some(&definition.name),
        }
    }

    pub fn node(&self) -> NodeRef {
        match self {
            Definition::Variable(definition) => definition.node.clone(),
            Definition::Constant(definition) => definition.node.clone(),
            Definition::Type(definition) => definition.node.clone(),
            Definition::Trait(definition) => definition.node.clone(),
            Definition::Instance(definition) => definition.node.clone(),
            Definition::TypeParameter(definition) => definition.node.clone(),
            Definition::MarkerConstructor(definition) => definition.node.clone(),
            Definition::StructureConstructor(definition) => definition.node.clone(),
            Definition::VariantConstructor(definition) => definition.node.clone(),
        }
    }

    pub fn comments(&self) -> &[String] {
        match self {
            Definition::Variable(_) => &[],
            Definition::Constant(definition) => &definition.comments,
            Definition::Type(definition) => &definition.comments,
            Definition::Trait(definition) => &definition.comments,
            Definition::Instance(definition) => &definition.comments,
            Definition::TypeParameter(_) => &[],
            Definition::MarkerConstructor(definition) => &definition.comments,
            Definition::StructureConstructor(definition) => &definition.comments,
            Definition::VariantConstructor(_) => &[],
        }
    }
}

impl From<VariableDefinition> for Definition {
    fn from(definition: VariableDefinition) -> Self {
        Definition::Variable(definition)
    }
}

impl From<ConstantDefinition> for Definition {
    fn from(definition: ConstantDefinition) -> Self {
        Definition::Constant(definition)
    }
}

impl From<TypeDefinition> for Definition {
    fn from(definition: TypeDefinition) -> Self {
        Definition::Type(definition)
    }
}

impl From<TraitDefinition> for Definition {
    fn from(definition: TraitDefinition) -> Self {
        Definition::Trait(definition)
    }
}

impl From<InstanceDefinition> for Definition {
    fn from(definition: InstanceDefinition) -> Self {
        Definition::Instance(definition)
    }
}

impl From<TypeParameterDefinition> for Definition {
    fn from(definition: TypeParameterDefinition) -> Self {
        Definition::TypeParameter(definition)
    }
}

impl From<MarkerConstructorDefinition> for Definition {
    fn from(definition: MarkerConstructorDefinition) -> Self {
        Definition::MarkerConstructor(definition)
    }
}

impl From<StructureConstructorDefinition> for Definition {
    fn from(definition: StructureConstructorDefinition) -> Self {
        Definition::StructureConstructor(definition)
    }
}

impl From<VariantConstructorDefinition> for Definition {
    fn from(definition: VariantConstructorDefinition) -> Self {
        Definition::VariantConstructor(definition)
    }
}
