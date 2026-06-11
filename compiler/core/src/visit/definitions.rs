use crate::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    span::Str,
    visit::attributes::{
        ConnectionAttributeValue, StringAttributeValue, parse_attribute_named,
        parse_attribute_with_value, parse_attributes_with_value,
    },
};
use dyn_clone::DynClone;
use serde::{Deserialize, Serialize};
use std::{any::Any, fmt::Debug};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Defined(pub Box<dyn Definition>);

#[typetag::serde]
impl Fact for Defined {}

impl Render for Defined {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx<'_>) {
        ctx.string("is a definition");
    }
}

#[typetag::serde]
pub trait Definition: Debug + DynClone + Send + Sync + Any {
    fn name(&self) -> Option<Str>;
    fn comments(&self) -> &[Str];
}

dyn_clone::clone_trait_object!(Definition);

impl dyn Definition {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        (self as &dyn Any).downcast_ref()
    }

    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        (self as &mut dyn Any).downcast_mut()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableDefinition {
    pub name: Str,
    pub value: Node,
    pub is_mutable: bool,
}

#[typetag::serde]
impl Definition for VariableDefinition {
    fn name(&self) -> Option<Str> {
        Some(self.name.clone())
    }

    fn comments(&self) -> &[Str] {
        &[]
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstantValue(pub Node);

#[typetag::serde]
impl Fact for ConstantValue {}

impl Render for ConstantValue {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstantDefinition {
    pub name: Str,
    pub comments: Vec<Str>,
    pub attributes: ConstantAttributes,
}

#[typetag::serde]
impl Definition for ConstantDefinition {
    fn name(&self) -> Option<Str> {
        Some(self.name.clone())
    }

    fn comments(&self) -> &[Str] {
        &self.comments
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstantAttributes {
    pub unit: bool,
    pub connect: Vec<ConnectionAttributeValue>,
}

impl ConstantAttributes {
    pub fn parse(db: &mut Db, attributes: &[Node]) -> Self {
        ConstantAttributes {
            unit: parse_attribute_named(db, attributes, "unit"),
            connect: parse_attributes_with_value(db, attributes, "connect"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeDefinition {
    pub name: Str,
    pub comments: Vec<Str>,
    pub attributes: TypeDefinitionAttributes,
    pub parameters: Vec<Node>,
}

#[typetag::serde]
impl Definition for TypeDefinition {
    fn name(&self) -> Option<Str> {
        Some(self.name.clone())
    }

    fn comments(&self) -> &[Str] {
        &self.comments
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeDefinitionAttributes {
    pub intrinsic: bool,
    pub representation: Option<Str>,
}

impl TypeDefinitionAttributes {
    pub fn parse(db: &mut Db, attributes: &[Node]) -> Self {
        TypeDefinitionAttributes {
            intrinsic: parse_attribute_named(db, attributes, "intrinsic"),
            representation: parse_attribute_with_value(db, attributes, "representation")
                .map(|syntax: StringAttributeValue| syntax.value.clone()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitDefinition {
    pub name: Str,
    pub comments: Vec<Str>,
    pub attributes: TraitDefinitionAttributes,
    pub parameters: Vec<Node>,
}

#[typetag::serde]
impl Definition for TraitDefinition {
    fn name(&self) -> Option<Str> {
        Some(self.name.clone())
    }

    fn comments(&self) -> &[Str] {
        &self.comments
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitDefinitionAttributes {}

impl TraitDefinitionAttributes {
    pub fn parse(_db: &mut Db, _attributes: &[Node]) -> Self {
        TraitDefinitionAttributes {}
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceTrait(pub Node);

#[typetag::serde]
impl Fact for InstanceTrait {}

impl Render for InstanceTrait {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceDefinition {
    pub comments: Vec<Str>,
    pub attributes: InstanceDefinitionAttributes,
    pub default: bool,
    pub error: bool,
    pub value: Option<Node>,
}

#[typetag::serde]
impl Definition for InstanceDefinition {
    fn name(&self) -> Option<Str> {
        None
    }

    fn comments(&self) -> &[Str] {
        &self.comments
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceDefinitionAttributes {}

impl InstanceDefinitionAttributes {
    pub fn parse(_db: &mut Db, _attributes: &[Node]) -> Self {
        InstanceDefinitionAttributes {}
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeParameterDefinition {
    pub name: Option<Str>,
}

#[typetag::serde]
impl Definition for TypeParameterDefinition {
    fn name(&self) -> Option<Str> {
        self.name.clone()
    }

    fn comments(&self) -> &[Str] {
        &[]
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarkerConstructorDefinition {
    pub name: Str,
    pub comments: Vec<Str>,
}

#[typetag::serde]
impl Definition for MarkerConstructorDefinition {
    fn name(&self) -> Option<Str> {
        Some(self.name.clone())
    }

    fn comments(&self) -> &[Str] {
        &self.comments
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructureConstructorDefinition {
    pub name: Str,
    pub comments: Vec<Str>,
    pub fields: Vec<(Str, Node)>,
}

#[typetag::serde]
impl Definition for StructureConstructorDefinition {
    fn name(&self) -> Option<Str> {
        Some(self.name.clone())
    }

    fn comments(&self) -> &[Str] {
        &self.comments
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariantConstructorDefinition {
    pub name: Str,
    pub type_definition: Node,
    pub index: usize,
    pub elements: Vec<Node>,
}

#[typetag::serde]
impl Definition for VariantConstructorDefinition {
    fn name(&self) -> Option<Str> {
        Some(self.name.clone())
    }

    fn comments(&self) -> &[Str] {
        &[]
    }
}
