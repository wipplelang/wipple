use crate::{
    ast::AstKey,
    codegen::CodegenValue,
    db::{Fact, Node},
    render::Render,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Syntax(pub AstKey);

#[typetag::serde]
impl Fact for Syntax {}

impl Render for Syntax {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Codegen(pub Box<dyn CodegenValue>);

#[typetag::serde]
impl Fact for Codegen {}

impl Render for Codegen {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Parent(pub Node);

#[typetag::serde]
impl Fact for Parent {}

impl Render for Parent {}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Children(pub Vec<Node>);

#[typetag::serde]
impl Fact for Children {}

impl Render for Children {}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GraphType;

#[typetag::serde]
impl Fact for GraphType {}

impl Render for GraphType {}
