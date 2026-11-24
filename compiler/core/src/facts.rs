use crate::{
    ast::AstKey,
    codegen::hir,
    db::{Db, Fact, Node},
    render::{Comments, Render, RenderCtx},
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Syntax(pub AstKey);

#[typetag::serde]
impl Fact for Syntax {}

impl Render for Syntax {
    fn render_into(&self, db: &Db, _node: Node, ctx: &mut RenderCtx<'_>) {
        ctx.string("at ");
        ctx.string(self.0.get(db).span(db).to_string());
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Codegen(pub Box<dyn hir::Write>);

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

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DebugInfo {
    pub statement: bool,
    pub variable: bool,
}

#[typetag::serde]
impl Fact for DebugInfo {}

impl Render for DebugInfo {}

pub mod description_comments {
    pub const DATA_FLOW: &str = "Data Flow";
    pub const CONSEQUENCES: &str = "Consequences";
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Description {
    pub summary: Vec<DescriptionEntry>,
    pub data_flow: Vec<DescriptionEntry>,
    pub consequences: Vec<DescriptionEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[non_exhaustive]
pub struct DescriptionEntry {
    pub comments: Comments,
    pub is_primary: bool,
    pub matching_node: Option<Node>,
}

#[typetag::serde]
impl Fact for Description {}

impl Render for Description {}

impl DescriptionEntry {
    pub fn new(is_primary: bool, comments: Comments) -> Self {
        DescriptionEntry {
            comments,
            is_primary,
            matching_node: None,
        }
    }

    pub fn primary(comments: Comments) -> Self {
        DescriptionEntry::new(true, comments)
    }

    pub fn secondary(comments: Comments) -> Self {
        DescriptionEntry::new(false, comments)
    }

    pub fn matching_node(mut self, node: Node) -> Self {
        self.matching_node = Some(node);
        self
    }
}
