use crate::{
    ast::AstKey,
    codegen::hir,
    db::{Db, Fact, Node},
    render::{Comments, Render, RenderCtx},
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Syntax(pub AstKey);

#[typetag::serde]
impl Fact for Syntax {}

impl Render for Syntax {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
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

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Description(BTreeMap<DescriptionKind, Vec<DescriptionEntry>>);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum DescriptionKind {
    #[default]
    Summary,
    DataFlow,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DescriptionEntry {
    pub comments: Comments,
    pub is_primary: bool,
}

#[typetag::serde]
impl Fact for Description {}

impl Render for Description {}

impl Description {
    pub fn entries(&self, kind: DescriptionKind) -> Option<&[DescriptionEntry]> {
        self.0.get(&kind).map(|entries| entries.as_slice())
    }

    pub fn insert(&mut self, kind: DescriptionKind, entry: DescriptionEntry) {
        self.0.entry(kind).or_default().push(entry);
    }
}

impl DescriptionKind {
    pub fn comments_section(&self) -> Option<&'static str> {
        match self {
            DescriptionKind::Summary => None,
            DescriptionKind::DataFlow => Some("Data Flow"),
        }
    }
}
