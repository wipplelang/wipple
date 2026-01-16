use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Db, Fact, Node, NodeRef, Render, Span},
    visit::{Visit, Visitor},
};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub reason: Option<String>,
    pub committed: Option<String>,
    pub span: Span,
}

impl Fact for ParseError {}

impl Render for ParseError {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "{}", self.message)?;

        if let Some(committed) = &self.committed {
            write!(w, " {}", committed)?;
        }

        Ok(())
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if cfg!(test) {
            write!(f, "{}: ", self.span)?;
        }

        write!(f, "{}", self.message)?;

        if let Some(committed) = &self.committed {
            write!(f, " {}", committed)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct ParseErrorNode;

impl Node for ParseErrorNode {}

impl Visit for ParseErrorNode {
    fn visit(&self, _node: &NodeRef, _visitor: &mut Visitor<'_>) {}
}

impl Codegen for ParseErrorNode {
    fn codegen(&self, codegen: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Err(codegen.error())
    }
}
