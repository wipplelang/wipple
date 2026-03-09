mod attributes;
mod constraints;
mod expressions;
mod patterns;
mod statements;
mod types;

pub use attributes::*;
pub use constraints::*;
pub use expressions::*;
pub use patterns::*;
pub use statements::*;
pub use types::*;

use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{Node, NodeRef},
    syntax::{ParseError, Parser},
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct FileNode {
    pub statements: Vec<NodeRef>,
}

impl Node for FileNode {
    fn is_hidden(&self) -> bool {
        true
    }
}

pub fn parse_file(parser: &mut Parser<'_>) -> Result<FileNode, ParseError> {
    let statements = parse_statements(parser)?;
    let _ = parse_comments(parser)?;

    Ok(FileNode { statements })
}

impl Visit for FileNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        for statement in &self.statements {
            visitor.visit(statement);
            visitor.edge(statement, node, "statement");
        }
    }
}

impl Codegen for FileNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        let mut statements = Vec::new();
        for statement in &self.statements {
            statements.push(ir::Expression::Trace.at(statement, ctx));
            statements.push(ctx.codegen(statement)?);
        }

        Some(ir::Expression::Sequence(statements).at(node, ctx))
    }
}
