use crate::statements::{parse_comments, parse_statements};

use serde::{Deserialize, Serialize};
use wipple_core::{
    anyhow,
    codegen::{CodegenCtx, CodegenError, ir},
    db::{Db, Node},
    facts::Syntax,
    span::Span,
    visit::{Visit, Visitor},
};
use wipple_parse::parser::Parser;

pub use wipple_parse::parser::ParseError;

pub fn parse_file(parser: &mut Parser) -> Result<File, ParseError> {
    let span = parser.spanned();
    let statements = parse_statements(parser)?;
    let _ = parse_comments(parser)?;
    Ok(File {
        span: span(parser),
        statements,
        error: None,
    })
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct File {
    pub span: Span,
    pub statements: Vec<Box<dyn Visit>>,
    pub error: Option<ParseError>,
}

impl From<ParseError> for File {
    fn from(error: ParseError) -> Self {
        File {
            span: error.span.clone(),
            statements: Vec::new(),
            error: Some(error),
        }
    }
}

#[typetag::serde]
impl Visit for File {
    fn span(&self) -> &Span {
        &self.span
    }

    fn is_hidden(&self) -> bool {
        true
    }

    fn visit(self: Box<Self>, db: &mut Db, _node: Node, visitor: &mut Visitor) {
        let statements = visitor.visit_statements(db, self.statements);

        for (statement_node, statement) in statements.into_iter() {
            visitor.top_level_statement(statement_node, statement.clone());
        }
    }
}

pub fn codegen_top_level_statements(
    db: &Db,
    ctx: &mut CodegenCtx,
    statements: impl IntoIterator<Item = Node>,
) -> Result<(), CodegenError> {
    for statement in statements {
        let span = db
            .get(statement)
            .map(|Syntax(syntax)| syntax.span().clone())
            .ok_or_else(|| anyhow::format_err!("missing span"))?;

        ctx.instruction(ir::Instruction::Trace { span });

        ctx.codegen(db, statement)?;
    }

    Ok(())
}
