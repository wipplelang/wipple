use crate::types::{parse_atomic_type, parse_type, visit_type};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    db::{Db, Node},
    span::Span,
    typecheck::{
        constraints::ty_constraint::TyConstraint,
        ty::{ConstructedTy, Ty},
    },
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionType {
    pub span: Span,
    pub inputs: Vec<AstKey>,
    pub output: AstKey,
}

pub fn parse_function_type(parser: &mut Parser<'_>) -> Result<FunctionType, ParseError> {
    let span = parser.spanned();
    let inputs = parse_function_type_inputs(parser)?;
    let output = parse_type(parser)?;
    Ok(FunctionType {
        span: span(parser),
        inputs,
        output,
    })
}

pub fn parse_function_type_inputs(parser: &mut Parser<'_>) -> Result<Vec<AstKey>, ParseError> {
    let inputs = parser.parse_many(1, parse_atomic_type)?;
    parser.token(TokenKind::FunctionOperator)?;
    parser.commit("in this function type");
    parser.consume_line_breaks();
    Ok(inputs)
}

#[typetag::serde]
impl Visit for FunctionType {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_type(db, node, visitor);

        let inputs = self
            .inputs
            .into_iter()
            .map(|input| visitor.visit(db, &input))
            .collect::<Vec<_>>();

        for &input in &inputs {
            db.graph.edge(input, node, "input");
        }

        let output = visitor.visit(db, &self.output);
        db.graph.edge(output, node, "output");

        visitor.constraint(
            db,
            TyConstraint::new(
                node,
                ConstructedTy::function(
                    inputs.into_iter().map(Ty::Node).collect(),
                    Ty::Node(output),
                ),
            ),
        );
    }
}
