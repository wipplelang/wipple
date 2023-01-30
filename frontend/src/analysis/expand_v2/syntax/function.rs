use crate::analysis::expand_v2::{
    operators::OperatorPrecedence,
    syntax::{
        BuiltinOperatorVisitor, BuiltinOperatorVisitorConstructor, Expression, ExpressionKind,
    },
};
use async_trait::async_trait;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct FunctionSyntax;

#[async_trait]
impl BuiltinOperatorVisitor for FunctionSyntax {
    const PRECEDENCE: OperatorPrecedence = OperatorPrecedence::Assignment;

    const CONSTRUCT: BuiltinOperatorVisitorConstructor =
        BuiltinOperatorVisitorConstructor::Binary(|span, lhs, rhs| Expression {
            span,
            kind: ExpressionKind::Function(Box::new(lhs), Box::new(rhs)),
        });
}
