use crate::analysis::expand_v2::{
    operators::OperatorPrecedence,
    syntax::{
        BuiltinOperatorVisitor, BuiltinOperatorVisitorConstructor, Expression, ExpressionKind,
    },
};
use async_trait::async_trait;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct CommaSyntax;

#[async_trait]
impl BuiltinOperatorVisitor for CommaSyntax {
    const PRECEDENCE: OperatorPrecedence = OperatorPrecedence::Assignment;

    const CONSTRUCT: BuiltinOperatorVisitorConstructor =
        BuiltinOperatorVisitorConstructor::Variadic(|span, exprs| Expression {
            span,
            kind: ExpressionKind::Tuple(exprs),
        });
}
