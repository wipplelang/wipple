use crate::analysis::expand_v2::{
    operators::OperatorPrecedence,
    syntax::{
        BuiltinOperatorVisitor, BuiltinOperatorVisitorConstructor, Expression, ExpressionKind,
    },
};
use async_trait::async_trait;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct AssignSyntax;

#[async_trait]
impl BuiltinOperatorVisitor for AssignSyntax {
    const PRECEDENCE: OperatorPrecedence = OperatorPrecedence::Assignment;

    const CONSTRUCT: BuiltinOperatorVisitorConstructor =
        BuiltinOperatorVisitorConstructor::Binary(|span, lhs, rhs| Expression {
            span,
            kind: ExpressionKind::Assign(Box::new(lhs), Box::new(rhs)),
        });
}
