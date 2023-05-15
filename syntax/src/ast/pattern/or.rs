use crate::{
    ast::{
        format::Format,
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Pattern, PatternSyntax, PatternSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct OrPattern<D: Driver> {
    pub span: D::Span,
    pub or_span: D::Span,
    pub left: Result<Box<Pattern<D>>, SyntaxError<D>>,
    pub right: Result<Box<Pattern<D>>, SyntaxError<D>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for OrPattern<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(OrPattern {
            span: Default::default(),
            or_span: Default::default(),
            left: arbitrary::Arbitrary::arbitrary(u)?,
            right: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> OrPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for OrPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} or {})",
            self.left?.format()?,
            self.right?.format()?,
        ))
    }
}

pub struct OrPatternSyntax;

impl<D: Driver> Syntax<D> for OrPatternSyntax {
    type Context = PatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "or",
            OperatorAssociativity::Left,
            |context, span, (lhs_span, lhs_exprs), or_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let left = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(context.clone(), lhs, scope)
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let right = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(OrPattern {
                    span,
                    or_span,
                    left: left.map(Box::new),
                    right: right.map(Box::new),
                }
                .into())
            },
        ))
    }
}
