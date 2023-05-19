use crate::{
    ast::{
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Pattern, PatternSyntax, PatternSyntaxContext, Type, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct AnnotatePattern<D: Driver> {
    pub span: D::Span,
    pub colon_span: D::Span,
    pub pattern: Result<Box<Pattern<D>>, SyntaxError<D>>,
    pub ty: Result<Type<D>, SyntaxError<D>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for AnnotatePattern<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(AnnotatePattern {
            span: Default::default(),
            colon_span: Default::default(),
            pattern: arbitrary::Arbitrary::arbitrary(u)?,
            ty: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> AnnotatePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for AnnotatePattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} :: {})",
            self.pattern?.format()?,
            self.ty?.format()?,
        ))
    }
}

pub struct AnnotatePatternSyntax;

impl<D: Driver> Syntax<D> for AnnotatePatternSyntax {
    type Context = PatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "::",
            OperatorAssociativity::Left,
            |context, span, (lhs_span, lhs_exprs), colon_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let pattern = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(context.clone(), lhs, scope)
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(
                        TypeSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope,
                    )
                    .await;

                Ok(AnnotatePattern {
                    span,
                    colon_span,
                    pattern: pattern.map(Box::new),
                    ty,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::ANNOTATE]
}
