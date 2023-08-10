use crate::{
    ast::{
        format::Format,
        r#type::{Type, TypeSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        TypeSyntax,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct FunctionType<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub input: Result<Box<Type<D>>, SyntaxError<D>>,
    pub output: Result<Box<Type<D>>, SyntaxError<D>>,
}

impl<D: Driver> FunctionType<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for FunctionType<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} -> {})",
            self.input?.format()?,
            self.output?.format()?,
        ))
    }
}

pub struct FunctionTypeSyntax;

impl<D: Driver> Syntax<D> for FunctionTypeSyntax {
    type Context = TypeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, span, (lhs_span, lhs_exprs), arrow_span, (rhs_span, rhs_exprs), scope_set| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let input = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(context.clone(), lhs, scope_set.clone())
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let output = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(context.clone(), rhs, scope_set)
                    .await;

                Ok(FunctionType {
                    span,
                    arrow_span,
                    input: input.map(Box::new),
                    output: output.map(Box::new),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::FUNCTION]
}
