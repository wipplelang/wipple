use crate::{
    ast::{
        format::Format,
        statement::StatementSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Statement, StatementAttributes, StatementSyntax, TypePattern, TypePatternSyntax,
        TypePatternSyntaxContext,
    },
    parse, Driver, File,
};

#[derive(Debug, Clone)]
pub struct TypeFunctionStatement<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub pattern: Result<TypePattern<D>, SyntaxError<D>>,
    pub value: Result<Box<Statement<D>>, SyntaxError<D>>,
    pub scope: D::Scope,
    pub attributes: StatementAttributes<D>,
}

impl<D: Driver> TypeFunctionStatement<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for TypeFunctionStatement<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{}{} => {}",
            self.attributes.format()?,
            self.pattern?.format()?,
            self.value?.format()?
        ))
    }
}

pub struct TypeFunctionStatementSyntax;

impl<D: Driver> Syntax<D> for TypeFunctionStatementSyntax {
    type Context = StatementSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs_exprs), arrow_span, (rhs_span, rhs_exprs), scope| async move {
                let scope = context.ast_builder.file.make_scope(scope);

                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);

                let pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(
                        TypePatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope
                    )
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);

                let value = context
                    .ast_builder
                    .build_expr::<StatementSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(TypeFunctionStatement {
                    span,
                    arrow_span,
                    pattern,
                    value: value.map(Box::new),
                    scope,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::TYPE_FUNCTION]
}
