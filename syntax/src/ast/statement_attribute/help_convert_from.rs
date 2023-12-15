use crate::{
    ast::{
        format::Format,
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxRule, SyntaxRules},
        SyntaxError, Type, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct HelpConvertFromStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub help_convert_from_span: D::Span,
    pub ty: Result<Type<D>, SyntaxError<D>>,
    pub replacement: parse::Expr<D>,
}

impl<D: Driver> HelpConvertFromStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for HelpConvertFromStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct HelpConvertFromStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for HelpConvertFromStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "help-convert-from",
            |context, span, help_convert_from_span, exprs, scope_set| async move {
                if exprs.len() != 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`help-convert-from` accepts 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let ty = exprs.next().unwrap();
                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(
                        TypeSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        ty,
                        scope_set,
                    )
                    .await;

                let replacement = exprs.next().unwrap();

                let attribute = HelpConvertFromStatementAttribute {
                    span,
                    help_convert_from_span,
                    ty,
                    replacement,
                };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .help_convert_from
                    .push(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
