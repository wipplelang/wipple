use crate::{
    ast::{
        format::Format,
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct DeriveStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub derive_span: D::Span,
    pub path_span: D::Span,
    pub path: D::InternedString,
    pub name_span: D::Span,
    pub name: D::InternedString,
}

impl<D: Driver> DeriveStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for DeriveStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct DeriveStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for DeriveStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "derive",
            |context, span, derive_span, exprs, _scope| async move {
                if exprs.len() != 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`derive` accepts 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let path = exprs.next().unwrap();
                let path_span = path.span;
                let path = match path.kind {
                    parse::ExprKind::Text(text) => text.ignoring_escaped_underscores(),
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "expected text here");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let name = exprs.next().unwrap();
                let name_span = name.span;
                let name = match name.kind {
                    parse::ExprKind::Text(text) => text.ignoring_escaped_underscores(),
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "expected text here");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = DeriveStatementAttribute {
                    span,
                    derive_span,
                    path_span,
                    path,
                    name_span,
                    name,
                };

                context.statement_attributes.unwrap().lock().derive = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
