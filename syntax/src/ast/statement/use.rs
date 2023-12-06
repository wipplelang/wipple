use crate::{
    ast::{
        format::Format,
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        StatementAttributes, StatementSyntaxContext,
    },
    parse, Driver, DriverExt,
};

#[derive(Debug, Clone)]
pub struct UseStatement<D: Driver> {
    pub span: D::Span,
    pub use_span: D::Span,
    pub kind: Result<UseStatementKind<D>, SyntaxError<D>>,
    pub attributes: StatementAttributes<D>,
}

#[derive(Debug, Clone)]
pub enum UseStatementKind<D: Driver> {
    File(D::Span, parse::Text<D>, D::InternedString),
}

impl<D: Driver> UseStatement<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for UseStatement<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{}use {}",
            self.attributes.format()?,
            match self.kind? {
                UseStatementKind::File(_, file, _) => format!("\"{}\"", file.raw().as_ref()),
            }
        ))
    }
}

pub struct UseStatementSyntax;

impl<D: Driver> Syntax<D> for UseStatementSyntax {
    type Context = StatementSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "use",
            |context, span, use_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`use` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let input = exprs.pop().unwrap();
                let kind = match input.kind {
                    parse::ExprKind::Text(text) => {
                        let path = text.ignoring_escaped_underscores().clone();

                        context
                            .ast_builder
                            .driver
                            .syntax_of(
                                Some(context.ast_builder.path.clone()),
                                Some(context.ast_builder.file.clone()),
                                Some(span),
                                path.clone(),
                                context.ast_builder.options,
                            )
                            .await;

                        Ok(UseStatementKind::File(input.span, text, path))
                    }
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "`use` accepts a file name");

                        Err(context.ast_builder.syntax_error(input.span))
                    }
                };

                Ok(UseStatement {
                    span,
                    use_span,
                    kind,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::USE]
}
