use crate::{
    ast::{
        format::Format,
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        QueuedTopLevelStatement, TopLevelStatementSyntaxContext,
    },
    parse, Driver,
};
use std::mem;

#[derive(Debug, Clone)]
pub struct UseTopLevelStatement<D: Driver> {
    pub path: Option<D::Path>,
    pub statement: QueuedTopLevelStatement<D>,
}

impl<D: Driver> UseTopLevelStatement<D> {
    pub fn span(&self) -> D::Span {
        self.statement.span
    }
}

impl<D: Driver> Format<D> for UseTopLevelStatement<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("finish parsing the queued statements first")
    }
}

pub struct UseTopLevelStatementSyntax;

impl<D: Driver> Syntax<D> for UseTopLevelStatementSyntax {
    type Context = TopLevelStatementSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "use",
            |context, span, use_span, mut exprs, _scope_set| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`use` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let input = exprs.pop().unwrap();
                match &input.kind {
                    parse::ExprKind::Text(text, _) => {
                        let path = context.ast_builder.driver.make_path(text.clone());

                        Ok(UseTopLevelStatement {
                            path,
                            statement: QueuedTopLevelStatement {
                                span,
                                attributes: mem::take(
                                    &mut context.statement_attributes.unwrap().lock().raw,
                                ),
                                expr: parse::Expr::new(
                                    span,
                                    parse::ExprKind::List(vec![parse::ListLine::from(vec![
                                        parse::Expr::new(
                                            use_span,
                                            parse::ExprKind::Name(
                                                context.ast_builder.driver.intern("use"),
                                                None,
                                            ),
                                        ),
                                        input,
                                    ])]),
                                ),
                            },
                        }
                        .into())
                    }
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "`use` accepts a file name");

                        Err(context.ast_builder.syntax_error(input.span))
                    }
                }
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::USE]
}
