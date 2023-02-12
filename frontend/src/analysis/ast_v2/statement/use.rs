use crate::{
    analysis::ast_v2::{
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        StatementAttributes, StatementSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, Span},
    FilePath,
};

#[derive(Debug, Clone)]
pub struct UseStatement {
    pub use_span: Span,
    pub kind: Result<UseStatementKind, SyntaxError>,
    pub attributes: StatementAttributes,
}

#[derive(Debug, Clone)]
pub enum UseStatementKind {
    File(Span, InternedString, Option<FilePath>),
    Name(Span, InternedString),
}

pub struct UseStatementSyntax;

impl Syntax for UseStatementSyntax {
    type Context = StatementSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "use",
            |context, span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`use` accepts 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let input = exprs.pop().unwrap();
                let kind = match input.kind {
                    parse::ExprKind::Name(name) => Ok(UseStatementKind::Name(input.span, name)),
                    parse::ExprKind::Text(text) => {
                        let mut resolved_path = None;
                        if let Some(file) = (context.ast_builder.load)(
                            context.ast_builder.compiler.clone(),
                            span,
                            FilePath::Path(text),
                        )
                        .await
                        {
                            resolved_path = Some(file.span.path);
                            context.ast_builder.add_dependency(file);
                        }

                        Ok(UseStatementKind::File(input.span, text, resolved_path))
                    }
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(span, "`use` accepts 1 input")],
                        );

                        Err(context.ast_builder.syntax_error(input.span))
                    }
                };

                Ok(UseStatement {
                    use_span: span,
                    kind,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}
