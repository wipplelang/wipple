use crate::{
    analysis::expand::{
        syntax::BuiltinSyntaxVisitor, Context, Expander, Expression, ExpressionKind,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::Span,
    FilePath, ScopeId,
};
use async_trait::async_trait;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct UseSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for UseSyntax {
    fn name(self) -> &'static str {
        "use"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("expr")),
            },
        ]
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        context: Option<Context<'_>>,
        _scope: ScopeId,
        expander: &Expander,
    ) -> Expression {
        let expr = vars.remove(&InternedString::new("expr")).unwrap();

        if let Some(Context::Statement(_)) = context {
            if UseSyntax::try_import(None, expr.span, vec![expr.clone()], expander)
                .await
                .is_ok()
            {
                return Expression {
                    span,
                    kind: ExpressionKind::EmptySideEffect,
                };
            }
        }

        Expression {
            span,
            kind: ExpressionKind::Use(Box::new(expr)),
        }
    }
}

impl UseSyntax {
    pub(crate) async fn try_import(
        lhs: Option<&[Expression]>,
        span: Span,
        exprs: Vec<Expression>,
        expander: &Expander,
    ) -> Result<(), ()> {
        if exprs.len() == 1 {
            if let ExpressionKind::Text(path) = exprs.first().unwrap().kind {
                let mut resolved_path = None;
                if let Some(file) =
                    (expander.load)(expander.compiler.clone(), span, FilePath::Path(path)).await
                {
                    resolved_path = Some(file.span.path);
                    expander.add_dependency(file);
                }

                if let Some(lhs) = lhs {
                    // TODO: Merge this logic with destructuring
                    let mut imports = HashMap::new();

                    let mut insert_import = |name, span| {
                        if imports.contains_key(&name) {
                            expander.compiler.add_error(
                                "duplicate import",
                                vec![Note::primary(span, "this name is already imported")],
                            );

                            return;
                        }

                        imports.insert(name, span);
                    };

                    let report_invalid_import = |span| {
                        expander.compiler.add_error(
                            "only names may be specified in a destructuring pattern",
                            vec![Note::primary(span, "expected name here")],
                        );
                    };

                    if lhs.len() == 1 {
                        if let ExpressionKind::Block(statements) = &lhs.first().unwrap().kind {
                            for statement in statements.clone() {
                                match statement.expr.kind {
                                    ExpressionKind::Name(name) => {
                                        insert_import(name, statement.expr.span)
                                    }
                                    ExpressionKind::List(exprs) => {
                                        for expr in exprs {
                                            match expr.kind {
                                                ExpressionKind::Name(name) => {
                                                    insert_import(name, expr.span)
                                                }
                                                ExpressionKind::Error(_) => {}
                                                _ => report_invalid_import(expr.span),
                                            }
                                        }
                                    }
                                    ExpressionKind::Error(_) => {}
                                    _ => report_invalid_import(statement.expr.span),
                                }
                            }
                        }
                    }

                    if let Some(path) = resolved_path {
                        expander.dependencies.lock().get_mut(&path).unwrap().1 = Some(imports);
                    }
                }

                return Ok(());
            }
        }

        Err(())
    }
}
