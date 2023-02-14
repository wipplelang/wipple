use crate::{
    analysis::ast::{
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes, StatementSyntax,
    },
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
    ScopeId,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};

syntax_group! {
    #[derive(Debug, Clone)]
    pub type SyntaxPattern<SyntaxPatternSyntaxContext> {
        non_terminal: {},
        terminal: {
            Unit,
            Name,
            Text,
            Number,
            Variable,
            VariableRepetition,
            List,
            ListRepetition,
        },
    }
}

#[derive(Debug, Clone)]
pub struct UnitSyntaxPattern {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NameSyntaxPattern {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct TextSyntaxPattern {
    pub span: Span,
    pub text: InternedString,
}

#[derive(Debug, Clone)]
pub struct NumberSyntaxPattern {
    pub span: Span,
    pub number: InternedString,
}

#[derive(Debug, Clone)]
pub struct VariableSyntaxPattern {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct VariableRepetitionSyntaxPattern {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct ListSyntaxPattern {
    pub span: Span,
    pub patterns: Vec<Result<SyntaxPattern, SyntaxError>>,
}

#[derive(Debug, Clone)]
pub struct ListRepetitionSyntaxPattern {
    pub span: Span,
    pub patterns: Vec<Result<SyntaxPattern, SyntaxError>>,
}

#[derive(Clone)]
pub struct SyntaxPatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for SyntaxPatternSyntaxContext {
    type Body = SyntaxPattern;
    type Statement = StatementSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        SyntaxPatternSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    async fn build_block(
        self,
        span: parse::Span,
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(
                span,
                "blocks in syntax patterns are not yet supported",
            )],
        );

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let patterns = stream::iter(exprs)
                    .then(|expr| {
                        self.ast_builder.build_expr::<SyntaxPatternSyntax>(
                            self.clone(),
                            expr,
                            scope,
                        )
                    })
                    .collect::<Vec<_>>()
                    .await;

                Ok(ListSyntaxPattern { span, patterns }.into())
            }
            Err(expr) => match expr.try_into_list_repetition_exprs() {
                Ok((span, exprs)) => {
                    let patterns = stream::iter(exprs)
                        .then(|expr| {
                            self.ast_builder.build_expr::<SyntaxPatternSyntax>(
                                self.clone(),
                                expr,
                                scope,
                            )
                        })
                        .collect::<Vec<_>>()
                        .await;

                    Ok(ListRepetitionSyntaxPattern { span, patterns }.into())
                }
                Err(expr) => match expr.kind {
                    parse::ExprKind::Name(name, _) => Ok(NameSyntaxPattern {
                        span: expr.span,
                        name,
                    }
                    .into()),
                    parse::ExprKind::QuoteName(name) => Ok(VariableSyntaxPattern {
                        span: expr.span,
                        name,
                    }
                    .into()),
                    parse::ExprKind::RepeatName(name) => Ok(VariableRepetitionSyntaxPattern {
                        span: expr.span,
                        name,
                    }
                    .into()),
                    parse::ExprKind::Text(text) => Ok(TextSyntaxPattern {
                        span: expr.span,
                        text,
                    }
                    .into()),
                    parse::ExprKind::Number(number) => Ok(NumberSyntaxPattern {
                        span: expr.span,
                        number,
                    }
                    .into()),
                    _ => {
                        self.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(expr.span, "expected syntax pattern")],
                        );

                        Err(self.ast_builder.syntax_error(expr.span))
                    }
                },
            },
        }
    }
}

impl FileBodySyntaxContext for SyntaxPatternSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
