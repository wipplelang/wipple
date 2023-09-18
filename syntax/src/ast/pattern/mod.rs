definitions! {
    mod annotate;
    mod or;
    mod tuple;
}

use crate::ScopeSet;
use crate::{
    ast::{
        format::Format,
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, Destructuring, DestructuringSyntax, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};
use wipple_util::Shared;

syntax_group! {
    pub type Pattern<PatternSyntaxContext> {
        non_terminal: {
            Tuple,
            Annotate,
            Or,
        },
        terminal: {
            Name,
            Text,
            Number,
            Unit,
            Variant,
            Destructure,
            Wildcard,
        },
    }
}

#[derive(Debug, Clone)]
pub struct NamePattern<D: Driver> {
    pub span: D::Span,
    pub name: D::InternedString,
    pub scope: ScopeSet<D::Scope>,
}

impl<D: Driver> NamePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NamePattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(self.name.as_ref().to_string())
    }
}

#[derive(Debug, Clone)]
pub struct TextPattern<D: Driver> {
    pub span: D::Span,
    pub text: parse::Text<D>,
}

impl<D: Driver> TextPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for TextPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("\"{}\"", self.text.raw().as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct NumberPattern<D: Driver> {
    pub span: D::Span,
    pub number: D::InternedString,
}

impl<D: Driver> NumberPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NumberPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(self.number.as_ref().to_string())
    }
}

#[derive(Debug, Clone)]
pub struct UnitPattern<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> UnitPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for UnitPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(String::from("()"))
    }
}

#[derive(Debug, Clone)]
pub struct VariantPattern<D: Driver> {
    pub span: D::Span,
    pub name_span: D::Span,
    pub name_scope_set: ScopeSet<D::Scope>,
    pub name: D::InternedString,
    pub values: Vec<Result<Pattern<D>, SyntaxError<D>>>,
}

impl<D: Driver> VariantPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for VariantPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({}{})",
            self.name.as_ref(),
            self.values
                .into_iter()
                .map(|value| Ok(format!(" {}", value?.format()?)))
                .collect::<Result<String, _>>()?
        ))
    }
}

#[derive(Debug, Clone)]
pub struct DestructurePattern<D: Driver> {
    pub span: D::Span,
    pub destructurings: Vec<Result<Destructuring<D>, SyntaxError<D>>>,
}

impl<D: Driver> DestructurePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for DestructurePattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{{\n{}\n}}",
            self.destructurings
                .into_iter()
                .map(|destructuring| destructuring?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join("\n")
        ))
    }
}

#[derive(Debug, Clone)]
pub struct WildcardPattern<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> WildcardPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for WildcardPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(String::from("_"))
    }
}

#[derive(Clone)]
pub struct PatternSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for PatternSyntaxContext<D> {
    type Body = Pattern<D>;
    type Statement = DestructuringSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        PatternSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes<D>>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }

    async fn build_block(
        self,
        span: D::Span,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        _scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        Ok(DestructurePattern {
            span,
            destructurings: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.kind {
            parse::ExprKind::Name(name, scope) => Ok(NamePattern {
                span: expr.span,
                name,
                scope: scope.unwrap_or_else(|| scope_set.lock().clone()),
            }
            .into()),
            parse::ExprKind::Text(text) => Ok(TextPattern {
                span: expr.span,
                text,
            }
            .into()),
            parse::ExprKind::Number(number) => Ok(NumberPattern {
                span: expr.span,
                number,
            }
            .into()),
            parse::ExprKind::List(_) => {
                let (span, attrs, exprs) = expr.try_into_list_exprs().unwrap();
                self.ast_builder.forbid_attributes(attrs);

                let mut exprs = exprs.into_iter();

                let name_expr = match exprs.next() {
                    Some(expr) => expr,
                    None => return Ok(UnitPattern { span }.into()),
                };

                let (name, name_scope_set) = match name_expr.kind {
                    parse::ExprKind::Name(name, scope) => {
                        (name, scope.unwrap_or_else(|| scope_set.lock().clone()))
                    }
                    _ => {
                        self.ast_builder
                            .driver
                            .syntax_error(name_expr.span, "expected name");

                        return Err(self.ast_builder.syntax_error(span));
                    }
                };

                let values = stream::iter(exprs)
                    .then(|expr| {
                        self.ast_builder.build_expr::<PatternSyntax>(
                            self.clone(),
                            expr,
                            scope_set.clone(),
                        )
                    })
                    .collect::<Vec<_>>()
                    .await;

                Ok(VariantPattern {
                    span,
                    name_span: name_expr.span,
                    name,
                    name_scope_set,
                    values,
                }
                .into())
            }
            parse::ExprKind::Underscore => Ok(WildcardPattern { span: expr.span }.into()),
            _ => {
                self.ast_builder
                    .driver
                    .syntax_error(expr.span, "expected expression");

                Err(self.ast_builder.syntax_error(expr.span))
            }
        }
    }
}
