use crate::{
    ast::{
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        AssignmentPattern, AssignmentPatternSyntax, AssignmentPatternSyntaxContext,
        AssignmentValue, AssignmentValueSyntax, AssignmentValueSyntaxContext, NamePattern, Pattern,
        PatternAssignmentPattern, StatementAttributes, StatementSyntaxContext,
    },
    parse, Driver, File,
};
use wipple_util::Shared;

#[derive(Debug, Clone)]
pub struct AssignStatement<D: Driver> {
    pub span: D::Span,
    pub colon_span: D::Span,
    pub pattern: Result<AssignmentPattern<D>, SyntaxError<D>>,
    pub value: Result<AssignmentValue<D>, SyntaxError<D>>,
    pub attributes: StatementAttributes<D>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for AssignStatement<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(AssignStatement {
            span: Default::default(),
            colon_span: Default::default(),
            pattern: arbitrary::Arbitrary::arbitrary(u)?,
            value: arbitrary::Arbitrary::arbitrary(u)?,
            attributes: Default::default(),
        })
    }
}

impl<D: Driver> AssignStatement<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for AssignStatement<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{} : {}",
            self.pattern?.format()?,
            self.value?.format()?,
        ))
    }
}

pub struct AssignStatementSyntax;

impl<D: Driver> Syntax<D> for AssignStatementSyntax {
    type Context = StatementSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            ":",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs_exprs), colon_span, (rhs_span, rhs_exprs), scope| async move {
                let mut declared_name = None;
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let pattern = match &lhs.kind {
                    parse::ExprKind::Name(name, _) => {
                        declared_name = Some(name.clone());

                        Ok(AssignmentPattern::Pattern(PatternAssignmentPattern {
                            pattern: Pattern::Name(NamePattern {
                                span: lhs.span,
                                name: name.clone(),
                                scope,
                            }),
                        }))
                    }
                    _ => {
                        context
                            .ast_builder
                            .build_expr::<AssignmentPatternSyntax>(
                                AssignmentPatternSyntaxContext::new(context.ast_builder.clone())
                                    .with_statement_attributes(
                                        context.statement_attributes.as_ref().unwrap().clone(),
                                    ),
                                lhs,
                                scope,
                            )
                            .await
                    }
                };

                let mut value_context = AssignmentValueSyntaxContext::new(context.ast_builder.clone())
                    .with_statement_attributes(context.statement_attributes.as_ref().unwrap().clone());

                let did_create_syntax = Shared::new(false);
                if let Ok(AssignmentPattern::Pattern(pattern)) = &pattern {
                    if let Pattern::Name(pattern) = &pattern.pattern {
                        value_context = value_context.with_assigned_name(
                            pattern.name.clone(),
                            pattern.span,
                            scope,
                            did_create_syntax.clone()
                        );
                    }
                }

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let value = context
                    .ast_builder
                    .build_expr::<AssignmentValueSyntax>(
                        value_context,
                        rhs,
                        scope,
                    )
                    .await;

                if let Some(name) = declared_name {
                    let did_create_syntax = did_create_syntax.into_unique();
                    if !did_create_syntax {
                        context.ast_builder.file.add_barrier(name, scope);
                    }
                }

                Ok(AssignStatement {
                    span,
                    colon_span,
                    pattern,
                    value,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::ASSIGN]
}
