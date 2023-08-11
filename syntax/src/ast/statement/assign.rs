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
            |context,
             span,
             (lhs_span, lhs_exprs),
             colon_span,
             (rhs_span, rhs_exprs),
             mut scope_set| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs.clone());

                let mut value_context =
                    AssignmentValueSyntaxContext::new(context.ast_builder.clone())
                        .with_statement_attributes(
                            context.statement_attributes.as_ref().unwrap().clone(),
                        );

                let did_create_syntax = Shared::new(false);
                if let parse::ExprKind::Name(name, _) = &lhs.kind {
                    value_context = value_context.with_assigned_name(
                        name.clone(),
                        scope_set.clone(),
                        did_create_syntax.clone(),
                    );
                }

                let constant_scope = match &lhs.kind {
                    parse::ExprKind::Name(name, scope) => context
                        .ast_builder
                        .file
                        .resolve_constant_body(
                            name.clone(),
                            scope.clone().unwrap_or_else(|| scope_set.lock().clone()),
                        )
                        .ok(),
                    _ => None,
                };

                // HACK: Allow plain variables to shadow previous names. This
                // works because all `AssignmentValue`s (except for plain
                // expressions) may only be declared once per scope
                //
                // FIXME: This may cause confusion if you define eg. a type
                // after defining a variable -- that type won't be visible to
                // previous code because of this scoping rule. Currently you
                // will have to declare all types/constants/etc. before any
                // top-level variable assignments
                if let Some(constant_scope) = constant_scope {
                    scope_set = Shared::new(constant_scope);
                } else if !context
                    .ast_builder
                    .list_matches_syntax::<AssignmentPatternSyntax>(lhs_exprs.clone())
                    && !context
                        .ast_builder
                        .list_matches_syntax::<AssignmentValueSyntax>(rhs_exprs.clone())
                {
                    scope_set
                        .lock()
                        .insert(context.ast_builder.file.make_scope());
                }

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);

                let value = context
                    .ast_builder
                    .build_expr::<AssignmentValueSyntax>(value_context, rhs, scope_set.clone())
                    .await;

                let pattern = match &lhs.kind {
                    parse::ExprKind::Name(name, scope) => {
                        Ok(AssignmentPattern::Pattern(PatternAssignmentPattern {
                            pattern: Pattern::Name(NamePattern {
                                span: lhs.span,
                                name: name.clone(),
                                scope: scope.clone().unwrap_or_else(|| scope_set.lock().clone()),
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
                                scope_set.clone(),
                            )
                            .await
                    }
                };

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
