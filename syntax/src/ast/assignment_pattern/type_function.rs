use crate::ScopeSet;
use crate::{
    ast::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        AssignmentPattern, AssignmentPatternSyntax, TypePattern, TypePatternSyntax,
        TypePatternSyntaxContext,
    },
    parse, Driver, File,
};
use wipple_util::Shared;

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentPattern<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub type_pattern: Result<TypePattern<D>, SyntaxError<D>>,
    pub assignment_pattern: Result<Box<AssignmentPattern<D>>, SyntaxError<D>>,
    pub scope_set: ScopeSet<D::Scope>,
}

impl<D: Driver> Format<D> for TypeFunctionAssignmentPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} => {})",
            self.type_pattern?.format()?,
            self.assignment_pattern?.format()?
        ))
    }
}

impl<D: Driver> TypeFunctionAssignmentPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct TypeFunctionAssignmentPatternSyntax;

impl<D: Driver> Syntax<D> for TypeFunctionAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs), arrow_span, (rhs_span, rhs), scope_set| async move {
                let mut scope_set = scope_set.lock().clone();
                scope_set.insert(context.ast_builder.file.make_scope());
                let scope_set = Shared::new(scope_set);

                let lhs = parse::Expr::list_or_expr(lhs_span, lhs);

                let type_pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(
                        TypePatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope_set.clone(),
                    )
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs);

                let assignment_pattern = context
                    .ast_builder
                    .build_expr::<AssignmentPatternSyntax>(context.clone(), rhs, scope_set.clone())
                    .await;

                Ok(TypeFunctionAssignmentPattern {
                    span,
                    arrow_span,
                    type_pattern,
                    assignment_pattern: assignment_pattern.map(Box::new),
                    scope_set: scope_set.into_unique(),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::TYPE_FUNCTION]
}
