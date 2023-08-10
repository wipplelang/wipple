use crate::{
    ast::{
        assignment_value::AssignmentValueSyntaxContext,
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        AssignmentValue, AssignmentValueSyntax, TypePattern, TypePatternSyntax,
        TypePatternSyntaxContext,
    },
    parse, Driver, File,
};
use std::collections::HashSet;
use wipple_util::Shared;

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentValue<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub pattern: Result<TypePattern<D>, SyntaxError<D>>,
    pub value: Result<Box<AssignmentValue<D>>, SyntaxError<D>>,
    pub scope_set: HashSet<D::Scope>,
}

impl<D: Driver> TypeFunctionAssignmentValue<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for TypeFunctionAssignmentValue<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} => {})",
            self.pattern?.format()?,
            self.value?.format()?
        ))
    }
}

pub struct TypeFunctionAssignmentValueSyntax;

impl<D: Driver> Syntax<D> for TypeFunctionAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context,
             span,
             (lhs_span, lhs_exprs),
             arrow_span,
             (rhs_span, rhs_exprs),
             scope_set| async move {
                let mut scope_set = scope_set.lock().clone();
                scope_set.insert(context.ast_builder.file.make_scope());
                let scope_set = Shared::new(scope_set);

                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);

                let pattern = context
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

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);

                let value = context
                    .ast_builder
                    .build_expr::<AssignmentValueSyntax>(context.clone(), rhs, scope_set.clone())
                    .await;

                Ok(TypeFunctionAssignmentValue {
                    span,
                    arrow_span,
                    pattern,
                    value: value.map(Box::new),
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
