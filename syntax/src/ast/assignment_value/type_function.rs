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

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentValue<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub pattern: Result<TypePattern<D>, SyntaxError<D>>,
    pub value: Result<Box<AssignmentValue<D>>, SyntaxError<D>>,
    pub scope: D::Scope,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for TypeFunctionAssignmentValue<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(TypeFunctionAssignmentValue {
            span: Default::default(),
            arrow_span: Default::default(),
            pattern: arbitrary::Arbitrary::arbitrary(u)?,
            value: arbitrary::Arbitrary::arbitrary(u)?,
            scope: Default::default(),
        })
    }
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
            |context, span, (lhs_span, lhs_exprs), arrow_span, (rhs_span, rhs_exprs), scope| async move {
                let scope = context.ast_builder.file.make_scope(scope);

                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);

                let pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(
                        TypePatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope
                    )
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);

                let value = context
                    .ast_builder
                    .build_expr::<AssignmentValueSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(TypeFunctionAssignmentValue {
                    span,
                    arrow_span,
                    pattern,
                    value: value.map(Box::new),
                    scope,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::TYPE_FUNCTION]
}