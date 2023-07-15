use crate::{
    ast::{
        constant_type_annotation::ConstantTypeAnnotationSyntaxContext,
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        ConstantTypeAnnotation, ConstantTypeAnnotationSyntax, TypePattern, TypePatternSyntax,
        TypePatternSyntaxContext,
    },
    parse, Driver, File,
};

#[derive(Debug, Clone)]
pub struct TypeFunctionConstantTypeAnnotation<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub pattern: Result<TypePattern<D>, SyntaxError<D>>,
    pub annotation: Result<Box<ConstantTypeAnnotation<D>>, SyntaxError<D>>,
    pub scope: D::Scope,
}

impl<D: Driver> TypeFunctionConstantTypeAnnotation<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for TypeFunctionConstantTypeAnnotation<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} => {})",
            self.pattern?.format()?,
            self.annotation?.format()?
        ))
    }
}

pub struct TypeFunctionConstantTypeAnnotationSyntax;

impl<D: Driver> Syntax<D> for TypeFunctionConstantTypeAnnotationSyntax {
    type Context = ConstantTypeAnnotationSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs), arrow_span, (rhs_span, rhs), scope| async move {
                let scope = context.ast_builder.file.make_scope(scope);

                let lhs = parse::Expr::list_or_expr(lhs_span, lhs);

                let pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(
                        TypePatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope,
                    )
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs);

                let annotation = context
                    .ast_builder
                    .build_expr::<ConstantTypeAnnotationSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(TypeFunctionConstantTypeAnnotation {
                    span,
                    arrow_span,
                    pattern,
                    annotation: annotation.map(Box::new),
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
