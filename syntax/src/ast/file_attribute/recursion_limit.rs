use crate::{
    ast::{
        file_attribute::FileAttributeSyntaxContext,
        format::Format,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct RecursionLimitFileAttribute<D: Driver> {
    pub span: D::Span,
    pub recursion_limit_span: D::Span,
    pub limit_span: D::Span,
    pub limit: usize,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for RecursionLimitFileAttribute<D> {
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(RecursionLimitFileAttribute {
            span: Default::default(),
            recursion_limit_span: Default::default(),
            limit_span: Default::default(),
            limit: 64, // FIXME: Use `DEFAULT_RECURSION_LIMIT` instead
        })
    }
}

impl<D: Driver> Format<D> for RecursionLimitFileAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("[[recursion-limit {}]]", self.limit))
    }
}

impl<D: Driver> RecursionLimitFileAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct RecursionLimitFileAttributeSyntax;

impl<D: Driver> Syntax<D> for RecursionLimitFileAttributeSyntax {
    type Context = FileAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "recursion-limit",
            |context, span, recursion_limit_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`recursion-limit` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let limit = match expr.kind {
                    parse::ExprKind::Number(number) => match number.as_ref().parse::<usize>() {
                        Ok(limit) => limit,
                        Err(_) => {
                            context
                                .ast_builder
                                .driver
                                .syntax_error(expr.span, "expected a positive integer");

                            return Err(context.ast_builder.syntax_error(span));
                        }
                    },
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(expr.span, "expected a number");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = RecursionLimitFileAttribute {
                    span,
                    recursion_limit_span,
                    limit_span: expr.span,
                    limit,
                };

                context.ast_builder.attributes.lock().recursion_limit = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
