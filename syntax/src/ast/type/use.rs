use crate::{
    ast::{
        format::Format,
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Type, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct UseType<D: Driver> {
    pub span: D::Span,
    pub use_span: D::Span,
    pub kind_span: D::Span,
    pub kind: UseKind,
    pub ty: Result<Box<Type<D>>, SyntaxError<D>>,
}

#[derive(Debug, Clone, Copy, strum::EnumString, strum::Display)]
#[strum(serialize_all = "kebab-case")]
pub enum UseKind {
    Once,
    Borrowed,
}

impl<D: Driver> UseType<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for UseType<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("(use {} {})", self.kind, self.ty?.format()?))
    }
}

pub struct UseTypeSyntax;

impl<D: Driver> Syntax<D> for UseTypeSyntax {
    type Context = TypeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "use",
            |context, span, use_span, exprs, scope_set| async move {
                if exprs.len() != 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`use` accepts 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let kind = exprs.next().unwrap();
                let kind_span = kind.span;
                let kind = match kind.kind {
                    parse::ExprKind::Name(name, _) => match name.as_ref().parse() {
                        Ok(kind) => kind,
                        Err(_) => {
                            context
                                .ast_builder
                                .driver
                                .syntax_error(span, "expected `once` or `borrowed`");

                            return Err(context.ast_builder.syntax_error(span));
                        }
                    },
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "expected `once` or `borrowed`");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(context.clone(), exprs.next().unwrap(), scope_set)
                    .await;

                Ok(UseType {
                    span,
                    use_span,
                    kind_span,
                    kind,
                    ty: ty.map(Box::new),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::END]
}
