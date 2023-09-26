use crate::{
    ast::{
        file_attribute::FileAttributeSyntaxContext,
        format::Format,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct NoImplicitUseFileAttribute<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> NoImplicitUseFileAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NoImplicitUseFileAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(String::from("[[no-implicit-use]]"))
    }
}

pub struct NoImplicitUseFileAttributeSyntax;

impl<D: Driver> Syntax<D> for NoImplicitUseFileAttributeSyntax {
    type Context = FileAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "no-implicit-use",
            |context, span, _no_std_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`no-implicit_use` does not accept parameters");
                }

                let attribute = NoImplicitUseFileAttribute { span };

                context.ast_builder.attributes.lock().no_implicit_use = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
