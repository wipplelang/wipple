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
pub struct NoStdFileAttribute<D: Driver> {
    pub span: D::Span,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for NoStdFileAttribute<D> {
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(NoStdFileAttribute {
            span: Default::default(),
        })
    }
}

impl<D: Driver> NoStdFileAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NoStdFileAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(String::from("[[no-std]]"))
    }
}

pub struct NoStdFileAttributeSyntax;

impl<D: Driver> Syntax<D> for NoStdFileAttributeSyntax {
    type Context = FileAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "no-std",
            |context, span, _no_std_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`no-std` does not accept parameters");
                }

                let attribute = NoStdFileAttribute { span };

                context.ast_builder.attributes.lock().no_std = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}