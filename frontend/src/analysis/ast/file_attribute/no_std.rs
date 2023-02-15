use crate::{
    analysis::ast::{
        file_attribute::FileAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct NoStdFileAttribute {
    pub span: Span,
}

impl NoStdFileAttribute {
    pub fn span(&self) -> Span {
        self.span
    }
}

pub struct NoStdFileAttributeSyntax;

impl Syntax for NoStdFileAttributeSyntax {
    type Context = FileAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "no-std",
            |context, span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`no-std` does not accept parameters")],
                    );
                }

                let attribute = NoStdFileAttribute { span };

                context.ast_builder.attributes.lock().no_std = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
