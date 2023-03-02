use crate::{
    analysis::ast::{
        file_attribute::FileAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::SpanList,
};

#[derive(Debug, Clone)]
pub struct NoStdFileAttribute {
    pub span: SpanList,
}

impl NoStdFileAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct NoStdFileAttributeSyntax;

impl Syntax for NoStdFileAttributeSyntax {
    type Context = FileAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "no-std",
            |context, span, _no_std_span, exprs, _scope| async move {
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
