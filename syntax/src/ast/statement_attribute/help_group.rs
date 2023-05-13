use crate::{
    ast::{
        format::Format,
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct HelpGroupStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub help_group_span: D::Span,
    pub help_group_text_span: D::Span,
    pub help_group_text: D::InternedString,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for HelpGroupStatementAttribute<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(HelpGroupStatementAttribute {
            span: Default::default(),
            help_group_span: Default::default(),
            help_group_text_span: Default::default(),
            help_group_text: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> HelpGroupStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for HelpGroupStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct HelpGroupStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for HelpGroupStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "help-group",
            |context, span, help_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`help-group` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let help_group_text = match expr.kind {
                    parse::ExprKind::Text(text, _) => text,
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(expr.span, "expected text");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = HelpGroupStatementAttribute {
                    span,
                    help_group_span: help_span,
                    help_group_text_span: expr.span,
                    help_group_text,
                };

                context.statement_attributes.unwrap().lock().help_group = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
