use crate::{
    ast::{
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        StatementAttributes, StatementSyntaxContext,
    },
    parse, Driver, DriverExt,
};

#[derive(Debug, Clone)]
pub struct UseStatement<D: Driver> {
    pub span: D::Span,
    pub use_span: D::Span,
    pub kind: Result<UseStatementKind<D>, SyntaxError<D>>,
    pub attributes: StatementAttributes<D>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for UseStatement<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(UseStatement {
            span: Default::default(),
            use_span: Default::default(),
            // This should prevent `use` statements from being generated
            kind: Err(arbitrary::Arbitrary::arbitrary(u)?),
            attributes: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum UseStatementKind<D: Driver> {
    File(D::Span, D::InternedString, Option<D::Path>),
}

impl<D: Driver> UseStatement<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct UseStatementSyntax;

impl<D: Driver> Syntax<D> for UseStatementSyntax {
    type Context = StatementSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "use",
            |context, span, use_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`use` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let input = exprs.pop().unwrap();
                let kind = match input.kind {
                    parse::ExprKind::Text(text, _) => {
                        let path = context.ast_builder.driver.make_path(text.clone());

                        if let Some(path) = path {
                            context
                                .ast_builder
                                .driver
                                .syntax_of(
                                    Some((
                                        context.ast_builder.path,
                                        context.ast_builder.file.clone(),
                                    )),
                                    Some(span),
                                    path,
                                )
                                .await;
                        }

                        Ok(UseStatementKind::File(input.span, text, path))
                    }
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "`use` accepts a file name");

                        Err(context.ast_builder.syntax_error(input.span))
                    }
                };

                Ok(UseStatement {
                    span,
                    use_span,
                    kind,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}
