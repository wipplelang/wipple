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
pub struct OperatorPrecedenceStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub operator_span: D::Span,
    pub precedence_span: D::Span,
    pub precedence: OperatorPrecedenceStatementAttributeKind,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a>
    for OperatorPrecedenceStatementAttribute<D>
{
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(OperatorPrecedenceStatementAttribute {
            span: Default::default(),
            operator_span: Default::default(),
            precedence_span: Default::default(),
            precedence: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> OperatorPrecedenceStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for OperatorPrecedenceStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

// TODO: User-defined precedences
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, strum::EnumString)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum OperatorPrecedenceStatementAttributeKind {
    #[strum(serialize = "Casting-Precedence")]
    Casting,
    #[strum(serialize = "Exponentiation-Precedence")]
    Exponentiation,
    #[strum(serialize = "Multiplication-Precedence")]
    Multiplication,
    #[strum(serialize = "Addition-Precedence")]
    Addition,
    #[strum(serialize = "Comparison-Precedence")]
    Comparison,
    #[strum(serialize = "Conjunction-Precedence")]
    Conjunction,
    #[strum(serialize = "Disjunction-Precedence")]
    Disjunction,
    #[strum(serialize = "Accessor-Precedence")]
    Accessor,
    #[strum(serialize = "Composition-Precedence")]
    Composition,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None,
}

impl OperatorPrecedenceStatementAttributeKind {
    pub fn associativity(&self) -> OperatorAssociativity {
        use OperatorAssociativity::*;
        use OperatorPrecedenceStatementAttributeKind::*;

        match self {
            Casting => Left,
            Exponentiation => Right,
            Multiplication => Left,
            Addition => Left,
            Comparison => Left,
            Conjunction => Left,
            Disjunction => Left,
            Accessor => Right,
            Composition => Left,
        }
    }
}

pub struct OperatorPrecedenceStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for OperatorPrecedenceStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "operator",
            |context, span, operator_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`operator` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let precedence = match expr.kind {
                    // TODO: User-defined precedences (make sure to use the name scope)
                    parse::ExprKind::Name(name, _) => match name
                        .as_ref()
                        .parse::<OperatorPrecedenceStatementAttributeKind>(
                    ) {
                        Ok(precedence) => precedence,
                        Err(_) => {
                            context.ast_builder.driver.syntax_error_with([
                                (expr.span, String::from("invalid `operator` precedence")),
                                (
                                    expr.span,
                                    String::from(
                                        "custom operator precedences are not yet supported",
                                    ),
                                ),
                            ]);

                            return Err(context.ast_builder.syntax_error(span));
                        }
                    },
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(expr.span, "expected a name");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = OperatorPrecedenceStatementAttribute {
                    span,
                    operator_span,
                    precedence_span: expr.span,
                    precedence,
                };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .operator_precedence = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
