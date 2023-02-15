use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct OperatorPrecedenceStatementAttribute {
    pub operator_span: Span,
    pub precedence_span: Span,
    pub precedence: OperatorPrecedenceStatementAttributeKind,
}

impl OperatorPrecedenceStatementAttribute {
    pub fn span(&self) -> Span {
        Span::join(self.operator_span, self.precedence_span)
    }
}

// TODO: User-defined precedences
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, strum::EnumString)]
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

impl Syntax for OperatorPrecedenceStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "operator",
            |context, span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`operator` accepts 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let precedence = match expr.kind {
                    // TODO: User-defined precedences (make sure to use the name scope)
                    parse::ExprKind::Name(name, _) => {
                        match name.parse::<OperatorPrecedenceStatementAttributeKind>() {
                            Ok(precedence) => precedence,
                            Err(_) => {
                                context.ast_builder.compiler.add_error(
                                    "invalid `operator` precedence",
                                    vec![Note::primary(
                                        expr.span,
                                        "custom operator precedences are not yet supported",
                                    )],
                                );

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        }
                    }
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(expr.span, "expected a name")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = OperatorPrecedenceStatementAttribute {
                    operator_span: span,
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
