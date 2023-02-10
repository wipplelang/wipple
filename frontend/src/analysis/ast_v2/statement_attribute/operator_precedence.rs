use crate::analysis::ast_v2::{
    statement_attribute::StatementAttributeSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

// TODO: User-defined precedences
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, strum::EnumString)]
pub enum OperatorPrecedenceStatementAttribute {
    #[strum(serialize = "Cast-Precedence")]
    Cast,
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
    Dot,
}

pub struct OperatorPrecedenceStatementAttributeSyntax;

impl Syntax for OperatorPrecedenceStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "operator",
            |context, span, exprs| async move {
                todo!();
            },
        ))
    }
}
