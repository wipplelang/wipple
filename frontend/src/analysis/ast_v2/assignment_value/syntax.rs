use crate::{
    analysis::ast_v2::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
};

#[derive(Debug, Clone)]
pub struct SyntaxAssignmentValue;

pub struct SyntaxAssignmentValueSyntax;

impl Syntax for SyntaxAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "syntax",
            |context, span, _exprs| async move {
                context.ast_builder.compiler.add_error(
                    "syntax error",
                    vec![Note::primary(
                        span,
                        "`syntax` definitions are not yet supported",
                    )],
                );

                Err(context.ast_builder.syntax_error(span))
            },
        ))
    }
}
