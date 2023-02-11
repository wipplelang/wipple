use crate::{
    analysis::ast_v2::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        TypeMember, TypeMemberSyntax,
    },
    diagnostics::Note,
    parse,
};

#[derive(Debug, Clone)]
pub struct TypeAssignmentValue {
    pub members: Option<Vec<Result<TypeMember, SyntaxError>>>,
}

pub struct TypeAssignmentValueSyntax;

impl Syntax for TypeAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "type",
            |context, span, mut exprs| async move {
                match exprs.len() {
                    0 => Ok(TypeAssignmentValue { members: None }.into()),
                    1 => {
                        let expr = exprs.pop().unwrap();

                        let statements = match expr.kind {
                            parse::ExprKind::Block(statements) => statements,
                            _ => {
                                context.ast_builder.compiler.add_error(
                                    "syntax error",
                                    vec![Note::primary(span, "expected a block")],
                                );

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        };

                        let members = context
                            .ast_builder
                            .build_statements::<TypeMemberSyntax>(statements)
                            .await
                            .collect();

                        Ok(TypeAssignmentValue {
                            members: Some(members),
                        }
                        .into())
                    }
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(span, "`type` accepts 1 input")],
                        );

                        Err(context.ast_builder.syntax_error(span))
                    }
                }
            },
        ))
    }
}
