use super::*;
use std::collections::VecDeque;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[repr(u8)]
pub enum OperatorPrecedence {
    Addition = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None,
}

impl OperatorPrecedence {
    pub const fn associativity(&self) -> OperatorAssociativity {
        match self {
            OperatorPrecedence::Addition => OperatorAssociativity::Left,
        }
    }
}

impl<L: Loader> Compiler<L> {
    pub(super) fn lower_list(
        &mut self,
        span: Span,
        list: Vec<ast::Expression>,
        scope: &Scope,
        info: &mut Info,
    ) -> Expression {
        if let (ast::ExpressionKind::Name(ty_name), ast::ExpressionKind::Block(statements)) =
            (&list[0].kind, &list[1].kind)
        {
            let fields = statements
                .iter()
                .filter_map(|s| match &s.kind {
                    ast::StatementKind::Assign(pattern, expr) => match &pattern.kind {
                        ast::PatternKind::Name(name) => Some((*name, expr)),
                        _ => None,
                    },
                    _ => None,
                })
                .collect::<Vec<_>>();

            match scope.get(*ty_name, list[0].span) {
                Some(ScopeValue::Type(ty)) => {
                    if fields.len() == statements.len() {
                        return self.lower_instantiation(
                            list[0].span,
                            list[1].span,
                            ty,
                            fields,
                            scope,
                            info,
                        );
                    }
                }
                Some(ScopeValue::TypeParameter(_)) => {
                    self.diagnostics.add(Diagnostic::error(
                        "cannot instantiate type parameter",
                        vec![Note::primary(
                            list[0].span,
                            "the actual type this represents is not known here",
                        )],
                    ));

                    return Expression::error(span);
                }
                Some(ScopeValue::BuiltinType(_)) => {
                    self.diagnostics.add(Diagnostic::error(
                        "cannot instantiate builtin type",
                        vec![Note::primary(list[0].span, "try usng a literal instead")],
                    ));
                }
                _ => {}
            }
        }

        let mut list = VecDeque::from(list);
        let first = list.pop_front().unwrap();

        list.into_iter()
            .fold(self.lower_expr(first, scope, info), |result, next| {
                Expression {
                    span: Span::join(result.span, next.span),
                    kind: ExpressionKind::Call(
                        Box::new(result),
                        Box::new(self.lower_expr(next, scope, info)),
                    ),
                }
            })
    }
}
