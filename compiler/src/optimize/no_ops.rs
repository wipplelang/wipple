use crate::{
    diagnostics::*,
    optimize::{Expression, ExpressionKind, PatternKind, Program},
    Compiler,
};
use std::mem;

impl<L> Compiler<L> {
    pub(super) fn optimize_no_ops(&mut self, program: &mut Program) {
        for constant in program.constants.values_mut() {
            constant.traverse(|expr| {
                if let ExpressionKind::Block(statements) = &mut expr.kind {
                    self.remove_no_op_statements(statements, false);
                }
            });
        }

        self.remove_no_op_statements(&mut program.body, true);
    }

    fn remove_no_op_statements(&mut self, statements: &mut Vec<Expression>, check_last: bool) {
        let mut warn_no_op = |statement: &Expression| {
            if let Some(span) = statement.span {
                self.diagnostics.add(Diagnostic::warning(
                    "statement doesn't do anything",
                    vec![Note::primary(span, "try removing this statement")],
                ))
            }
        };

        let mut s = mem::take(statements);
        let last = match s.pop() {
            Some(last) => last,
            None => return,
        };

        let last_is_no_op = is_no_op(&last);
        if check_last && last_is_no_op {
            warn_no_op(&last);
        }

        *statements = s
            .into_iter()
            .filter(|statement| {
                let is_no_op = is_no_op(statement);
                if is_no_op {
                    warn_no_op(statement);
                }

                !is_no_op
            })
            .chain((!last_is_no_op).then(|| last))
            .collect();
    }
}

fn is_no_op(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::Marker
        | ExpressionKind::Variable(_)
        | ExpressionKind::Constant(_)
        | ExpressionKind::Text(_)
        | ExpressionKind::Number(_)
        | ExpressionKind::Function(_, _) => true,
        ExpressionKind::Call(_, _)
        | ExpressionKind::External(_, _, _, _)
        | ExpressionKind::Initialize(_, _)
        | ExpressionKind::Return(_)
        | ExpressionKind::Loop(_)
        | ExpressionKind::Break(_)
        | ExpressionKind::Continue => false,
        ExpressionKind::Block(values)
        | ExpressionKind::Structure(values)
        | ExpressionKind::Variant(_, values)
        | ExpressionKind::ListLiteral(values) => values.iter().all(is_no_op),
        ExpressionKind::When(input, arms) => {
            is_no_op(input)
                && arms.iter().all(|arm| {
                    is_no_op(&arm.body)
                        && match &arm.pattern.kind {
                            PatternKind::Where(_, condition) => is_no_op(condition),
                            _ => true,
                        }
                })
        }
    }
}
