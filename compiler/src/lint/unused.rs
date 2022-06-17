use crate::{compile, diagnostics::*, Compiler};
use std::collections::BTreeSet;

impl<L> Compiler<L> {
    pub fn lint_unused(&mut self, program: &compile::Program) {
        let mut used_variables = BTreeSet::new();
        let mut find_used_variables = |expr: &compile::typecheck::Expression| {
            expr.traverse(|expr| {
                if let compile::typecheck::ExpressionKind::Variable(var) = &expr.kind {
                    used_variables.insert(*var);
                }
            })
        };

        for statement in &program.body {
            find_used_variables(statement);
        }

        for constant in program.declarations.generic_constants.values() {
            find_used_variables(&constant.decl.value);
        }

        program
            .declarations
            .variables
            .iter()
            .filter_map(|(var, decl)| (!used_variables.contains(var)).then(|| decl))
            .for_each(|decl| {
                self.diagnostics.add(Diagnostic::warning(
                    "unused variable",
                    vec![Note::primary(
                        decl.span,
                        format!(
                            "`{}` is never used",
                            decl.name.expect("variables always have names")
                        ),
                    )],
                ))
            });
    }
}
