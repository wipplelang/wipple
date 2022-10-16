use crate::{analysis, diagnostics::*, Compiler};

impl Compiler<'_> {
    pub fn lint_unused(&self, program: &analysis::Program) {
        program
            .declarations
            .variables
            .values()
            .filter(|decl| decl.uses.is_empty())
            .for_each(|decl| {
                if let Some(name) = decl.name {
                    self.diagnostics.add(Diagnostic::warning(
                        "unused variable",
                        vec![Note::primary(
                            decl.span,
                            format!("`{}` is never used", name),
                        )],
                    ));
                }
            });
    }
}
