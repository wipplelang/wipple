use crate::{analysis, diagnostics::*, Compiler};

impl Compiler<'_> {
    pub(super) fn unused_variable_lint(&self, program: &analysis::Program) {
        for variable in program.declarations.variables.values() {
            if let Some(name) = variable.name {
                if variable.uses.is_empty() {
                    self.add_warning(
                        "unused variable",
                        vec![Note::primary(
                            variable.span,
                            format!("`{}` is never used", name),
                        )],
                    );
                }
            }
        }
    }
}
