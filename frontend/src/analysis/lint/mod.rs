mod unused_variable;

use crate::{analysis, Compiler};

impl Compiler<'_> {
    pub fn lint(&self, program: &analysis::Program) {
        self.unused_variable_lint(program);
    }
}
