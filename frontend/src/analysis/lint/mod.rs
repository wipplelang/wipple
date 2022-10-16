mod unused;

use crate::{analysis, Compiler};

impl Compiler<'_> {
    pub fn lint(&self, program: &analysis::Program) {
        self.lint_unused(program);
    }
}
