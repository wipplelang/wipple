mod unused;

use crate::{compile, Compiler};

impl<L> Compiler<L> {
    pub fn lint(&mut self, program: &compile::Program) {
        self.lint_unused(program);
    }
}
