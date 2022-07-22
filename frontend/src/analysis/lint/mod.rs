mod unused;

use crate::{analysis, Compiler, Loader};

impl<L: Loader> Compiler<L> {
    pub fn lint(&mut self, program: &analysis::Program) {
        self.lint_unused(program);
    }
}
