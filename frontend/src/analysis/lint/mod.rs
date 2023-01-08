mod unused_variable;
mod useless_bounds;

use crate::{analysis, Compiler};

impl Compiler<'_> {
    pub fn lint(&self, program: &analysis::Program) {
        self.unused_variable_lint(program);
        self.useless_bounds_lint(program);
    }
}
