use crate::{optimize::Program, Compiler};

impl<L> Compiler<L> {
    pub(super) fn optimize_tail_calls(&mut self, _program: &mut Program) {
        // TODO
    }
}
