use crate::{optimize::Program, Compiler, Loader};

impl<L: Loader> Compiler<L> {
    pub(super) fn optimize_tail_calls(&mut self, _program: &mut Program) {
        // TODO
    }
}
