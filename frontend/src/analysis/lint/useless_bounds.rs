use crate::{analysis, diagnostics::*, Compiler};

impl Compiler<'_> {
    pub(super) fn useless_bounds_lint(&self, program: &analysis::Program) {
        for constant in program.declarations.constants.values() {
            for bound in &constant.bounds {
                if bound.params.iter().all(|ty| ty.params().is_empty()) {
                    self.add_warning(
                        "this bound doesn't refer to any type parameters",
                        vec![Note::primary(bound.span, "try removing this bound")],
                    );
                }
            }
        }
    }
}
