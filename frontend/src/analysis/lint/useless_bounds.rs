use crate::{analysis, diagnostics::*, Compiler};

impl Compiler {
    pub(super) fn useless_bounds_lint(&self, program: &analysis::Program) {
        for constant in program.declarations.constants.values() {
            self.check_useless_bounds(&constant.bounds);
        }

        for instance in program
            .declarations
            .instances
            .values()
            .flat_map(|decls| decls.values())
        {
            self.check_useless_bounds(&instance.bounds);
        }
    }

    fn check_useless_bounds(&self, bounds: &[analysis::Bound]) {
        for bound in bounds {
            if bound.params.iter().all(|ty| ty.params().is_empty()) {
                self.add_diagnostic(
                    self.warning(
                        bound.span,
                        "this bound doesn't refer to any type parameters",
                        "useless-bound",
                    )
                    .fix_with(
                        "remove the bound",
                        FixRange::replace(bound.span.first()),
                        String::new(),
                    ),
                );
            }
        }
    }
}
