use crate::{analysis, diagnostics::*, Compiler};
use std::ops::ControlFlow;

impl Compiler {
    pub(super) fn useless_expression_lint(&self, program: &analysis::Program) {
        for item in program.generic_items.values() {
            self.check_useless_expression(&item.expr, program);
        }

        for item in program.items.values() {
            let item = item.read();
            let (_, expr) = &*item;

            self.check_useless_expression(expr, program);
        }
    }

    fn check_useless_expression(&self, expr: &analysis::Expression, program: &analysis::Program) {
        expr.traverse(
            |expr| {
                if let analysis::ExpressionKind::Block(exprs, entrypoint) = &expr.kind {
                    let mut exprs = exprs.iter().peekable();

                    while let Some(expr) = exprs.next() {
                        let statement = *entrypoint || exprs.peek().is_some();

                        if statement && expr.is_pure(program) {
                            self.add_diagnostic(
                                self.warning(
                                    expr.span,
                                    "result of this code is never used",
                                    "useless-expression",
                                )
                                .fix_with(
                                    "`show` the result",
                                    FixRange::before(expr.span.first()),
                                    "show ",
                                ),
                            );
                        }
                    }
                }

                ControlFlow::Continue(())
            },
            |_| ControlFlow::Continue(()),
        );
    }
}
