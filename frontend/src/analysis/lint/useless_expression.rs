use crate::{analysis, diagnostics::*, Compiler};

impl Compiler {
    pub(super) fn useless_expression_lint(&self, program: &analysis::Program) {
        for decl in program.declarations.constants.values() {
            if let Some(expr) = &decl.body {
                self.check_useless_expression(expr, program);
            }
        }

        for item in program.items.values() {
            let item = item.lock();
            let (_, expr) = &*item;

            self.check_useless_expression(expr, program);
        }
    }

    fn check_useless_expression(&self, expr: &analysis::Expression, program: &analysis::Program) {
        expr.traverse(|expr| {
            if let analysis::ExpressionKind::Block(exprs, entrypoint) = &expr.kind {
                let mut exprs = exprs.iter().peekable();

                while let Some(expr) = exprs.next() {
                    let statement = *entrypoint || exprs.peek().is_some();

                    if statement && expr.is_pure(program) {
                        self.add_warning(
                            "this expression doesn't do anything",
                            vec![Note::primary(
                                expr.span,
                                "did you mean to use the result, eg. `show` it?",
                            )],
                        );
                    }
                }
            }
        });
    }
}
