use crate::{
    analysis::{
        self,
        typecheck::{engine, Typechecker},
        SpanList,
    },
    diagnostics::Note,
    VariableId,
};
use im::ordmap::Entry;
use std::{mem, ops::ControlFlow};

impl Typechecker {
    pub fn collect_usage(&self, expr: &analysis::Expression) {
        let reuse_info = |var: VariableId| {
            let decls = self.declarations.borrow();

            let decl = match decls.variables.get(&var) {
                Some(decl) => decl,
                None => return None,
            };

            let var_name = decl.name.as_deref().unwrap_or("<unknown>").to_string();

            let id = match decl.ty {
                engine::Type::Named(id, _, _) => id,
                _ => return None,
            };

            let ty = decls.types.get(&id).unwrap();

            Some((var_name, ty.attributes.on_reuse?))
        };

        type TrackUsed = im::OrdMap<VariableId, Vec<SpanList>>;

        let check_used = |var: VariableId, spans: Vec<SpanList>, used: &mut TrackUsed| {
            let (var_name, on_reuse_message) = match reuse_info(var) {
                Some((var_name, on_reuse_message)) => (var_name, on_reuse_message),
                None => return,
            };

            match used.entry(var) {
                Entry::Vacant(entry) => {
                    entry.insert(spans);
                }
                Entry::Occupied(entry) => {
                    self.compiler.add_error(
                        on_reuse_message,
                        spans
                            .iter()
                            .copied()
                            .map(|span| {
                                Note::primary(
                                    span,
                                    format!("cannot use `{var_name}` more than once"),
                                )
                            })
                            .chain(entry.get().iter().copied().map(|span| {
                                Note::secondary(span, format!("`{var_name}` first used here"))
                            }))
                            .collect(),
                        "reused-variable",
                    );
                }
            }
        };

        fn enter(
            expr: &analysis::Expression,
            typechecker: &Typechecker,
            branch: &mut Option<TrackUsed>,
            check_used: &impl Fn(VariableId, Vec<SpanList>, &mut TrackUsed),
        ) -> ControlFlow<()> {
            match &expr.kind {
                analysis::ExpressionKind::Variable(var) => {
                    if let Some(branch) = branch {
                        check_used(*var, vec![expr.span], branch);
                    } else {
                        check_used(*var, vec![expr.span], &mut typechecker.used.borrow_mut());
                    }
                }
                analysis::ExpressionKind::When(input, arms) => {
                    enter(input, typechecker, branch, check_used);

                    let mut all_branches = TrackUsed::new();
                    for arm in arms {
                        let prev_branch = mem::replace(branch, Some(TrackUsed::new()));

                        if let Some(guard) = &arm.guard {
                            guard.traverse(
                                |expr| enter(expr, typechecker, branch, check_used),
                                |_| ControlFlow::Continue(()),
                            );
                        }

                        arm.body.traverse(
                            |expr| enter(expr, typechecker, branch, check_used),
                            |_| ControlFlow::Continue(()),
                        );

                        let new_branch = mem::replace(branch, prev_branch).unwrap();

                        for (var, spans) in new_branch {
                            all_branches.entry(var).or_default().extend(spans);
                        }
                    }

                    for (var, spans) in all_branches {
                        check_used(var, spans, &mut typechecker.used.borrow_mut());
                    }

                    // Don't iterate over the `when` expression's children;
                    // we already do that above
                    return ControlFlow::Break(());
                }
                _ => {}
            }

            ControlFlow::Continue(())
        }

        let mut branch = None;
        expr.traverse(
            |expr| enter(expr, self, &mut branch, &check_used),
            |_| ControlFlow::Continue(()),
        );
    }

    pub fn report_unused(&self) {
        // TODO

        // let decls = self.declarations.borrow();
        // let used = self.used.take();

        // for (&var, decl) in &decls.variables {
        //     if !used.contains_key(&var) {
        //         let id = match decl.ty {
        //             engine::Type::Named(id, _, _) => id,
        //             _ => return,
        //         };

        //         let ty = decls.types.get(&id).unwrap();

        //         let message = match ty.attributes.on_reuse {
        //             Some(message) => message,
        //             None => return,
        //         };

        //         self.compiler.add_error(
        //             format!(
        //                 "`{}` was never used",
        //                 decl.name.as_deref().unwrap_or("<unknown>")
        //             ),
        //             vec![Note::primary(
        //                 decl.span,
        //                 "try passing this variable to a function",
        //             )],
        //             "never-used",
        //         );
        //     }
        // }
    }
}
