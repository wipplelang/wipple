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

impl Typechecker {
    pub fn collect_usage(&self, expr: &analysis::Expression) {
        let check_used = |var: VariableId, spans: Vec<SpanList>| {
            let decls = self.declarations.borrow();

            let decl = match decls.variables.get(&var) {
                Some(decl) => decl,
                None => return,
            };

            let id = match decl.ty {
                engine::Type::Named(id, _, _) => id,
                _ => return,
            };

            let ty = decls.types.get(&id).unwrap();

            let message = match ty.attributes.on_reuse {
                Some(message) => message,
                None => return,
            };

            match self.used.borrow_mut().entry(var) {
                Entry::Vacant(entry) => {
                    entry.insert(spans);
                }
                Entry::Occupied(entry) => {
                    let var_name = decl.name.as_deref().unwrap_or("<unknown>");

                    self.compiler.add_error(
                        message,
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

        expr.traverse_with(
            Vec::<im::OrdMap<_, Vec<_>>>::new(),
            |expr, branch| match &expr.kind {
                analysis::ExpressionKind::Variable(var) => {
                    if let Some(branch) = branch.last_mut() {
                        branch.entry(*var).or_default().push(expr.span);
                    } else {
                        check_used(*var, vec![expr.span]);
                    }
                }
                analysis::ExpressionKind::When(_, _) => {
                    branch.push(im::OrdMap::new());
                }
                analysis::ExpressionKind::Constant(_) => {
                    // TODO: Handle constants
                }
                _ => {}
            },
            |expr, branch| {
                if let analysis::ExpressionKind::When(_, _) = &expr.kind {
                    let vars = branch.pop().unwrap();
                    for (var, spans) in vars {
                        check_used(var, spans);
                    }
                }
            },
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
