use crate::{
    analysis::{
        self,
        typecheck::{engine, Typechecker},
        SpanList,
    },
    diagnostics::Note,
    VariableId,
};
use std::{
    cell::RefCell,
    collections::{btree_map::Entry, BTreeMap},
};

impl Typechecker {
    pub fn check_usage(&self, expr: &analysis::Expression) {
        let used: RefCell<BTreeMap<VariableId, Vec<SpanList>>> = Default::default();

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

            if !ty.attributes.is_linear {
                return;
            }

            match used.borrow_mut().entry(var) {
                Entry::Vacant(entry) => {
                    entry.insert(spans);
                }
                Entry::Occupied(entry) => {
                    self.compiler.add_error(
                        "cannot use this value more than once",
                        spans
                            .iter()
                            .copied()
                            .map(|span| Note::primary(span, "this value has already been used"))
                            .chain(
                                entry
                                    .get()
                                    .iter()
                                    .copied()
                                    .map(|span| Note::secondary(span, "value used here")),
                            )
                            .collect(),
                        "used-multiple-times",
                    );
                }
            }
        };

        expr.traverse_with(
            Vec::<BTreeMap<_, Vec<_>>>::new(),
            |expr, branch| match &expr.kind {
                analysis::ExpressionKind::Variable(var) => {
                    if let Some(branch) = branch.last_mut() {
                        branch.entry(*var).or_default().push(expr.span);
                    } else {
                        check_used(*var, vec![expr.span]);
                    }
                }
                analysis::ExpressionKind::When(_, _) => {
                    branch.push(BTreeMap::new());
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

        // for expr in unused.values() {
        //     self.compiler.add_error(
        //         "value was never used",
        //         vec![Note::primary(
        //             expr.span,
        //             "try passing this value to a function",
        //         )],
        //         "never-used",
        //     );
        // }
    }
}
