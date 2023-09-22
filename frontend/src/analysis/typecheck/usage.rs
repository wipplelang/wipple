use crate::{
    analysis::{
        self,
        typecheck::{BuiltinType, Type, Typechecker},
        SpanList, TypeStructure,
    },
    diagnostics::Note,
    VariableId,
};
use im::ordmap::Entry;
use std::{mem, ops::ControlFlow};

impl Typechecker {
    pub fn check_usage(&self, expr: &analysis::Expression) {
        let reuse_info = |var: VariableId| {
            let decls = self.declarations.borrow();

            let decl = match decls.variables.get(&var) {
                Some(decl) => decl,
                None => return None,
            };

            let var_name = decl.name.as_deref().unwrap_or("<unknown>").to_string();

            let message = self.on_reuse_message(&decl.ty)?.to_string();

            Some((var_name, message))
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
                analysis::ExpressionKind::Semantics(semantics, _) => {
                    if *semantics == analysis::Semantics::Nonconsuming {
                        return ControlFlow::Break(());
                    }
                }
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

    pub(crate) fn on_reuse_message(&self, ty: &Type) -> Option<&'static str> {
        match ty {
            Type::Named(id, _, structure) => {
                if let Some(message) = self
                    .with_type_decl(*id, |decl| decl.attributes.on_reuse)
                    .unwrap()
                {
                    return Some(message.as_str());
                }

                match structure {
                    TypeStructure::Marker | TypeStructure::Recursive(_) => None,
                    TypeStructure::Structure(tys) => {
                        tys.iter().find_map(|ty| self.on_reuse_message(ty))
                    }
                    TypeStructure::Enumeration(variants) => variants
                        .iter()
                        .flatten()
                        .find_map(|ty| self.on_reuse_message(ty)),
                }
            }
            Type::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => self.on_reuse_message(ty),
                BuiltinType::Number
                | BuiltinType::Integer
                | BuiltinType::Natural
                | BuiltinType::Byte
                | BuiltinType::Signed
                | BuiltinType::Unsigned
                | BuiltinType::Float
                | BuiltinType::Double
                | BuiltinType::Text
                | BuiltinType::Ui
                | BuiltinType::TaskGroup => None,
            },
            _ => None,
        }
    }
}
