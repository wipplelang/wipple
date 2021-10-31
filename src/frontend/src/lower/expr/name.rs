use crate::{lower::*, typecheck::Ty};

#[derive(Debug)]
pub struct NameExpr {
    pub span: Span,
    pub value: LocalIntern<String>,
}

impl NameExpr {
    pub fn new(span: Span, value: LocalIntern<String>) -> Self {
        NameExpr { span, value }
    }
}

impl ExprKind for NameExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower_to_form(self, stack: &Stack, info: &mut Info) -> Form {
        match self.resolve(stack, info) {
            Some(form) => form,
            None => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    format!("'{}' is not defined", self.value),
                    vec![Note::primary(
                        self.span,
                        "This name does not resolve to a variable",
                    )],
                ));

                Form::item(self.span, Item::error(self.span))
            }
        }
    }

    fn lower_to_binding(self, _: &Stack, _: &mut Info) -> Option<SpannedBinding> {
        Some(SpannedBinding::from(NameBinding::new(
            self.span, self.value,
        )))
    }

    fn lower_to_ty(self, stack: &Stack, info: &mut Info) -> Option<Ty> {
        let form = self.resolve(stack, info)?;

        match form.kind {
            FormKind::Ty { ty } => Some(ty),
            _ => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Expected type",
                    vec![Note::primary(
                        self.span,
                        "Expected type here",
                    )],
                ));

                None
            }
        }
    }
}

impl NameExpr {
    pub(super) fn resolve(&self, mut stack: &Stack, info: &mut Info) -> Option<Form> {
        let mut used = vec![info.used_variables.clone()];

        loop {
            macro_rules! parent {
                () => {
                    if let Some(parent) = stack.parent {
                        stack = parent;
                    } else {
                        break None;
                    }
                };
            }

            if let Some(variable) = stack.variables.borrow().get(&self.value) {
                let form = (variable.form)(self.span, info);

                if matches!(form.kind, FormKind::Item { .. }) {
                    for list in used {
                        list.borrow_mut().insert(variable.id);
                    }
                }

                break Some(form);
            } else {
                if let Some(captures) = &stack.captures {
                    used.push(captures.clone());
                }

                parent!();
            }
        }
    }
}
