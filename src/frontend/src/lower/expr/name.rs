use crate::lower::*;

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

                Form::Item(Item::error(self.span))
            }
        }
    }

    fn lower_to_binding(self, _: &Stack, _: &mut Info) -> Option<SpannedBinding> {
        Some(SpannedBinding::from(NameBinding::new(
            self.span, self.value,
        )))
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

                if matches!(form, Form::Item(_)) {
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
