use crate::{compile::*, *};

#[derive(Debug)]
pub struct NameExpr {
    pub span: Span,
    pub value: InternedString,
}

impl NameExpr {
    pub fn new(span: Span, value: InternedString) -> Self {
        NameExpr { span, value }
    }
}

impl ExprKind for NameExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower(self, context: LowerContext, stack: &Stack, info: &mut Info) -> Option<Form> {
        match context {
            LowerContext::Binding => Some(
                self.resolve(context, stack, info)?
                    .and_then(|form| match form.kind {
                        FormKind::Binding { .. } => Some(form),
                        _ => None,
                    })
                    .unwrap_or_else(|| {
                        Form::binding(
                            self.span,
                            Binding::from(NameBinding::new(self.span, self.value)),
                        )
                    }),
            ),
            _ => {
                let form = self.resolve(context, stack, info)?;

                if form.is_none() {
                    info.diagnostics.add(Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!("'{}' is not defined", self.value),
                        vec![Note::primary(
                            self.span,
                            "This name does not resolve to a variable",
                        )],
                    ));
                }

                form
            }
        }
    }
}

impl NameExpr {
    pub(super) fn resolve(
        &self,
        context: LowerContext,
        mut stack: &Stack,
        info: &mut Info,
    ) -> Option<Option<Form>> {
        let mut used = vec![info.used_variables.clone()];

        loop {
            macro_rules! parent {
                () => {
                    if let Some(parent) = stack.parent {
                        stack = parent;
                    } else {
                        break Some(None);
                    }
                };
            }

            if let Some(variable) = stack.variables.borrow().get(&self.value) {
                let form = variable.form(self.span, context, info)?;

                if matches!(form.kind, FormKind::Item { .. }) {
                    for list in used {
                        list.borrow_mut().insert(variable.id);
                    }
                }

                break Some(Some(form));
            } else {
                if let Some(captures) = &stack.captures {
                    used.push(captures.clone());
                }

                parent!();
            }
        }
    }
}
