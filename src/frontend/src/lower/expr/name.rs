use crate::lower::*;

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

    fn lower(self, context: LowerContext, stack: &Stack, info: &mut Info) -> Option<Form> {
        match context {
            LowerContext::Binding => Some(Form::binding(
                self.span,
                Binding::from(NameBinding::new(self.span, self.value)),
            )),
            _ => match self.resolve(stack, info) {
                Some(form) => Some(form),
                None => {
                    info.diagnostics.add(Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!("'{}' is not defined", self.value),
                        vec![Note::primary(
                            self.span,
                            "This name does not resolve to a variable",
                        )],
                    ));

                    None
                }
            },
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
                let form = if let Some(form) = &variable.form {
                    form(self.span, info)
                } else {
                    for list in used {
                        list.borrow_mut().insert(variable.id);
                    }

                    let mut item = Item::variable(self.span, variable.id);
                    item.debug_info.declared_name = Some(self.value);
                    Form::item(self.span, item)
                };

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
