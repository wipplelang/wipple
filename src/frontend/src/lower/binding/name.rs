use crate::lower::*;

pub struct NameBinding {
    pub span: Span,
    pub name: LocalIntern<String>,
}

impl NameBinding {
    pub fn new(span: Span, name: LocalIntern<String>) -> Self {
        NameBinding { span, name }
    }
}

impl Binding for NameBinding {
    fn span(&self) -> Span {
        self.span
    }

    fn assign(self, span: Span, form: SpannedForm, stack: Stack, info: &mut Info) -> SpannedItem {
        match &stack.scope {
            Scope::Function { .. } => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Cannot assign to variable here",
                    vec![Note::primary(span, "Try moving this into a block")],
                ));

                SpannedItem::error(span)
            }
            Scope::Block { variables } => {
                let variable = Variable::runtime(self.span, self.name);
                let variable_id = variable.id;

                variables.borrow_mut().insert(self.name, variable.clone());
                info.declared_variables.push(variable);

                if let Form::Item(item) = form.form {
                    SpannedItem::new(
                        span,
                        Item::Initialize(InitializeItem::new(
                            variable_id,
                            SpannedItem::with_info(form.info, item),
                        )),
                    )
                } else {
                    SpannedItem::unit(span)
                }
            }
        }
    }
}
