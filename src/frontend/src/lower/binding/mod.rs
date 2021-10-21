use crate::lower::*;
use wipple_parser::Intern;

pub struct SpannedBinding {
    pub span: Span,
    pub binding: Binding,
}

pub enum Binding {
    Name(Intern<String>),
    Error,
    // eventually, destructuring and conversions
}

impl SpannedBinding {
    pub fn new(span: Span, binding: Binding) -> Self {
        SpannedBinding { span, binding }
    }

    pub fn name(span: Span, name: Intern<String>) -> Self {
        SpannedBinding::new(span, Binding::Name(name))
    }

    pub fn error(span: Span) -> Self {
        SpannedBinding::new(span, Binding::Error)
    }
}

impl SpannedBinding {
    pub fn assign(
        self,
        assignment_span: Span,
        form: SpannedForm,
        stack: Stack,
        diagnostics: &mut Diagnostics,
    ) -> SpannedItem {
        match self.binding {
            Binding::Name(name) => {
                let form_span = form.span;
                let variable = Variable::runtime(self.span, name);
                let variable_id = variable.id;

                match &stack.scope {
                    Scope::Function { .. } => {
                        diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Cannot assign to variable here",
                            vec![Note::primary(
                                assignment_span,
                                "Try moving this into a block",
                            )],
                        ));

                        return SpannedItem::error(assignment_span);
                    }
                    Scope::Block { variables } => {
                        variables.borrow_mut().insert(name, variable);
                    }
                }

                if let Form::Item(item) = form.form {
                    SpannedItem::new(
                        assignment_span,
                        Item::Initialize(InitializeItem::new(
                            variable_id,
                            SpannedItem::new(form_span, item),
                        )),
                    )
                } else {
                    SpannedItem::unit(assignment_span)
                }
            }
            Binding::Error => SpannedItem::error(form.span),
        }
    }
}
