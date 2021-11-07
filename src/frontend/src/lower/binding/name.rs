use crate::lower::*;

#[derive(Debug, Clone, Serialize)]
pub struct NameBinding {
    pub span: Span,
    pub name: LocalIntern<String>,
}

impl NameBinding {
    pub fn new(span: Span, name: LocalIntern<String>) -> Self {
        NameBinding { span, name }
    }
}

impl BindingKind for NameBinding {
    fn span(&self) -> Span {
        self.span
    }

    fn assign(self, span: Span, form: Form, stack: &Stack, info: &mut Info) -> Item {
        let (variable, runtime_item) = match form.kind {
            FormKind::Item { item } => (Variable::runtime(self.span, self.name), Some(item)),
            _ => (
                Variable::compile_time(self.span, self.name, move |_| form.clone()),
                None,
            ),
        };

        let variable_id = variable.id;

        stack
            .variables
            .borrow_mut()
            .insert(self.name, variable.clone());

        info.declared_variables.push(variable);

        if let Some(runtime_item) = runtime_item {
            Item::initialize(span, variable_id, Box::new(runtime_item))
        } else {
            Item::unit(span)
        }
    }
}
