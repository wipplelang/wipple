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

    fn assign(self, span: Span, form: Form, stack: &Stack, info: &mut Info) -> Item {
        let variable = Variable::runtime(self.span, self.name);
        let variable_id = variable.id;

        stack
            .variables
            .borrow_mut()
            .insert(self.name, variable.clone());

        info.declared_variables.push(variable);

        if let Form::Item(item) = form {
            Item::initialize(span, variable_id, item)
        } else {
            Item::unit(span)
        }
    }
}
