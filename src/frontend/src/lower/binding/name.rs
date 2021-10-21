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

    fn assign(self, span: Span, form: SpannedForm, stack: &Stack, info: &mut Info) -> SpannedItem {
        let variable = Variable::runtime(self.span, self.name);
        let variable_id = variable.id;

        stack
            .variables
            .borrow_mut()
            .insert(self.name, variable.clone());

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
