use crate::{compile::*, *};
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct NameBinding {
    pub span: Span,
    pub name: InternedString,
}

impl NameBinding {
    pub fn new(span: Span, name: InternedString) -> Self {
        NameBinding { span, name }
    }
}

impl BindingKind for NameBinding {
    fn span(&self) -> Span {
        self.span
    }

    fn assign(self, span: Span, form: Form, stack: &Stack, info: &mut Info) -> Item {
        let mut decl_item = form.as_decl_item();
        if let Some(decl_item) = &mut decl_item {
            decl_item.info.declared_name = Some(self.name);
        }

        let (variable, runtime_item) = match form.kind {
            FormKind::Item(item) => (Variable::runtime(self.name), Some(item)),
            _ => (
                Variable::compile_time(move |_, _, _| {
                    let mut form = form.clone();
                    form.info.declared_name = Some(self.name);
                    Some(form)
                }),
                None,
            ),
        };

        let variable_id = variable.id;

        stack.with_variables(|variables| {
            variables.insert(self.name, variable.clone());
        });

        let mut binding_info = ItemInfo::new(self.span);
        binding_info.declared_name = Some(self.name);

        info.declared_variables.insert(variable.id, binding_info);

        if let Some(runtime_item) = runtime_item {
            Item::initialize(span, binding_info, variable_id, runtime_item)
        } else {
            decl_item.unwrap_or_else(|| Item::unit(span))
        }
    }
}
