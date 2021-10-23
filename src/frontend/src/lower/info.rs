use crate::lower::*;
use serde::Serialize;
use std::{cell::RefCell, collections::HashSet, rc::Rc, sync::Arc};
use wipple_diagnostics::*;

pub struct Info<'a> {
    pub diagnostics: &'a mut Diagnostics,
    pub declared_variables: Vec<Variable>,
    pub used_variables: Rc<RefCell<HashSet<VariableId>>>,
}

impl<'a> Info<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Info {
            diagnostics,
            declared_variables: Default::default(),
            used_variables: Default::default(),
        }
    }
}

#[derive(Clone, Serialize)]
pub struct Variable {
    pub id: VariableId,
    pub declaration_span: Span,
    pub name: LocalIntern<String>,
    #[serde(skip)]
    pub form: Arc<dyn Fn(Span, &mut Info) -> Form>,
}

impl Variable {
    pub fn compiletime(
        declaration_span: Span,
        name: LocalIntern<String>,
        form: impl Fn(Span) -> Form + 'static,
    ) -> Self {
        Variable {
            id: VariableId::new(),
            declaration_span,
            name,
            form: Arc::new(move |span, _| form(span)),
        }
    }

    pub fn runtime(declaration_span: Span, name: LocalIntern<String>) -> Self {
        let id = VariableId::new();

        Variable {
            id,
            declaration_span,
            name,
            form: Arc::new(move |span, _| {
                let mut item = Item::variable(span, id);
                item.declared_name = Some(name);
                Form::Item(item)
            }),
        }
    }
}
