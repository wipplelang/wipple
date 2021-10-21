use crate::lower::*;
use serde::Serialize;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Clone)]
pub struct Stack<'a: 'p, 'p> {
    pub parent: Option<&'a Stack<'p, 'p>>,
    pub variables: RefCell<HashMap<LocalIntern<String>, Variable>>,
    pub captures: Option<RefCell<HashSet<VariableId>>>,
}

impl<'a, 'p> Stack<'a, 'p> {
    pub fn root() -> Self {
        Stack {
            parent: None,
            variables: RefCell::new(builtins()),
            captures: None,
        }
    }

    pub fn child_block(&'a self) -> Self {
        Stack {
            parent: Some(self),
            variables: Default::default(),
            captures: None,
        }
    }

    pub fn child_function(&'a self) -> Self {
        Stack {
            parent: Some(self),
            variables: Default::default(),
            captures: Some(Default::default()),
        }
    }
}

id! {
    pub struct VariableId;
}

#[derive(Clone, Serialize)]
pub struct Variable {
    pub id: VariableId,
    pub declaration_span: Span,
    pub name: LocalIntern<String>,
    #[serde(skip)]
    pub form: Arc<dyn Fn(Span) -> SpannedForm>,
}

impl Variable {
    pub fn new(
        declaration_span: Span,
        name: LocalIntern<String>,
        form: impl Fn(Span) -> SpannedForm + 'static,
    ) -> Self {
        Variable {
            id: VariableId::new(),
            declaration_span,
            name,
            form: Arc::new(form),
        }
    }

    pub fn runtime(declaration_span: Span, name: LocalIntern<String>) -> Self {
        let id = VariableId::new();

        Variable {
            id,
            declaration_span,
            name,
            form: Arc::new(move |span| {
                let mut item = SpannedItem::variable(span, id);
                item.info.declared_name = Some(name);
                item.into()
            }),
        }
    }
}
