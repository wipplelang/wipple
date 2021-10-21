use crate::lower::*;
use serde::Serialize;
use std::{cell::RefCell, collections::HashMap, sync::Arc};
use wipple_parser::Intern;

#[derive(Clone, Copy)]
pub struct Stack<'a: 'p, 'p> {
    pub parent: Option<&'a Stack<'p, 'p>>,
    pub scope: Scope<'a>,
}

#[derive(Clone, Copy)]
pub enum Scope<'a> {
    Function {
        parameter_name: Intern<String>,
        parameter: &'a Variable,
        captures: &'a RefCell<Vec<VariableId>>,
    },
    Block {
        variables: &'a RefCell<HashMap<Intern<String>, Variable>>,
    },
}

impl<'a, 'p> Stack<'a, 'p> {
    pub fn root(scope: Scope<'a>) -> Self {
        Stack {
            parent: None,
            scope,
        }
    }

    pub fn child(&'a self, scope: Scope<'p>) -> Self {
        Stack {
            parent: Some(self),
            scope,
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
    pub name: Intern<String>,
    #[serde(skip)]
    pub form: Arc<dyn Fn(Span) -> SpannedForm>,
}

impl Variable {
    pub fn new(
        declaration_span: Span,
        name: Intern<String>,
        form: impl Fn(Span) -> SpannedForm + 'static,
    ) -> Self {
        Variable {
            id: VariableId::new(),
            declaration_span,
            name,
            form: Arc::new(form),
        }
    }

    pub fn runtime(declaration_span: Span, name: Intern<String>) -> Self {
        let id = VariableId::new();

        Variable {
            id,
            declaration_span,
            name,
            form: Arc::new(move |span| SpannedItem::variable(span, id).into()),
        }
    }
}
