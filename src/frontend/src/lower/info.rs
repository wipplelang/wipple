use crate::{lower::*, project::Project, typecheck::Ty};
use serde::Serialize;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
    sync::Arc,
};
use wipple_diagnostics::*;

pub struct Info<'a> {
    pub diagnostics: &'a mut Diagnostics,
    pub project: &'a Project,
    pub files: Vec<Rc<File>>,
    pub declared_variables: Vec<Variable>,
    pub used_variables: Rc<RefCell<HashSet<VariableId>>>,
    pub externals: HashMap<(String, String), Vec<Ty>>,
}

impl<'a> Info<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics, project: &'a Project) -> Self {
        Info {
            diagnostics,
            project,
            files: Default::default(),
            declared_variables: Default::default(),
            used_variables: Default::default(),
            externals: Default::default(),
        }
    }
}

#[derive(Clone, Serialize)]
pub struct Variable {
    pub id: VariableId,
    pub declaration_span: Span,
    pub name: LocalIntern<String>,
    #[serde(skip)]
    pub form: Option<Arc<dyn Fn(Span, &mut Info) -> Form>>,
}

impl Variable {
    pub fn compile_time(
        declaration_span: Span,
        name: LocalIntern<String>,
        form: impl Fn(Span) -> Form + 'static,
    ) -> Self {
        Variable {
            id: VariableId::new(),
            declaration_span,
            name,
            form: Some(Arc::new(move |span, _| form(span))),
        }
    }

    pub fn runtime(declaration_span: Span, name: LocalIntern<String>) -> Self {
        Variable {
            id: VariableId::new(),
            declaration_span,
            name,
            form: None,
        }
    }
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Variable")
            .field("id", &self.id)
            .field("declaration_span", &self.declaration_span)
            .field("name", &self.name)
            .finish()
    }
}
