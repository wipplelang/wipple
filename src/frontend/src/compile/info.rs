use crate::{compile::*, project::Project, typecheck::Type};
use serde::Serialize;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    sync::Arc,
};
use wipple_diagnostics::*;

pub struct Info<'a> {
    pub diagnostics: &'a mut Diagnostics,
    pub project: &'a Project,
    pub files: Vec<Arc<File>>,
    pub declared_variables: Vec<Variable>,
    pub used_variables: Arc<RefCell<HashSet<VariableId>>>,
    pub externals: HashMap<(String, String), Vec<Type>>,
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
    pub name: InternedString,
    #[serde(skip)]
    pub form: Arc<dyn Fn(Span, LowerContext, &mut Info) -> Option<Form>>,
}

impl Variable {
    pub fn compile_time(
        declaration_span: Span,
        name: InternedString,
        form: impl Fn(Span, LowerContext, &mut Info) -> Option<Form> + 'static,
    ) -> Self {
        Variable {
            id: VariableId::new(),
            declaration_span,
            name,
            form: Arc::new(form),
        }
    }

    pub fn runtime(declaration_span: Span, name: InternedString) -> Self {
        let id = VariableId::new();

        Variable {
            id,
            declaration_span,
            name,
            form: Arc::new(move |span, _, _| {
                let mut item = Item::variable(span, id);
                item.debug_info.declared_name = Some(name);
                Some(Form::item(span, item))
            }),
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
