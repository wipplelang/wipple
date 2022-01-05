#![allow(clippy::type_complexity)]

use crate::{compile::*, project::Project, typecheck::Type, *};
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
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
    pub fn with_prelude(diagnostics: &'a mut Diagnostics, project: &'a Project) -> Self {
        let (prelude, mut info) = prelude(diagnostics, project);
        info.files.push(prelude);
        info
    }

    pub(crate) fn with_files(
        diagnostics: &'a mut Diagnostics,
        project: &'a Project,
        files: Vec<Arc<File>>,
    ) -> Self {
        Info {
            diagnostics,
            project,
            files,
            declared_variables: Default::default(),
            used_variables: Default::default(),
            externals: Default::default(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Variable {
    pub id: VariableId,
    #[serde(skip)]
    pub form: Option<Arc<dyn Fn(Span, LowerContext, &mut Info) -> Option<Form>>>,
}

impl Variable {
    pub fn compile_time(
        form: impl Fn(Span, LowerContext, &mut Info) -> Option<Form> + 'static,
    ) -> Self {
        Variable {
            id: VariableId::new(),
            form: Some(Arc::new(form)),
        }
    }

    pub fn runtime(declared_name: InternedString) -> Self {
        let id = VariableId::new();

        Variable {
            id,
            form: Some(Arc::new(move |span, _, _| {
                let mut item = Item::variable(span, id);
                item.info.declared_name = Some(declared_name);
                Some(Form::item(span, item))
            })),
        }
    }

    pub fn form(&self, span: Span, context: LowerContext, info: &mut Info) -> Option<Form> {
        let form = self
            .form
            .as_ref()
            .expect("Cannot use 'form' after compilation");

        form(span, context, info)
    }
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Variable").field("id", &self.id).finish()
    }
}
