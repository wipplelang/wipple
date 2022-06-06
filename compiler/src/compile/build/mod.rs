#![allow(clippy::type_complexity)]

use crate::{
    compile::{
        self,
        expand::{self, ScopeValues},
        lower, Program,
    },
    diagnostics::*,
    parse::Span,
    Compiler, FilePath, Loader,
};
use indexmap::IndexMap;
use std::{cell::RefCell, rc::Rc, sync::Arc};

pub enum Progress {
    Resolving,
    Lowering {
        path: FilePath,
        current: usize,
        total: usize,
    },
    Typechecking(compile::typecheck::Progress),
}

impl<L: Loader> Compiler<L> {
    pub fn build(&mut self, path: FilePath) -> Option<Program> {
        self.build_with_progress(path, |_| {})
    }

    pub fn build_with_progress(
        &mut self,
        path: FilePath,
        mut progress: impl FnMut(Progress),
    ) -> Option<Program> {
        fn load<L: Loader>(
            compiler: &mut Compiler<L>,
            path: FilePath,
            source_span: Option<Span>,
            cache: &RefCell<IndexMap<FilePath, (Rc<lower::File>, Rc<ScopeValues>)>>,
            info: &mut expand::Info<L>,
        ) -> Option<(Rc<lower::File>, Rc<ScopeValues>)> {
            if let Some(cached) = cache.borrow().get(&path) {
                return Some(cached.clone());
            }

            let code = match compiler.loader.load(path) {
                Ok(code) => code,
                Err(error) => {
                    compiler.diagnostics.add(Diagnostic::error(
                        format!("cannot load file `{}`: {}", path, error),
                        source_span
                            .map(|span| Note::primary(span, "try fixing this import"))
                            .into_iter()
                            .collect(),
                    ));

                    return None;
                }
            };

            compiler
                .diagnostics
                .add_file(path, Arc::from(code.as_ref()));

            let file = compiler.parse_v2(path, &code)?;

            let dependencies: RefCell<Vec<Rc<lower::File>>> = Default::default();

            let (file, scope) = compiler.expand(file, info, |compiler, path, info| {
                load(compiler, path, source_span, cache, info).map(|(file, scope)| {
                    dependencies.borrow_mut().push(file);
                    scope
                })
            });

            let file = compiler.build_ast(file);
            let file = compiler.lower(file, dependencies.into_inner());

            Some(
                cache
                    .borrow_mut()
                    .entry(path)
                    .or_insert((Rc::new(file), Rc::new(scope)))
                    .clone(),
            )
        }

        progress(Progress::Resolving);

        let cache = Default::default();
        load(self, path, None, &cache, &mut expand::Info::default())?;

        let lowered_files = cache
            .into_inner()
            .into_values()
            .map(|(file, _)| Rc::try_unwrap(file).unwrap())
            .collect::<Vec<_>>();

        if lowered_files.iter().all(|file| file.complete) {
            self.typecheck_with_progress(lowered_files, |p| progress(Progress::Typechecking(p)))
        } else {
            None
        }
    }
}
