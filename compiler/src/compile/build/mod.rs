#![allow(clippy::too_many_arguments)]

use crate::{
    compile::{self, Program},
    diagnostics::*,
    parse::Span,
    Compiler, FilePath, Loader,
};
use std::{cell::Cell, rc::Rc};

#[derive(Debug)]
pub enum Progress {
    Resolving {
        path: FilePath,
        count: usize,
    },
    Lowering {
        path: FilePath,
        current: usize,
        total: usize,
    },
    Typechecking(compile::typecheck::Progress),
}

impl<L: Loader> Compiler<'_, L> {
    pub fn build(&mut self, path: FilePath) -> Option<Program> {
        self.build_with_progress(path, |_| {})
    }

    pub fn build_with_progress(
        &mut self,
        path: FilePath,
        mut progress: impl FnMut(Progress),
    ) -> Option<Program> {
        let mut files = indexmap::IndexMap::new();

        fn load<L: Loader>(
            compiler: &mut Compiler<L>,
            path: FilePath,
            source_path: Option<FilePath>,
            source_span: Option<Span>,
            progress: &mut impl FnMut(Progress),
            count: Rc<Cell<usize>>,
            files: &mut indexmap::IndexMap<FilePath, Rc<compile::expand::File<L>>>,
        ) -> Option<Rc<compile::expand::File<L>>> {
            macro_rules! try_load {
                ($expr:expr) => {
                    match $expr {
                        Ok(x) => x,
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
                    }
                };
            }

            let resolved_path = try_load!(compiler.loader.resolve(path, source_path));

            if let Some(cached) = compiler.loader.cache().get(&resolved_path) {
                return Some(cached.clone());
            }

            count.set(count.get() + 1);
            progress(Progress::Resolving {
                path: resolved_path,
                count: count.get(),
            });

            let code = try_load!(compiler.loader.load(resolved_path));

            let file = compiler.parse(resolved_path, &code)?;
            let file = compiler.expand(file, |compiler, new_path| {
                load(
                    compiler,
                    new_path,
                    Some(resolved_path),
                    source_span,
                    progress,
                    count.clone(),
                    files,
                )
            })?;

            let file = Rc::new(file);

            // Don't cache virtual or builtin paths
            if matches!(resolved_path, FilePath::Path(_)) {
                compiler.loader.cache().insert(resolved_path, file.clone());
            }

            compiler
                .loader
                .source_map()
                .insert(resolved_path, code.clone());

            files.insert(resolved_path, file.clone());

            Some(file)
        }

        load(
            self,
            path,
            None,
            None,
            &mut progress,
            Default::default(),
            &mut files,
        )?;

        let mut cache = indexmap::IndexMap::new();

        fn lower<L: Loader>(
            compiler: &mut Compiler<L>,
            file: Rc<compile::expand::File<L>>,
            cache: &mut indexmap::IndexMap<FilePath, Rc<compile::lower::File>>,
        ) -> Rc<compile::lower::File> {
            let path = file.path;

            if let Some(file) = cache.get(&path) {
                return file.clone();
            }

            let dependencies = file
                .dependencies
                .clone()
                .into_iter()
                .map(|dependency| lower(compiler, dependency, cache))
                .collect::<Vec<_>>();

            let file = compiler.build_ast((*file).clone());

            let file = Rc::new(compiler.lower(file, dependencies));
            cache.insert(path, file.clone());

            file
        }

        #[allow(clippy::needless_collect)] // needed for diagnostics below
        let lowered_files = files
            .into_values()
            .map(|file| lower(self, file, &mut cache))
            .collect::<Vec<_>>();

        drop(cache);

        let lowered_files = lowered_files
            .into_iter()
            .map(|file| Rc::try_unwrap(file).unwrap())
            .collect::<Vec<_>>();

        self.typecheck_with_progress(lowered_files, |p| progress(Progress::Typechecking(p)))
    }
}
