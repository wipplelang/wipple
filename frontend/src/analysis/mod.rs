#![allow(clippy::too_many_arguments, clippy::type_complexity)]

pub mod ast;
pub mod expand;
pub mod lint;
pub mod lower;
pub mod optimize;
pub mod typecheck;

pub use typecheck::{
    Arm, Expression, ExpressionKind, Pattern, PatternKind, Program, RuntimeFunction, Type,
    TypeAnnotation, TypeAnnotationKind, TypeStructure,
};

use crate::{diagnostics::*, helpers::Shared, parse::Span, Compiler, FilePath};
use async_recursion::async_recursion;
use std::sync::{atomic::AtomicUsize, Arc};

#[derive(Clone)]
pub struct Options {
    progress: Option<Shared<Box<dyn Fn(Progress) + Send + Sync>>>,
    lint: bool,
}

impl Options {
    pub fn new() -> Self {
        Options {
            progress: None,
            lint: true,
        }
    }

    pub fn tracking_progress(
        mut self,
        progress: impl Fn(Progress) + Send + Sync + 'static,
    ) -> Self {
        self.progress = Some(Shared::new(Box::new(progress)));
        self
    }

    pub fn lint(mut self, lint: bool) -> Self {
        self.lint = lint;
        self
    }
}

impl Default for Options {
    fn default() -> Self {
        Self::new()
    }
}

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
    Typechecking(typecheck::Progress),
}

impl Compiler<'_> {
    pub async fn analyze_with(
        &self,
        entrypoint: FilePath,
        options: &Options,
    ) -> (Program, FinalizedDiagnostics) {
        let files = self.expand_with(entrypoint, options).await;
        let (entrypoint, lowering_is_complete) = self.lower_with(files, options);
        let program = self.typecheck_with(entrypoint, lowering_is_complete, options);
        self.lint_with(&program, options);

        let diagnostics = self.finish_analysis();
        (program, diagnostics)
    }

    pub async fn expand_with(
        &self,
        entrypoint: FilePath,
        options: &Options,
    ) -> indexmap::IndexMap<FilePath, Arc<expand::File>> {
        #[async_recursion]
        async fn load(
            compiler: &Compiler,
            path: FilePath,
            source_path: Option<FilePath>,
            source_span: Option<Span>,
            entrypoint: FilePath,
            count: Arc<AtomicUsize>,
            files: Shared<indexmap::IndexMap<FilePath, Arc<expand::File>>>,
            stack: Shared<Vec<FilePath>>,
            options: &Options,
        ) -> Option<Arc<expand::File>> {
            macro_rules! try_load {
                ($expr:expr) => {
                    match $expr {
                        Ok(x) => x,
                        Err(error) => {
                            compiler.add_error(
                                format!("cannot load file `{}`: {}", path, error),
                                source_span
                                    .map(|span| Note::primary(span, "try fixing this import"))
                                    .into_iter()
                                    .collect(),
                            );

                            return None;
                        }
                    }
                };
            }

            let resolved_path = try_load!(compiler.loader.resolve(path, source_path));

            if let Some(cached) = compiler.loader.cache().lock().get(&resolved_path) {
                fn insert(
                    file: &Arc<expand::File>,
                    files: &mut indexmap::IndexMap<FilePath, Arc<expand::File>>,
                ) {
                    for (dependency, _) in &file.dependencies {
                        insert(dependency, files);
                    }

                    files.insert(file.path, file.clone());
                }

                let mut files = files.lock();
                insert(cached, &mut files);

                return Some(cached.clone());
            }

            {
                let count = count.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;

                if let Some(progress) = &options.progress {
                    progress.lock()(Progress::Resolving {
                        path: resolved_path,
                        count,
                    });
                }
            }

            {
                let stack = stack.lock();
                if let Some(index) = stack.iter().position(|path| path == &resolved_path) {
                    let stack = stack[index..]
                        .iter()
                        .chain(std::iter::once(&resolved_path))
                        .map(|path| path.to_string())
                        .collect::<Vec<_>>()
                        .join(", which imports");

                    compiler.add_error(
                        "import cycle detected",
                        vec![Note::primary(
                            source_span.unwrap(),
                            format!("this imports {}", stack),
                        )],
                    );

                    return None;
                }
            }

            let code = try_load!(compiler.loader.load(resolved_path).await);

            compiler
                .loader
                .source_map()
                .lock()
                .insert(resolved_path, code.clone());

            let file = compiler.parse(resolved_path, &code);

            stack.lock().push(resolved_path);

            let file = compiler
                .expand(file, {
                    let count = count.clone();
                    let files = files.clone();
                    let stack = stack.clone();
                    let options = options.clone();

                    move |compiler, source_span, new_path| {
                        let count = count.clone();
                        let files = files.clone();
                        let stack = stack.clone();
                        let options = options.clone();

                        Box::pin(async move {
                            load(
                                compiler,
                                new_path,
                                Some(resolved_path),
                                Some(source_span),
                                entrypoint,
                                count,
                                files,
                                stack,
                                &options,
                            )
                            .await
                        })
                    }
                })
                .await;

            stack.lock().pop();

            let file = Arc::new(file);

            // Don't cache virtual or builtin paths, nor the entrypoint
            if resolved_path != entrypoint
                && !matches!(resolved_path, FilePath::Virtual(_) | FilePath::Builtin(_))
            {
                compiler
                    .loader
                    .cache()
                    .lock()
                    .insert(resolved_path, file.clone());
            }

            files.lock().insert(resolved_path, file.clone());

            Some(file)
        }

        let files = Shared::default();

        load(
            self,
            entrypoint,
            None,
            None,
            entrypoint,
            Default::default(),
            files.clone(),
            Default::default(),
            options,
        )
        .await;

        files.into_unique()
    }

    pub fn lower_with(
        &self,
        files: indexmap::IndexMap<FilePath, Arc<expand::File>>,
        _options: &Options,
    ) -> (lower::File, bool) {
        assert!(!files.is_empty(), "expected at least one file");

        fn lower(compiler: &Compiler, file: Arc<expand::File>) -> Arc<lower::File> {
            let path = file.path;

            if let Some(file) = compiler.cache.lock().get(&path) {
                return file.clone();
            }

            let dependencies = file
                .dependencies
                .clone()
                .into_iter()
                .map(|(file, imports)| (lower(compiler, file), imports))
                .collect::<Vec<_>>();

            let file = compiler.build_ast((*file).clone());

            let file = Arc::new(compiler.lower(file, dependencies));

            // Only cache files already cached by loader
            if compiler.loader.cache().lock().contains_key(&path) {
                compiler.cache.lock().insert(path, file.clone());
            }

            file
        }

        let mut lowered_files = files
            .into_values()
            .map(|file| (*lower(self, file)).clone())
            .collect::<Vec<_>>();

        let lowering_is_complete = !self.diagnostics.contains_errors();
        let entrypoint = lowered_files.pop().unwrap();

        (entrypoint, lowering_is_complete)
    }

    pub fn typecheck_with(
        &self,
        entrypoint: lower::File,
        lowering_is_complete: bool,
        options: &Options,
    ) -> Program {
        self.typecheck_with_progress(entrypoint, lowering_is_complete, move |p| {
            if let Some(progress) = &options.progress {
                progress.lock()(Progress::Typechecking(p))
            }
        })
    }

    pub fn lint_with(&self, program: &Program, options: &Options) {
        if options.lint {
            self.lint(program);
        }
    }
}
