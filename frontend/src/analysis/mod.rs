#![allow(clippy::too_many_arguments)]

pub mod ast;
pub mod expand;
pub mod lint;
pub mod lower;
pub mod typecheck;

pub use typecheck::{
    Arm, Expression, ExpressionKind, Pattern, PatternKind, Program, Type, TypeStructure,
    TypecheckMode,
};

use crate::{diagnostics::*, parse::Span, Compiler, FilePath, Uses};
use async_recursion::async_recursion;
use parking_lot::Mutex;
use std::sync::{atomic::AtomicUsize, Arc};

#[derive(Default)]
pub struct Options {
    progress: Option<Box<dyn Fn(Progress) + Send + Sync>>,
    typecheck_mode: typecheck::TypecheckMode,
}

impl Options {
    pub fn tracking_progress(
        mut self,
        progress: impl Fn(Progress) + Send + Sync + 'static,
    ) -> Self {
        self.progress = Some(Box::new(progress));
        self
    }

    pub fn typecheck_mode(mut self, mode: typecheck::TypecheckMode) -> Self {
        self.typecheck_mode = mode;
        self
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
    pub async fn analyze(&self, entrypoint: FilePath, options: Options) -> Program {
        let progress = Arc::new(Mutex::new(
            options.progress.unwrap_or_else(|| Box::new(|_| {})),
        ));

        let files: Arc<Mutex<indexmap::IndexMap<FilePath, Arc<expand::File>>>> = Default::default();

        #[async_recursion]
        async fn load<'a, 'l>(
            compiler: &'a Compiler<'l>,
            path: FilePath,
            source_path: Option<FilePath>,
            source_span: Option<Span>,
            entrypoint: FilePath,
            progress: Arc<Mutex<impl Fn(Progress) + Send + Sync + 'static>>,
            count: Arc<AtomicUsize>,
            files: Arc<Mutex<indexmap::IndexMap<FilePath, Arc<expand::File>>>>,
            stack: Arc<Mutex<Vec<FilePath>>>,
        ) -> Option<Arc<expand::File>> {
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
                progress.lock()(Progress::Resolving {
                    path: resolved_path,
                    count,
                });
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

                    compiler.diagnostics.add(Diagnostic::error(
                        "import cycle detected",
                        vec![Note::primary(
                            source_span.unwrap(),
                            format!("this imports {}", stack),
                        )],
                    ));

                    return None;
                }
            }

            let code = try_load!(compiler.loader.load(resolved_path).await);

            compiler
                .loader
                .source_map()
                .lock()
                .insert(resolved_path, code.clone());

            let file = compiler.parse(resolved_path, &code)?;

            stack.lock().push(resolved_path);

            let file = compiler
                .expand(file, {
                    let progress = progress.clone();
                    let count = count.clone();
                    let files = files.clone();
                    let stack = stack.clone();

                    move |compiler, source_span, new_path| {
                        let progress = progress.clone();
                        let count = count.clone();
                        let files = files.clone();
                        let stack = stack.clone();

                        Box::pin(async move {
                            load(
                                compiler,
                                new_path,
                                Some(resolved_path),
                                Some(source_span),
                                entrypoint,
                                progress,
                                count,
                                files,
                                stack,
                            )
                            .await
                        })
                    }
                })
                .await?;

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

        load(
            self,
            entrypoint,
            None,
            None,
            entrypoint,
            progress.clone(),
            Default::default(),
            files.clone(),
            Default::default(),
        )
        .await;

        let files = Arc::try_unwrap(files).unwrap().into_inner();

        let mut uses = Uses::default();
        for (_, file) in &files {
            for (id, spans) in &file.template_uses {
                for span in spans {
                    uses.record_use_of_template(*id, *span);
                }
            }
        }

        let uses = Arc::new(Mutex::new(uses));

        fn lower(
            compiler: &Compiler,
            file: Arc<expand::File>,
            uses: Arc<Mutex<Uses>>,
        ) -> Arc<lower::File> {
            let path = file.path;

            if let Some(file) = compiler.cache.lock().get(&path) {
                return file.clone();
            }

            let dependencies = file
                .dependencies
                .clone()
                .into_iter()
                .map(|(file, imports)| (lower(compiler, file, uses.clone()), imports))
                .collect::<Vec<_>>();

            let file = compiler.build_ast((*file).clone());

            let file = Arc::new(compiler.lower(file, dependencies, uses));

            // Only cache files already cached by loader
            if compiler.loader.cache().lock().contains_key(&path) {
                compiler.cache.lock().insert(path, file.clone());
            }

            file
        }

        let lowered_files = files
            .into_values()
            .map(|file| (*lower(self, file, uses.clone())).clone())
            .collect::<Vec<_>>();

        let uses = Arc::try_unwrap(uses).unwrap().into_inner();

        let lowering_is_complete = !self.diagnostics.contains_errors();

        self.typecheck_with_progress(
            lowered_files,
            uses,
            options.typecheck_mode,
            lowering_is_complete,
            move |p| progress.lock()(Progress::Typechecking(p)),
        )
    }
}
