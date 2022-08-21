#![allow(clippy::too_many_arguments)]

pub mod ast;
pub mod expand;
pub mod lint;
pub mod lower;
pub mod typecheck;

pub use typecheck::{Arm, Expression, ExpressionKind, Pattern, PatternKind, Program};

use crate::{diagnostics::*, parse::Span, Compiler, FilePath, Loader};
use async_recursion::async_recursion;
use parking_lot::Mutex;
use std::sync::{atomic::AtomicUsize, Arc};

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

impl<L: Loader> Compiler<L> {
    pub async fn analyze(&mut self, path: FilePath) -> Option<Program> {
        self.analyze_with_progress(path, |_| {}).await
    }

    pub async fn analyze_with_progress(
        &mut self,
        path: FilePath,
        progress: impl Fn(Progress) + Send + Sync + 'static,
    ) -> Option<Program> {
        let progress = Arc::new(Mutex::new(progress));

        let files: Arc<Mutex<indexmap::IndexMap<FilePath, Arc<expand::File<L>>>>> =
            Default::default();

        #[async_recursion]
        async fn load<L: Loader>(
            compiler: &Compiler<L>,
            path: FilePath,
            source_path: Option<FilePath>,
            source_span: Option<Span>,
            progress: Arc<Mutex<impl Fn(Progress) + Send + Sync + 'static>>,
            count: Arc<AtomicUsize>,
            files: Arc<Mutex<indexmap::IndexMap<FilePath, Arc<expand::File<L>>>>>,
            stack: Arc<Mutex<Vec<FilePath>>>,
        ) -> Option<Arc<expand::File<L>>> {
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

            // Don't cache virtual or builtin paths
            if !matches!(resolved_path, FilePath::Virtual(_) | FilePath::Builtin(_)) {
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
            path,
            None,
            None,
            progress.clone(),
            Default::default(),
            files.clone(),
            Default::default(),
        )
        .await?;

        let mut cache = indexmap::IndexMap::new();

        fn lower<L: Loader>(
            compiler: &mut Compiler<L>,
            file: Arc<expand::File<L>>,
            cache: &mut indexmap::IndexMap<FilePath, Arc<lower::File>>,
        ) -> Arc<lower::File> {
            let path = file.path;

            if let Some(file) = cache.get(&path) {
                return file.clone();
            }

            let dependencies = file
                .dependencies
                .clone()
                .into_iter()
                .map(|(file, imports)| (lower(compiler, file, cache), imports))
                .collect::<Vec<_>>();

            let file = compiler.build_ast((*file).clone());

            let file = Arc::new(compiler.lower(file, dependencies));
            cache.insert(path, file.clone());

            file
        }

        #[allow(clippy::needless_collect)] // needed to ensure Arc::try_unwrap succeeds
        let lowered_files = Arc::try_unwrap(files)
            .unwrap()
            .into_inner()
            .into_values()
            .map(|file| lower(self, file, &mut cache))
            .collect::<Vec<_>>();

        drop(cache);

        let lowered_files = lowered_files
            .into_iter()
            .map(|file| Arc::try_unwrap(file).unwrap())
            .collect::<Vec<_>>();

        if self.diagnostics.contains_errors() {
            return None;
        }

        let program = self.typecheck_with_progress(lowered_files, |p| {
            progress.lock()(Progress::Typechecking(p))
        });

        Some(program)
    }
}
