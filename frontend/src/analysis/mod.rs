#![allow(clippy::too_many_arguments, clippy::type_complexity)]

pub mod lint;
pub mod lower;
pub mod optimize;
pub mod span;
pub mod typecheck;

pub use span::{Span, SpanList};
pub use typecheck::{
    Arm, Bound, Expression, ExpressionKind, Intrinsic, LiteralKind, Pattern, PatternKind, Program,
    Type, TypeAnnotation, TypeAnnotationKind, TypeStructure,
};
pub use wipple_syntax::ast;

use crate::{
    diagnostics::*,
    helpers::{InternedString, Shared},
    BuiltinSyntaxId, Compiler, FileKind, FilePath, ScopeId, SyntaxId,
};
use async_trait::async_trait;
use futures::future::BoxFuture;
use std::{
    collections::{BTreeMap, HashMap},
    sync::{atomic::AtomicUsize, Arc},
};
use wipple_syntax::{ast::builtin_syntax_definitions, DriverExt};
use wipple_util::Backtrace;

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

impl std::fmt::Debug for Options {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Options").finish()
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

impl Compiler {
    pub fn analyze_with(
        &self,
        entrypoint: FilePath,
        options: &Options,
    ) -> BoxFuture<'static, (Program, FinalizedDiagnostics)> {
        let compiler = self.clone();
        let options = options.clone();

        Box::pin(async move {
            let files = compiler.expand_with(entrypoint, &options).await;
            if files.is_empty() || compiler.diagnostics.contains_errors() {
                return (Program::default(), compiler.finish_analysis());
            }

            let (entrypoint, lowering_is_complete) = compiler.lower_with(files, &options);

            let program = compiler
                .typecheck_with(entrypoint, lowering_is_complete, &options)
                .await;

            compiler.lint_with(&program, &options);

            let diagnostics = compiler.finish_analysis();
            (program, diagnostics)
        })
    }

    pub async fn expand_with(
        &self,
        entrypoint: FilePath,
        options: &Options,
    ) -> indexmap::IndexMap<FilePath, Arc<ast::File<Analysis>>> {
        let driver = Analysis {
            compiler: self.clone(),
            options: options.clone(),
            entrypoint,
            cache: Default::default(),
            count: Default::default(),
            stack: Default::default(),
        };

        let cache = driver.cache.clone();
        driver.syntax_of(None, None, entrypoint).await;

        cache.into_unique()
    }

    pub fn lower_with(
        &self,
        files: indexmap::IndexMap<FilePath, Arc<ast::File<Analysis>>>,
        _options: &Options,
    ) -> (lower::File, bool) {
        assert!(!files.is_empty(), "expected at least one file");

        fn lower(compiler: &Compiler, file: Arc<ast::File<Analysis>>) -> Arc<lower::File> {
            let path = file.file.path;

            if let Some(file) = compiler.cache.lock().get(&path) {
                return file.clone();
            }

            let dependencies = file.file.dependencies.lock().clone();

            let dependencies = dependencies
                .into_iter()
                .map(|file| lower(compiler, file))
                .collect::<Vec<_>>();

            let file = Arc::new(compiler.lower(&file, dependencies));

            // Only cache files already cached by loader
            if compiler.loader.cache().lock().contains_key(&path) {
                compiler.cache.lock().insert(path, file.clone());
            }

            file
        }

        let mut lowered_files = files
            .values()
            .map(|file| (*lower(self, file.clone())).clone())
            .collect::<Vec<_>>();

        let lowering_is_complete = !self.diagnostics.contains_errors();
        let entrypoint = lowered_files.pop().unwrap();

        (entrypoint, lowering_is_complete)
    }

    pub async fn typecheck_with(
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
        .await
    }

    pub fn lint_with(&self, program: &Program, options: &Options) {
        if options.lint {
            self.lint(program);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Analysis {
    compiler: Compiler,
    options: Options,
    entrypoint: FilePath,
    cache: Shared<indexmap::IndexMap<FilePath, Arc<wipple_syntax::ast::File<Self>>>>,
    count: Arc<AtomicUsize>,
    stack: Shared<Vec<FilePath>>,
}

#[async_trait]
impl wipple_syntax::Driver for Analysis {
    type InternedString = InternedString;
    type Path = FilePath;
    type Span = SpanList;
    type File = File;
    type Scope = ScopeId;

    fn intern(&self, s: impl AsRef<str>) -> Self::InternedString {
        InternedString::new(s)
    }

    fn make_path(&self, path: Self::InternedString) -> Option<Self::Path> {
        Some(FilePath::Path(path))
    }

    fn make_span(&self, path: Self::Path, range: std::ops::Range<usize>) -> Self::Span {
        SpanList::from(Span::new(path, range))
    }

    fn std_path(&self) -> Option<Self::Path> {
        self.compiler.loader.std_path()
    }

    async fn load_file(
        &self,
        source_file: Option<(Self::Path, Self::File)>,
        source_span: Option<Self::Span>,
        path: Self::Path,
        expand: impl FnOnce(Self::Path, Self::File) -> BoxFuture<'static, Arc<ast::File<Self>>>
            + Send
            + 'static,
    ) -> Option<Arc<ast::File<Self>>> {
        fn add_dependency(file: &Arc<ast::File<Analysis>>, source_file: File) {
            source_file
                .syntax_declarations
                .lock()
                .extend(file.file.syntax_declarations.lock().clone());

            source_file
                .scopes
                .lock()
                .extend(file.file.scopes.lock().clone());

            source_file
                .scopes
                .lock()
                .get_mut(&source_file.root_scope)
                .unwrap()
                .syntaxes
                .extend(
                    file.file
                        .scopes
                        .lock()
                        .get(&file.file.root_scope)
                        .unwrap()
                        .syntaxes
                        .clone(),
                );

            source_file.dependencies.lock().push(file.clone());
        }

        macro_rules! try_load {
            ($expr:expr) => {
                match $expr {
                    Ok(x) => x,
                    Err(error) => {
                        let mut spans = source_span
                            .map(|span| Note::primary(span, "try fixing this import"))
                            .into_iter()
                            .collect::<Vec<_>>();

                        if spans.is_empty() {
                            spans.push(Note::primary(
                                Span::new(path, 0..0),
                                "error while loading this file",
                            ));
                        }

                        self.compiler
                            .add_error(format!("cannot load file `{}`: {}", path, error), spans);

                        return None;
                    }
                }
            };
        }

        let resolved_path = try_load!(self.compiler.loader.resolve(
            path,
            FileKind::Source,
            source_file.as_ref().map(|(path, _)| *path)
        ));

        if let Some(cached) = self.compiler.loader.cache().lock().get(&resolved_path) {
            fn insert(
                file: Arc<wipple_syntax::ast::File<Analysis>>,
                files: &mut indexmap::IndexMap<FilePath, Arc<wipple_syntax::ast::File<Analysis>>>,
            ) {
                for dependency in &*file.file.dependencies.lock() {
                    insert(dependency.clone(), files);
                }

                files.insert(file.file.path, file.clone());
            }

            let mut files = self.cache.lock();
            insert(cached.clone(), &mut files);

            if let Some((_, source_file)) = source_file {
                add_dependency(cached, source_file);
            }

            return Some(cached.clone());
        }

        {
            let count = self
                .count
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
                + 1;

            if let Some(progress) = &self.options.progress {
                progress.lock()(Progress::Resolving {
                    path: resolved_path,
                    count,
                });
            }
        }

        {
            let stack = self.stack.lock();
            if let Some(index) = stack.iter().position(|path| path == &resolved_path) {
                let stack = stack[index..]
                    .iter()
                    .chain(std::iter::once(&resolved_path))
                    .map(|path| path.to_string())
                    .collect::<Vec<_>>()
                    .join(", which imports ");

                self.compiler.add_error(
                    "import cycle detected",
                    vec![Note::primary(
                        source_span.unwrap(),
                        format!("this imports {stack}"),
                    )],
                );

                return None;
            }
        }

        let code = try_load!(self.compiler.loader.load(resolved_path).await);

        self.compiler
            .loader
            .source_map()
            .lock()
            .insert(resolved_path, code.clone());

        self.stack.lock().push(resolved_path);

        let mut scopes = BTreeMap::new();
        let root_scope = self.compiler.new_scope_id_in(resolved_path);
        scopes.insert(root_scope, Scope::default());

        let file = File {
            code,
            compiler: self.compiler.clone(),
            path: resolved_path,
            dependencies: Default::default(),
            syntax_declarations: Default::default(),
            root_scope,
            scopes: Shared::new(scopes),
            builtin_syntax_uses: Default::default(),
        };

        self.stack.lock().pop();

        let file = expand(resolved_path, file).await;

        // Don't cache virtual or builtin paths, nor the entrypoint
        if resolved_path != self.entrypoint
            && !matches!(resolved_path, FilePath::Virtual(_) | FilePath::Builtin)
        {
            self.compiler
                .loader
                .cache()
                .lock()
                .insert(resolved_path, file.clone());
        }

        self.cache.lock().insert(resolved_path, file.clone());

        if let Some((_, source_file)) = source_file {
            add_dependency(&file, source_file);
        }

        Some(file)
    }

    fn syntax_error_with(
        &self,
        msgs: impl IntoIterator<Item = (Self::Span, String)>,
        fix: Option<wipple_syntax::Fix>,
    ) {
        let mut msgs = msgs.into_iter();
        let mut notes = Vec::with_capacity(msgs.size_hint().0);

        let (primary_span, primary_msg) = msgs.next().expect("must provide at least one message");
        notes.push(Note::primary(primary_span, primary_msg));

        for (span, msg) in msgs {
            notes.push(Note::secondary(span, msg));
        }

        let mut error = self.compiler.error("syntax error", notes);
        if let Some(fix) = fix {
            error = error.fix_with(
                fix.description,
                FixRange(fix.range.range()),
                fix.replacement,
            );
        }

        self.compiler.add_diagnostic(error);
    }

    fn backtrace(&self) -> Backtrace {
        self.compiler.backtrace()
    }
}

impl wipple_syntax::Span for SpanList {
    fn join(left: Self, right: Self) -> Self {
        SpanList::join(left, right)
    }

    fn merge(&mut self, other: Self) {
        *self = SpanList::merge(*self, other);
    }

    fn set_caller(&mut self, caller: Self) {
        self.set_caller(caller);
    }

    fn range(&self) -> std::ops::Range<usize> {
        self.first().primary_range()
    }
}

#[derive(Debug, Clone)]
pub struct File {
    compiler: Compiler,
    path: FilePath,
    code: Arc<str>,
    dependencies: Shared<Vec<Arc<wipple_syntax::ast::File<Analysis>>>>,
    syntax_declarations:
        Shared<BTreeMap<SyntaxId, wipple_syntax::ast::SyntaxAssignmentValue<Analysis>>>,
    root_scope: ScopeId,
    scopes: Shared<BTreeMap<ScopeId, Scope>>,
    builtin_syntax_uses: Shared<
        HashMap<
            &'static str,
            (
                BuiltinSyntaxId,
                wipple_syntax::ast::BuiltinSyntaxDefinition,
                Vec<SpanList>,
            ),
        >,
    >,
}

#[derive(Debug, Clone, Default)]
struct Scope {
    parent: Option<ScopeId>,
    syntaxes: HashMap<InternedString, Option<SyntaxId>>,
}

impl wipple_syntax::File<Analysis> for File {
    fn code(&self) -> &str {
        &self.code
    }

    fn root_scope(&self) -> ScopeId {
        self.root_scope
    }

    fn make_scope(&self, parent: ScopeId) -> ScopeId {
        let id = self.compiler.new_scope_id_in(self.path);

        self.scopes.lock().insert(
            id,
            Scope {
                parent: Some(parent),
                ..Default::default()
            },
        );

        id
    }

    fn define_syntax(
        &self,
        name: InternedString,
        scope: ScopeId,
        value: wipple_syntax::ast::SyntaxAssignmentValue<Analysis>,
    ) {
        let id = self.compiler.new_syntax_id_in(self.path);

        self.syntax_declarations.lock().insert(id, value);

        self.scopes
            .lock()
            .get_mut(&scope)
            .unwrap()
            .syntaxes
            .insert(name, Some(id));
    }

    fn add_barrier(&self, name: InternedString, scope: ScopeId) {
        self.scopes
            .lock()
            .get_mut(&scope)
            .unwrap()
            .syntaxes
            .insert(name, None);
    }

    fn resolve_syntax(
        &self,
        span: SpanList,
        name: InternedString,
        scope: ScopeId,
    ) -> Option<wipple_syntax::ast::SyntaxAssignmentValue<Analysis>> {
        let scopes = self.scopes.lock();

        let mut parent = Some(scope);
        while let Some(scope) = parent {
            let scope = scopes.get(&scope).unwrap();

            if let Some(syntax) = scope.syntaxes.get(&name) {
                let mut syntax_declarations = self.syntax_declarations.lock();
                let syntax = syntax_declarations.get_mut(syntax.as_ref()?).unwrap();
                syntax.uses.push(span);

                return Some(syntax.clone());
            }

            parent = scope.parent;
        }

        None
    }

    fn use_builtin_syntax(&self, span: SpanList, name: &'static str) {
        let compiler = self.compiler.clone();
        let mut builtin_syntax_uses = self.builtin_syntax_uses.lock();

        if let Some((_, _, uses)) = builtin_syntax_uses.get_mut(&name) {
            uses.push(span);
        } else if let Some(definition) = builtin_syntax_definitions()
            .into_iter()
            .find(|definition| definition.name == name)
        {
            builtin_syntax_uses.insert(
                name,
                (compiler.new_builtin_syntax_id(), definition, vec![span]),
            );
        }
    }
}

impl Compiler {
    fn source_code_for_span(&self, span: Span) -> Option<String> {
        Some(
            self.loader
                .source_map()
                .lock()
                .get(&span.path)?
                .get(span.primary_range())?
                .to_string(),
        )
    }
}
