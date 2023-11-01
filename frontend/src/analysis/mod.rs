#![allow(clippy::too_many_arguments, clippy::type_complexity)]

pub mod lint;
pub mod lower;
pub mod optimize;
pub mod span;
pub mod typecheck;

pub use span::{Span, SpanList};
pub use typecheck::{
    Arm, Bound, Expression, ExpressionKind, Intrinsic, LiteralKind, Pattern, PatternKind, Program,
    Semantics, Type, TypeAnnotation, TypeAnnotationKind, TypeKind, TypeStructure,
};
pub use wipple_syntax::{ast, parse};

pub type ScopeSet = wipple_syntax::ScopeSet<ScopeId>;

use crate::{
    diagnostics::*,
    helpers::{InternedString, Shared},
    BuiltinSyntaxId, Compiler, FileKind, FilePath, ScopeId, SnippetId, SyntaxId,
};
use async_trait::async_trait;
use futures::future::BoxFuture;
use itertools::Itertools;
use std::{
    collections::{BTreeMap, HashMap},
    sync::{atomic::AtomicUsize, Arc},
};
use wipple_syntax::{ast::builtin_syntax_definitions, CharIndex, DriverExt, ResolveSyntaxError};
use wipple_util::Backtrace;

#[derive(Clone)]
pub struct Options {
    progress: Option<Shared<Box<dyn Fn(Progress) + Send + Sync>>>,
    lint: bool,
    implicit_imports: Vec<FilePath>,
    parse: parse::Options,
}

impl Options {
    pub fn new() -> Self {
        Options {
            progress: None,
            lint: true,
            implicit_imports: Vec::new(),
            parse: Default::default(),
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

    pub fn with_implicit_imports(mut self, imports: impl IntoIterator<Item = FilePath>) -> Self {
        self.implicit_imports = Vec::from_iter(imports);
        self
    }

    pub fn with_parse_options(mut self, options: parse::Options) -> Self {
        self.parse = options;
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
        driver
            .syntax_of(None, None, entrypoint, options.parse)
            .await;

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
    cache: Shared<indexmap::IndexMap<FilePath, Arc<ast::File<Self>>>>,
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

    fn make_span(&self, path: Self::Path, range: std::ops::Range<CharIndex>) -> Self::Span {
        SpanList::from(Span::new(path, range))
    }

    fn implicit_entrypoint_imports(&self) -> Vec<Self::Path> {
        self.compiler
            .loader
            .std_path()
            .into_iter()
            .chain(self.options.implicit_imports.clone())
            .collect()
    }

    fn implicit_dependency_imports(&self) -> Vec<Self::Path> {
        Vec::from_iter(self.compiler.loader.std_path())
    }

    async fn queue_files(&self, source_path: Option<Self::Path>, paths: Vec<Self::Path>) {
        let queue = self.compiler.loader.queue();

        futures::future::join_all(
            paths
                .into_iter()
                .filter_map(|path| {
                    self.compiler
                        .loader
                        .resolve(path, FileKind::Source, source_path)
                        .ok()
                })
                .filter(|resolved_path| !queue.contains(resolved_path))
                .map(|resolved_path| async move {
                    let _ = self.compiler.loader.load(resolved_path).await;
                }),
        )
        .await;
    }

    async fn expand_file(
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

            let dependency_syntaxes = file.file.syntaxes.lock().clone();
            for (name, dependency_syntaxes) in dependency_syntaxes {
                source_file.syntaxes.lock().entry(name).or_default().extend(
                    dependency_syntaxes
                        .into_iter()
                        .map(|(_, syntaxes)| (ScopeSet::new(), syntaxes)),
                );
            }

            let dependency_snippets = file.file.snippets.lock().clone();
            for (name, dependency_snippets) in dependency_snippets {
                source_file
                    .snippets
                    .lock()
                    .entry(name)
                    .or_default()
                    .extend(dependency_snippets);
            }

            source_file.dependencies.lock().push(file.clone());
        }

        macro_rules! try_load {
            ($expr:expr) => {
                match $expr {
                    Ok(x) => x,
                    Err(error) => {
                        let span = source_span.unwrap_or_else(|| {
                            Span::new(path, CharIndex::ZERO..CharIndex::ZERO).into()
                        });

                        self.compiler.add_error(
                            span,
                            format!("cannot load file `{}`: {}", path, error),
                            "missing-file",
                        );

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
                file: Arc<ast::File<Analysis>>,
                files: &mut indexmap::IndexMap<FilePath, Arc<ast::File<Analysis>>>,
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
                    source_span.unwrap(),
                    format!("import cycle detected: this imports {stack}"),
                    "",
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

        let file = File {
            is_entrypoint: source_file.is_none(),
            code,
            compiler: self.compiler.clone(),
            path: resolved_path,
            dependencies: Default::default(),
            syntax_declarations: Default::default(),
            syntaxes: Default::default(),
            builtin_syntax_uses: Default::default(),
            constants: Default::default(),
            snippet_declarations: Default::default(),
            snippets: Default::default(),
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

        let (primary_span, primary_msg) = msgs.next().expect("must provide at least one message");

        let mut error = self
            .compiler
            .error(primary_span, primary_msg, "syntax-error");

        error.notes = msgs.map(|(span, msg)| Note::secondary(span, msg)).collect();

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
    type InternedString = InternedString;

    fn join(left: Self, right: Self) -> Self {
        SpanList::join(left, right)
    }

    fn merge(&mut self, other: Self) {
        *self = SpanList::merge(*self, other);
    }

    fn set_expanded_from_operator(
        &mut self,
        name: InternedString,
        left: Option<(Self, Vec<Self>)>,
        right: Option<(Self, Vec<Self>)>,
    ) {
        self.set_expanded_from_operator(name, left, right);
    }

    fn set_caller(&mut self, caller: Self) {
        self.set_caller(caller);
    }

    fn range(&self) -> std::ops::Range<CharIndex> {
        self.first().primary_range()
    }
}

#[derive(Debug, Clone)]
pub struct File {
    compiler: Compiler,
    path: FilePath,
    is_entrypoint: bool,
    code: Arc<str>,
    dependencies: Shared<Vec<Arc<ast::File<Analysis>>>>,
    syntax_declarations: Shared<BTreeMap<SyntaxId, ast::SyntaxAssignmentValue<Analysis>>>,
    syntaxes: Shared<HashMap<InternedString, Vec<(ScopeSet, SyntaxId)>>>,
    builtin_syntax_uses: Shared<
        HashMap<
            &'static str,
            (
                BuiltinSyntaxId,
                ast::BuiltinSyntaxDefinition,
                im::HashSet<SpanList>,
            ),
        >,
    >,
    constants: Shared<HashMap<InternedString, Vec<(ScopeSet, ScopeSet)>>>,
    snippet_declarations: Shared<HashMap<SnippetId, ast::SnippetAssignmentValue<Analysis>>>,
    snippets: Shared<HashMap<InternedString, Vec<SnippetId>>>,
}

impl wipple_syntax::File<Analysis> for File {
    fn is_entrypoint(&self) -> bool {
        self.is_entrypoint
    }

    fn code(&self) -> &str {
        &self.code
    }

    fn make_scope(&self) -> ScopeId {
        self.compiler.new_scope_id_in(self.path)
    }

    fn define_syntax(
        &self,
        name: InternedString,
        scope_set: ScopeSet,
        value: ast::SyntaxAssignmentValue<Analysis>,
    ) {
        let id = self.compiler.new_syntax_id_in(self.path);

        self.syntax_declarations.lock().insert(id, value);

        self.syntaxes
            .lock()
            .entry(name)
            .or_default()
            .push((scope_set, id));
    }

    fn resolve_syntax(
        &self,
        span: SpanList,
        name: InternedString,
        scope_set: ScopeSet,
    ) -> Result<ast::SyntaxAssignmentValue<Analysis>, ResolveSyntaxError> {
        let syntaxes = self.syntaxes.lock();

        let mut candidates = syntaxes
            .get(&name)
            .ok_or(ResolveSyntaxError::NotFound)?
            .iter()
            .filter(|(candidate, _)| candidate.is_subset(&scope_set))
            .max_set_by_key(|(candidate, _)| {
                candidate.clone().intersection(scope_set.clone()).len()
            })
            .into_iter()
            .map(|(_, syntax)| *syntax)
            .unique()
            .collect::<Vec<_>>();

        match candidates.len() {
            0 => Err(ResolveSyntaxError::NotFound),
            1 => {
                let syntax = candidates.pop().unwrap();

                let mut syntax_declarations = self.syntax_declarations.lock();
                let syntax = syntax_declarations.get_mut(&syntax).unwrap();
                syntax.uses.push(span);

                Ok(syntax.clone())
            }
            _ => Err(ResolveSyntaxError::Ambiguous),
        }
    }

    fn use_builtin_syntax(&self, span: SpanList, name: &'static str) {
        let compiler = self.compiler.clone();
        let mut builtin_syntax_uses = self.builtin_syntax_uses.lock();

        if let Some((_, _, uses)) = builtin_syntax_uses.get_mut(&name) {
            uses.insert(span);
        } else if let Some(definition) = builtin_syntax_definitions()
            .into_iter()
            .find(|definition| definition.name == name)
        {
            builtin_syntax_uses.insert(
                name,
                (
                    compiler.new_builtin_syntax_id(),
                    definition,
                    im::HashSet::from_iter([span]),
                ),
            );
        }
    }

    fn define_constant(
        &self,
        name: InternedString,
        visible_scope_set: ScopeSet,
        body_scope_set: ScopeSet,
    ) {
        self.constants
            .lock()
            .entry(name)
            .or_default()
            .push((visible_scope_set, body_scope_set));
    }

    fn resolve_constant_body(
        &self,
        name: InternedString,
        scope_set: ScopeSet,
    ) -> Result<ScopeSet, ResolveSyntaxError> {
        let mut constants = self.constants.lock();

        let mut candidates = constants
            .get(&name)
            .ok_or(ResolveSyntaxError::NotFound)?
            .iter()
            .enumerate()
            .filter(|(_, (candidate, _))| candidate.is_subset(&scope_set))
            .max_set_by_key(|(_, (candidate, _))| {
                candidate.clone().intersection(scope_set.clone()).len()
            })
            .into_iter()
            .map(|(index, _)| index)
            .collect::<Vec<_>>();

        match candidates.len() {
            0 => Err(ResolveSyntaxError::NotFound),
            1 => {
                let constant = candidates.pop().unwrap();

                // Constants may only be initialized once
                let (_, scope) = constants.get_mut(&name).unwrap().remove(constant);

                Ok(scope)
            }
            _ => Err(ResolveSyntaxError::Ambiguous),
        }
    }

    fn define_snippet(&self, name: InternedString, value: ast::SnippetAssignmentValue<Analysis>) {
        let id = self.compiler.new_snippet_id_in(self.path);
        self.snippet_declarations.lock().insert(id, value);
        self.snippets.lock().entry(name).or_default().push(id);
    }
}

impl Compiler {
    fn source_code_for_span(&self, span: Span) -> Option<String> {
        Some(
            self.loader
                .source_map()
                .lock()
                .get(&span.path)?
                .get(span.primary_range().start.utf8..span.primary_range().end.utf8)?
                .to_string(),
        )
    }

    fn single_line_source_code_for_span(&self, span: Span) -> Option<String> {
        self.source_code_for_span(span)
            .filter(|code| !code.contains('\n'))
    }
}
