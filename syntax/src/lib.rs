#![warn(clippy::dbg_macro, clippy::todo)]

pub mod ast;
pub mod parse;

use async_trait::async_trait;
use futures::future::BoxFuture;
use serde::{Deserialize, Serialize};
use std::{fmt::Debug, hash::Hash, ops::Range, sync::Arc};
use sync_wrapper::SyncFuture;
use wipple_util::Backtrace;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct CharIndex {
    pub utf8: usize,
    pub utf16: usize,
}

impl CharIndex {
    pub const ZERO: CharIndex = CharIndex { utf8: 0, utf16: 0 };
}

impl std::ops::Add for CharIndex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        CharIndex {
            utf8: self.utf8 + rhs.utf8,
            utf16: self.utf16 + rhs.utf16,
        }
    }
}

pub type ScopeSet<S> = im::OrdSet<S>;

#[async_trait]
pub trait Driver: Debug + Clone + Send + Sync + 'static {
    type InternedString: Debug + Clone + Eq + AsRef<str> + Eq + Hash + Send + Sync;
    type Path: Debug + Copy + Send + Sync + 'static;
    type Span: Debug + Copy + Span<InternedString = Self::InternedString> + Send + Sync + 'static;
    type File: Debug + Clone + Send + Sync + File<Self> + 'static;
    type Scope: Debug + Copy + Eq + Ord + Send + Sync;

    fn intern(&self, s: impl AsRef<str>) -> Self::InternedString;
    fn make_path(&self, path: Self::InternedString) -> Option<Self::Path>;
    fn make_span(&self, path: Self::Path, range: std::ops::Range<CharIndex>) -> Self::Span;

    fn implicit_entrypoint_imports(&self) -> Vec<Self::Path>;
    fn implicit_dependency_imports(&self) -> Vec<Self::Path>;

    /// Allows the driver to download files in parallel so loading is faster.
    async fn queue_files(&self, source_path: Option<Self::Path>, paths: Vec<Self::Path>) {
        let _ = (source_path, paths); // do nothing
    }

    async fn expand_file(
        &self,
        source_file: Option<(Self::Path, Self::File)>,
        source_span: Option<Self::Span>,
        path: Self::Path,
        expand: impl FnOnce(Self::Path, Self::File) -> BoxFuture<'static, Arc<ast::File<Self>>>
            + Send
            + 'static,
    ) -> Option<Arc<ast::File<Self>>>;

    fn syntax_error_with(
        &self,
        msgs: impl IntoIterator<Item = (Self::Span, String)>,
        fix: Option<Fix>,
    );

    fn syntax_error(&self, span: Self::Span, msg: impl ToString) {
        self.syntax_error_with([(span, msg.to_string())], None)
    }

    fn backtrace(&self) -> Backtrace;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolveSyntaxError {
    NotFound,
    Ambiguous,
}

pub trait File<D: Driver> {
    fn is_entrypoint(&self) -> bool;

    fn code(&self) -> &str;

    fn make_scope(&self) -> D::Scope;

    fn define_syntax(
        &self,
        name: D::InternedString,
        scope_set: ScopeSet<D::Scope>,
        value: ast::SyntaxAssignmentValue<D>,
    );

    fn resolve_syntax(
        &self,
        span: D::Span,
        name: D::InternedString,
        scope_set: ScopeSet<D::Scope>,
    ) -> Result<ast::SyntaxAssignmentValue<D>, ResolveSyntaxError>;

    fn define_constant(
        &self,
        name: D::InternedString,
        visible_scope_set: ScopeSet<D::Scope>,
        body_scope_set: ScopeSet<D::Scope>,
    );

    fn resolve_constant_body(
        &self,
        name: D::InternedString,
        scope_set: ScopeSet<D::Scope>,
    ) -> Result<ScopeSet<D::Scope>, ResolveSyntaxError>;

    fn use_builtin_syntax(&self, span: D::Span, name: &'static str);

    fn define_snippet(&self, name: D::InternedString, value: ast::SnippetAssignmentValue<D>);
}

pub trait Span: Sized + Debug {
    type InternedString: Debug + Clone + Eq + AsRef<str> + Eq + Hash + Send + Sync;

    fn join(left: Self, right: Self) -> Self;

    fn merge(&mut self, other: Self);

    fn set_expanded_from_operator(
        &mut self,
        name: Self::InternedString,
        left: Option<(Self, Vec<Self>)>,
        right: Option<(Self, Vec<Self>)>,
    );

    fn set_caller(&mut self, caller: Self);

    fn range(&self) -> Range<CharIndex>;

    fn merged_with(mut self, other: Self) -> Self
    where
        Self: Sized,
    {
        self.merge(other);
        self
    }
}

pub trait DriverExt: Driver {
    fn syntax_of(
        self,
        source_file: Option<(Self::Path, Self::File)>,
        source_span: Option<Self::Span>,
        path: Self::Path,
        options: parse::Options,
    ) -> SyncFuture<BoxFuture<'static, Option<Arc<ast::File<Self>>>>>;
}

impl<D: Driver> DriverExt for D {
    fn syntax_of(
        self,
        source_file: Option<(Self::Path, Self::File)>,
        source_span: Option<Self::Span>,
        path: Self::Path,
        options: parse::Options,
    ) -> SyncFuture<BoxFuture<'static, Option<Arc<ast::File<Self>>>>> {
        SyncFuture::new(Box::pin(async move {
            self.expand_file(source_file, source_span, path, {
                let driver = self.clone();

                move |resolved_path, driver_file| {
                    Box::pin(async move {
                        let parse_file =
                            parse::parse(&driver, resolved_path, driver_file.code(), options);

                        let file =
                            ast::build(driver, resolved_path, driver_file, parse_file, options)
                                .await;

                        Arc::new(file)
                    })
                }
            })
            .await
        }))
    }
}

#[derive(Debug, Clone)]
pub struct Fix {
    pub description: String,
    pub range: FixRange,
    pub replacement: String,
}

impl Fix {
    pub fn new(description: impl ToString, range: FixRange, replacement: impl ToString) -> Self {
        Fix {
            description: description.to_string(),
            range,
            replacement: replacement.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FixRange(Range<CharIndex>);

impl FixRange {
    pub fn replace(span: impl Span) -> FixRange {
        FixRange(span.range())
    }

    pub fn before(span: impl Span) -> FixRange {
        FixRange(span.range().start..span.range().start)
    }

    pub fn after(span: impl Span) -> FixRange {
        FixRange(span.range().end..span.range().end)
    }

    pub fn range(&self) -> Range<CharIndex> {
        self.0.clone()
    }
}

#[derive(Debug, Clone)]
pub struct SingleFile(pub String);

impl<D: Driver<Span = (), File = SingleFile, Scope = ()>> File<D> for SingleFile {
    fn is_entrypoint(&self) -> bool {
        true
    }

    fn code(&self) -> &str {
        &self.0
    }

    fn make_scope(&self) -> D::Scope {}

    fn define_syntax(
        &self,
        _name: D::InternedString,
        _scope_set: ScopeSet<D::Scope>,
        _value: ast::SyntaxAssignmentValue<D>,
    ) {
        // do nothing
    }

    fn resolve_syntax(
        &self,
        _span: D::Span,
        _name: D::InternedString,
        _scope_set: ScopeSet<D::Scope>,
    ) -> Result<ast::SyntaxAssignmentValue<D>, ResolveSyntaxError> {
        Err(ResolveSyntaxError::NotFound)
    }

    fn define_constant(
        &self,
        _name: D::InternedString,
        _visible_scope_set: ScopeSet<D::Scope>,
        _body_scope_set: ScopeSet<D::Scope>,
    ) {
        // do nothing
    }

    fn resolve_constant_body(
        &self,
        _name: D::InternedString,
        _scope_set: ScopeSet<D::Scope>,
    ) -> Result<ScopeSet<D::Scope>, ResolveSyntaxError> {
        Err(ResolveSyntaxError::NotFound)
    }

    fn use_builtin_syntax(&self, _span: D::Span, _name: &'static str) {
        // do nothing
    }

    fn define_snippet(&self, _name: D::InternedString, _value: ast::SnippetAssignmentValue<D>) {
        // do nothing
    }
}

impl Span for () {
    type InternedString = String;

    fn join(_left: Self, _right: Self) -> Self {}

    fn merge(&mut self, _other: Self) {
        // do nothing
    }

    fn set_expanded_from_operator(
        &mut self,
        _name: Self::InternedString,
        _left: Option<(Self, Vec<Self>)>,
        _right: Option<(Self, Vec<Self>)>,
    ) {
        // do nothing
    }

    fn set_caller(&mut self, _caller: Self) {
        // do nothing
    }

    fn range(&self) -> Range<CharIndex> {
        CharIndex::ZERO..CharIndex::ZERO
    }
}
