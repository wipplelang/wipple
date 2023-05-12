pub mod ast;
pub mod parse;

use async_trait::async_trait;
use futures::future::BoxFuture;
use std::{fmt::Debug, hash::Hash, ops::Range, sync::Arc};
use sync_wrapper::SyncFuture;
use wipple_util::Backtrace;

#[async_trait]
pub trait Driver: Debug + Clone + Send + Sync + 'static {
    type InternedString: Debug + Clone + AsRef<str> + Eq + Hash + Send + Sync;
    type Path: Debug + Copy + Send + Sync + 'static;
    type Span: Debug + Copy + Span + Send + Sync + 'static;
    type File: Debug + Clone + Send + Sync + File<Self> + 'static;
    type Scope: Debug + Copy + Send + Sync;

    fn intern(&self, s: impl AsRef<str>) -> Self::InternedString;
    fn make_path(&self, path: Self::InternedString) -> Option<Self::Path>;
    fn make_span(&self, path: Self::Path, range: std::ops::Range<usize>) -> Self::Span;

    fn std_path(&self) -> Option<Self::Path>;

    async fn load_file(
        &self,
        source_file: Option<(Self::Path, Self::File)>,
        source_span: Option<Self::Span>,
        path: Self::Path,
        expand: impl FnOnce(Self::Path, Self::File) -> BoxFuture<'static, Arc<ast::File<Self>>>
            + Send
            + 'static,
    ) -> Option<Arc<ast::File<Self>>>;

    fn syntax_error_with(&self, msgs: impl IntoIterator<Item = (Self::Span, String)>);

    fn syntax_error(&self, span: Self::Span, msg: impl ToString) {
        self.syntax_error_with([(span, msg.to_string())])
    }

    fn backtrace(&self) -> Backtrace;
}

pub trait File<D: Driver> {
    fn code(&self) -> &str;

    fn root_scope(&self) -> D::Scope;

    fn make_scope(&self, parent: D::Scope) -> D::Scope;

    fn define_syntax(
        &self,
        name: D::InternedString,
        scope: D::Scope,
        value: ast::SyntaxAssignmentValue<D>,
    );

    fn add_barrier(&self, name: D::InternedString, scope: D::Scope);

    fn resolve_syntax(
        &self,
        span: D::Span,
        name: D::InternedString,
        scope: D::Scope,
    ) -> Option<ast::SyntaxAssignmentValue<D>>;
}

pub trait Span {
    fn join(left: Self, right: Self) -> Self;
    fn merge(&mut self, other: Self);
    fn range(self) -> Range<usize>;
}

pub trait DriverExt: Driver {
    fn syntax_of(
        self,
        source_file: Option<(Self::Path, Self::File)>,
        source_span: Option<Self::Span>,
        path: Self::Path,
    ) -> SyncFuture<BoxFuture<'static, Option<Arc<ast::File<Self>>>>>;
}

impl<D: Driver> DriverExt for D {
    fn syntax_of(
        self,
        source_file: Option<(Self::Path, Self::File)>,
        source_span: Option<Self::Span>,
        path: Self::Path,
    ) -> SyncFuture<BoxFuture<'static, Option<Arc<ast::File<Self>>>>> {
        SyncFuture::new(Box::pin(async move {
            self.load_file(source_file, source_span, path, {
                let driver = self.clone();

                move |resolved_path, driver_file| {
                    Box::pin(async move {
                        let parse_file = parse::parse(&driver, resolved_path, driver_file.code());
                        let file = ast::build(driver, resolved_path, driver_file, parse_file).await;
                        Arc::new(file)
                    })
                }
            })
            .await
        }))
    }
}

#[cfg(feature = "arbitrary")]
pub struct FuzzString(String);

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for FuzzString {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(FuzzString(
            ('a'..='z').nth(u.choose_index(16)?).unwrap().to_string(),
        ))
    }
}

#[cfg(feature = "arbitrary")]
pub trait FuzzDriver: Driver<Span = (), Scope = (), InternedString = FuzzString> {}
