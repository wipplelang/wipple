mod builtin;

pub use builtin::*;

use crate::{
    helpers::{InternedString, Shared},
    parse::{self, Span},
    Compiler, FilePath, ScopeId, TemplateId,
};
use futures::future::BoxFuture;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    sync::Arc,
};

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub attributes: FileAttributes,
    pub syntax_declarations: BTreeMap<TemplateId, SyntaxAssignmentValue>,
    pub root_scope: ScopeId,
    pub scopes: BTreeMap<ScopeId, (Option<Span>, Option<ScopeId>)>,
    pub statements: Vec<Statement>,
}

impl Compiler {
    pub(crate) fn build_ast_v2(
        &self,
        file: parse::File,
        load: impl Fn(&Compiler, Span, FilePath) -> BoxFuture<'static, Option<Arc<File>>>
            + 'static
            + Send
            + Sync,
    ) -> File {
        todo!()
    }
}

#[derive(Debug, Clone)]
struct Scope {
    id: ScopeId,
    span: Option<Span>,
    parent: Option<ScopeId>,
    syntaxes: HashMap<InternedString, UserDefinedSyntax>,
}

#[derive(Debug, Clone)]
struct UserDefinedSyntax {
    rules: Vec<UserDefinedSyntaxRule>,
}

#[derive(Debug, Clone)]
struct UserDefinedSyntaxRule {
    span: Span,
    pattern: Expression,
    body: Expression,
}

#[derive(Clone)]
struct AstBuilder {
    file: FilePath,
    compiler: Compiler,
    dependencies: Shared<HashMap<FilePath, (Arc<File>, Option<HashMap<InternedString, Span>>)>>,
    attributes: Shared<FileAttributes>,
    scopes: Shared<BTreeMap<ScopeId, Scope>>,
    file_scope: Option<ScopeId>,
    load: Arc<
        dyn Fn(Compiler, Span, FilePath) -> BoxFuture<'static, Option<Arc<File>>> + Send + Sync,
    >,
    expanded: Shared<HashSet<Span>>,
}
