mod operators;
mod syntax;

pub use syntax::{Expression, ExpressionKind, Statement};

use crate::{
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
    Compiler, FilePath, ScopeId, TemplateId,
};
use futures::{future::BoxFuture, stream, StreamExt};
use operators::*;
use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    mem,
    sync::Arc,
};
use syntax::*;

#[derive(Debug, Clone)]
pub struct File {
    pub span: Span,
    pub dependencies: Vec<(FilePath, Option<HashMap<InternedString, Span>>)>,
    pub attributes: FileAttributes,
    pub declarations: Declarations,
    pub scope: ScopeId,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Default)]
pub struct FileAttributes {
    pub no_std: bool,
    pub recursion_limit: Option<usize>,
}

#[derive(Debug, Clone, Default)]
pub struct StatementAttributes {
    pub language_item: Option<LanguageItem>,
    pub help: VecDeque<InternedString>,
    pub on_unimplemented: Option<InternedString>,
    pub on_mismatch: VecDeque<(Option<(Span, InternedString)>, InternedString)>,
    pub specialize: bool,
    pub allow_overlapping_instances: bool,
    pub keyword: bool,
}

#[derive(Debug, Clone, Copy, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum LanguageItem {
    Boolean,
}

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub operators: BTreeMap<TemplateId, SyntaxDeclaration<Operator>>,
    pub syntaxes: BTreeMap<TemplateId, SyntaxDeclaration<SyntaxDefinition>>,
}

impl Declarations {
    fn merge(&mut self, other: Declarations) {
        self.operators.extend(other.operators);
        self.syntaxes.extend(other.syntaxes);
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxDeclaration<T> {
    pub name: InternedString,
    pub span: Span,
    pub uses: HashSet<Span>,
    pub attributes: SyntaxDeclarationAttributes,
    pub value: T,
}

impl<T> SyntaxDeclaration<T> {
    fn new(name: impl AsRef<str>, span: Span, value: T) -> Self {
        SyntaxDeclaration {
            name: InternedString::new(name),
            span,
            uses: Default::default(),
            attributes: Default::default(),
            value,
        }
    }

    fn keyword(mut self) -> Self {
        self.attributes.keyword = true;
        self
    }
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct SyntaxDeclarationAttributes {
    pub keyword: bool,
    pub help: VecDeque<InternedString>,
}

impl<'l> Compiler<'l> {
    pub(crate) async fn expand(
        &self,
        file: parse::File,
        load: impl for<'a> Fn(&'a Compiler<'l>, Span, FilePath) -> BoxFuture<'a, Option<Arc<File>>>
            + 'static
            + Send
            + Sync,
    ) -> File {
        let expander = Expander {
            compiler: self,
            file: file.span.path,
            dependencies: Default::default(),
            attributes: Default::default(),
            declarations: Default::default(),
            scopes: Default::default(),
            load: Arc::new(load),
            expanded: Default::default(),
        };

        let scope = expander.new_scope();

        let file_attributes = file
            .attributes
            .into_iter()
            .map(Expression::from)
            .collect::<Vec<_>>();

        expander
            .expand_file_attributes(file_attributes, scope)
            .await;

        let mut statements = file
            .statements
            .into_iter()
            .map(Statement::from)
            .collect::<Vec<_>>();

        loop {
            let old_statements = statements.clone();

            statements = stream::iter(statements)
                .then(|statement| expander.expand_statement(statement, scope))
                .collect::<Vec<_>>()
                .await;

            let expanded = mem::take(&mut *expander.expanded.lock());

            if expanded.is_empty() {
                break;
            }

            if statements == old_statements {
                self.add_error(
                    "infinite recursion detected during syntax expansion",
                    expanded
                        .into_iter()
                        .map(|span| Note::primary(span, "while expanding this"))
                        .collect(),
                );

                break;
            }

            let mut block = Expression {
                span: file.span,
                kind: ExpressionKind::Block(Some(scope), statements),
            };

            block.traverse_mut_with(scope, |expr, inherited_scope| match &mut expr.kind {
                ExpressionKind::Block(scope, _) | ExpressionKind::TypeFunction(scope, _, _) => {
                    *scope.get_or_insert_with(|| expander.compiler.new_scope_id())
                }
                ExpressionKind::Name(scope, _) => *scope.get_or_insert(inherited_scope),
                _ => inherited_scope,
            });

            statements = match block.kind {
                ExpressionKind::Block(_, statements) => statements,
                _ => unreachable!(),
            };
        }

        File {
            span: file.span,
            dependencies: expander.dependencies.into_unique(),
            attributes: expander.attributes.into_unique(),
            declarations: expander.declarations.into_unique(),
            scope,
            statements,
        }
    }
}

pub(crate) struct Expander<'a, 'l> {
    file: FilePath,
    compiler: &'a Compiler<'l>,
    dependencies: Shared<Vec<(FilePath, Option<HashMap<InternedString, Span>>)>>,
    attributes: Shared<FileAttributes>,
    declarations: Shared<Declarations>,
    scopes: Shared<BTreeMap<ScopeId, Scope>>,
    load: Arc<
        dyn Fn(&'a Compiler<'l>, Span, FilePath) -> BoxFuture<'a, Option<Arc<File>>> + Send + Sync,
    >,
    expanded: Shared<HashSet<Span>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    pub(crate) id: ScopeId,
    pub(crate) parent: Option<ScopeId>,
    pub(crate) values: HashMap<InternedString, ScopeValueKind>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ScopeValueKind {
    Operator(Operator),
    Syntax(Syntax),
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Operator {
    pub precedence: OperatorPrecedence,
    pub syntax: Syntax,
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Syntax {
    Defined(TemplateId),
    Builtin(BuiltinSyntax),
}

impl<'a, 'l> Expander<'a, 'l> {
    pub(crate) fn new_scope(&self) -> ScopeId {
        let mut scope = self.create_scope();
        BuiltinSyntax::load_into(&mut scope);

        let id = scope.id;
        self.scopes.lock().insert(id, scope);

        id
    }

    pub(crate) fn child_scope(&self, parent: ScopeId) -> ScopeId {
        let mut scope = self.create_scope();
        scope.parent = Some(parent);

        let id = scope.id;
        self.scopes.lock().insert(id, scope);

        id
    }

    fn create_scope(&self) -> Scope {
        Scope {
            id: self.compiler.new_scope_id_in(self.file),
            parent: None,
            values: Default::default(),
        }
    }

    pub(crate) fn get_name(&self, name: InternedString, scope: ScopeId) -> Option<ScopeValueKind> {
        let mut parent = Some(scope);
        let mut result = None;

        while let Some(scope) = parent {
            let scopes = self.scopes.lock();
            let scope = scopes.get(&scope).unwrap();

            if let Some(value) = scope.values.get(&name).cloned() {
                result = Some(value);
                break;
            }

            parent = scope.parent;
        }

        result
    }

    pub(crate) fn set_name(&self, name: InternedString, value: ScopeValueKind, scope: ScopeId) {
        let mut scopes = self.scopes.lock();
        let scope = scopes.get_mut(&scope).unwrap();
        scope.values.insert(name, value);
    }
}

impl<'a, 'l> Expander<'a, 'l> {
    pub(crate) fn add_dependency(&self, file: Arc<File>, scope: ScopeId) {
        self.dependencies.lock().push((file.span.path, None));

        for decl in file.declarations.operators.values() {
            self.set_name(decl.name, ScopeValueKind::Operator(decl.value), scope);
        }

        for (&id, decl) in &file.declarations.syntaxes {
            self.set_name(
                decl.name,
                ScopeValueKind::Syntax(Syntax::Defined(id)),
                scope,
            );
        }
    }
}

struct Info<'a> {
    scope: ScopeId,
    context: Context<'a>,
}

enum Context<'a> {
    File(&'a mut FileAttributes),
    Statement(&'a mut StatementAttributes),
}

impl<'a, 'l> Expander<'a, 'l> {
    pub(crate) async fn expand_file_attributes(&self, attributes: Vec<Expression>, scope: ScopeId) {
        todo!()
    }

    pub(crate) async fn expand_statement(&self, statement: Statement, scope: ScopeId) -> Statement {
        todo!()
    }

    async fn expand_list(&self, span: Span, exprs: Vec<Expression>, info: &Info<'_>) -> Expression {
        match self.expand_operators(span, exprs, info.scope) {
            ExpandOperatorsResult::Error(trace) => Expression {
                span,
                kind: ExpressionKind::Error(trace),
            },
            ExpandOperatorsResult::Empty => Expression {
                span,
                kind: ExpressionKind::List(Vec::new()),
            },
            ExpandOperatorsResult::Single(expr) => expr,
            ExpandOperatorsResult::List(exprs) => Expression {
                span,
                kind: ExpressionKind::List(exprs),
            },
            ExpandOperatorsResult::Operator(operator_span, operator_name, syntax, left, right) => {
                let name = Expression {
                    span,
                    kind: ExpressionKind::Name(None, operator_name),
                };

                let input = Expression {
                    span,
                    kind: ExpressionKind::List(vec![left, name, right]),
                };

                self.expand_syntax(operator_span, syntax, input).await
            }
            ExpandOperatorsResult::Syntax(syntax_span, syntax_name, syntax, inputs) => {
                let name = Expression {
                    span,
                    kind: ExpressionKind::Name(None, syntax_name),
                };

                let input = if inputs.is_empty() {
                    name
                } else {
                    Expression {
                        span,
                        kind: ExpressionKind::List(std::iter::once(name).chain(inputs).collect()),
                    }
                };

                self.expand_syntax(syntax_span, syntax, input).await
            }
        }
    }

    async fn expand_syntax(&self, span: Span, syntax: Syntax, input: Expression) -> Expression {
        self.expanded.lock().insert(span);

        match syntax {
            Syntax::Defined(id) => {
                let declarations = self.declarations.lock();
                let decl = &declarations.syntaxes.get(&id).unwrap();
                for rule in &decl.value.rules {
                    if let Some(vars) = input.unify(&rule.pattern) {
                        let mut body = rule.body.clone();
                        body.expand(&vars, self);
                        return body;
                    }
                }

                self.compiler.add_error(
                    "syntax did not match any rules",
                    vec![
                        Note::primary(span, "incorrect syntax"),
                        Note::secondary(decl.span, "syntax defined here"),
                    ],
                );

                Expression {
                    span,
                    kind: ExpressionKind::error(self.compiler),
                }
            }
            Syntax::Builtin(syntax) => {
                let vars = match input.unify(&syntax.pattern()) {
                    Some(vars) => vars,
                    None => {
                        self.compiler.add_error(
                            "syntax did not match any rules",
                            vec![Note::primary(span, "incorrect syntax")],
                        );

                        return Expression {
                            span,
                            kind: ExpressionKind::error(self.compiler),
                        };
                    }
                };

                syntax.expand(span, vars, self).await
            }
        }
    }
}
