mod operators;
mod syntax;

pub use syntax::{Expression, ExpressionKind, Statement};

use crate::{
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
    Compiler, FilePath, ScopeId, TemplateId,
};
use async_recursion::async_recursion;
use futures::{
    future::{BoxFuture, OptionFuture},
    stream, StreamExt,
};
use operators::*;
use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    mem,
    sync::Arc,
};
use syntax::*;

pub(crate) const GLOBAL_ROOT_SCOPE: ScopeId = ScopeId {
    file: None,
    counter: 0,
};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct File {
    pub span: Span,
    pub dependencies: Vec<(Arc<File>, Option<HashMap<InternedString, Span>>)>,
    pub attributes: FileAttributes,
    pub declarations: Declarations,
    pub root_scope: ScopeId,
    pub scopes: BTreeMap<ScopeId, (Option<Span>, Option<ScopeId>)>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct FileAttributes {
    pub no_std: bool,
    pub recursion_limit: Option<usize>,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct StatementAttributes {
    pub language_item: Option<LanguageItem>,
    pub help: VecDeque<InternedString>,
    pub on_unimplemented: Option<InternedString>,
    pub on_mismatch: VecDeque<(
        Option<(Span, Option<ScopeId>, InternedString)>,
        InternedString,
    )>,
    pub specialize: bool,
    pub allow_overlapping_instances: bool,
    pub operator_precedence: Option<OperatorPrecedence>,
    pub keyword: bool,
}

#[derive(Debug, Clone, Copy, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum LanguageItem {
    Boolean,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct SyntaxDeclaration<T> {
    pub name: InternedString,
    pub span: Span,
    pub uses: HashSet<Span>,
    pub attributes: SyntaxDeclarationAttributes,
    pub value: T,
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
        let mut expander = Expander {
            compiler: self,
            file: file.span.path,
            dependencies: Default::default(),
            attributes: Default::default(),
            declarations: Default::default(),
            scopes: Default::default(),
            file_scope: None,
            load: Arc::new(load),
            expanded: Default::default(),
        };

        // Load the global builtins

        let mut scope = Scope {
            id: GLOBAL_ROOT_SCOPE,
            span: None,
            parent: None,
            values: Default::default(),
        };

        BuiltinSyntax::load_into(&mut scope);

        expander.scopes.lock().insert(scope.id, scope);

        // Initialize the file's own scope

        let scope = expander.child_scope(file.span, GLOBAL_ROOT_SCOPE);
        expander.file_scope = Some(scope);

        // Expand file attributes

        let file_attributes = expander
            .expand_file_attributes(file.attributes.into_iter().map(Attribute::from), scope)
            .await;

        if !file_attributes.no_std {
            let std_path = expander.compiler.loader.std_path();
            if let Some(std_path) = std_path {
                if let Some(file) = (expander.load)(expander.compiler, file.span, std_path).await {
                    expander.add_dependency(file);
                }
            } else {
                expander.compiler.add_error(
                    "standard library is missing, but this file requires it",
                    vec![Note::primary(
                        file.span.with_end(file.span.start),
                        "try adding `[[no-std]]` to this file to prevent automatically loading the standard library",
                    )],
                );
            }
        }

        // Expand the file's contents

        let mut statements = file
            .statements
            .into_iter()
            .flat_map(|statement| statement.try_into().ok())
            .collect::<Vec<_>>();

        let mut expansion_count = 0usize;
        loop {
            // Expand any new templates and operators found in the program

            statements = stream::iter(statements)
                .then(|statement| expander.expand_statement(statement, scope))
                .collect::<Vec<_>>()
                .await;

            // Prevent infinite recursion

            let expanded = mem::take(&mut *expander.expanded.lock());

            if expanded.is_empty() {
                break;
            }

            if expansion_count
                > expander
                    .attributes
                    .lock()
                    .recursion_limit
                    .unwrap_or(Compiler::DEFAULT_RECURSION_LIMIT)
            {
                self.add_error(
                    "recursion limit reached",
                    expanded
                        .into_iter()
                        .map(|span| Note::primary(span, "while expanding this"))
                        .collect(),
                );

                break;
            }

            expansion_count += 1;
        }

        let mut block = Expression {
            span: file.span,
            kind: ExpressionKind::Block(Some(scope), statements),
        };

        expander.update_scopes(&mut block, scope);

        statements = match block.kind {
            ExpressionKind::Block(_, statements) => statements,
            _ => unreachable!(),
        };

        File {
            span: file.span,
            dependencies: expander.dependencies.into_unique().into_values().collect(),
            attributes: expander.attributes.into_unique(),
            declarations: expander.declarations.into_unique(),
            root_scope: scope,
            scopes: expander
                .scopes
                .into_unique()
                .into_iter()
                .filter(|(id, _)| *id != GLOBAL_ROOT_SCOPE)
                .map(|(id, scope)| (id, (scope.span, scope.parent)))
                .collect(),
            statements,
        }
    }
}

pub(crate) struct Expander<'a, 'l> {
    file: FilePath,
    compiler: &'a Compiler<'l>,
    dependencies: Shared<HashMap<FilePath, (Arc<File>, Option<HashMap<InternedString, Span>>)>>,
    attributes: Shared<FileAttributes>,
    declarations: Shared<Declarations>,
    scopes: Shared<BTreeMap<ScopeId, Scope>>,
    file_scope: Option<ScopeId>,
    load: Arc<
        dyn Fn(&'a Compiler<'l>, Span, FilePath) -> BoxFuture<'a, Option<Arc<File>>> + Send + Sync,
    >,
    expanded: Shared<HashSet<Span>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    pub(crate) id: ScopeId,
    pub(crate) span: Option<Span>,
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
    pub(crate) fn child_scope(&self, span: Span, parent: ScopeId) -> ScopeId {
        let mut scope = self.create_scope(span);
        scope.parent = Some(parent);

        let id = scope.id;
        self.scopes.lock().insert(id, scope);

        id
    }

    fn create_scope(&self, span: Span) -> Scope {
        Scope {
            id: self.compiler.new_scope_id_in(self.file),
            span: Some(span),
            parent: None,
            values: Default::default(),
        }
    }

    pub(crate) fn get_name(&self, name: InternedString, scope: ScopeId) -> Option<ScopeValueKind> {
        let mut parent = Some(scope);
        while let Some(scope) = parent {
            let scopes = self.scopes.lock();
            let scope = scopes
                .get(&scope)
                .unwrap_or_else(|| panic!("unregistered scope {:?}", scope));

            if let Some(value) = scope.values.get(&name).cloned() {
                return Some(value);
            }

            parent = scope.parent;
        }

        None
    }

    pub(crate) fn set_name(&self, name: InternedString, value: ScopeValueKind, scope: ScopeId) {
        let mut scopes = self.scopes.lock();
        let scope = scopes.get_mut(&scope).unwrap();
        scope.values.insert(name, value);
    }
}

impl<'a, 'l> Expander<'a, 'l> {
    pub(crate) fn add_dependency(&self, file: Arc<File>) {
        self.declarations.lock().merge(file.declarations.clone());

        self.scopes
            .lock()
            .extend(file.scopes.iter().map(|(&id, &(span, parent))| {
                (
                    id,
                    Scope {
                        id,
                        span,
                        parent,
                        values: Default::default(), // never used
                    },
                )
            }));

        for (&id, decl) in &file.declarations.syntaxes {
            self.set_name(
                decl.name,
                ScopeValueKind::Syntax(Syntax::Defined(id)),
                self.file_scope.unwrap(),
            );
        }

        for decl in file.declarations.operators.values() {
            self.set_name(
                decl.name,
                ScopeValueKind::Operator(decl.value),
                self.file_scope.unwrap(),
            );
        }

        self.dependencies
            .lock()
            .insert(file.span.path, (file, None));
    }
}

enum Context<'a> {
    FileAttributes(&'a mut FileAttributes),
    StatementAttributes(&'a mut StatementAttributes),
    Statement(&'a StatementAttributes),
}

impl<'a, 'l> Expander<'a, 'l> {
    async fn expand_file_attributes(
        &self,
        attributes: impl IntoIterator<Item = Attribute>,
        scope: ScopeId,
    ) -> FileAttributes {
        let mut file_attributes = FileAttributes::default();

        for mut attribute in attributes {
            for expr in &mut attribute.exprs {
                self.update_scopes(expr, scope);
            }

            match self.expand_operators(attribute.span, attribute.exprs, scope) {
                ExpandOperatorsResult::Error(_) => {}
                ExpandOperatorsResult::Empty
                | ExpandOperatorsResult::Single(_)
                | ExpandOperatorsResult::List(_) => {
                    self.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(attribute.span, "expected attribute")],
                    );
                }
                ExpandOperatorsResult::Operator(operator_span, _, _, _, _) => {
                    self.compiler.add_error(
                        "operators are not allowed in attributes",
                        vec![Note::primary(operator_span, "try removing this")],
                    );
                }
                ExpandOperatorsResult::Syntax(syntax_span, syntax_name, syntax, inputs) => {
                    let name = Expression {
                        span: attribute.span,
                        kind: ExpressionKind::Name(None, syntax_name),
                    };

                    let input = if inputs.is_empty() {
                        name
                    } else {
                        Expression {
                            span: attribute.span,
                            kind: ExpressionKind::List(
                                std::iter::once(name).chain(inputs).collect(),
                            ),
                        }
                    };

                    self.expand_syntax(
                        syntax_span,
                        syntax,
                        input,
                        Some(Context::FileAttributes(&mut file_attributes)),
                        scope,
                    )
                    .await;
                }
            }
        }

        file_attributes
    }

    async fn expand_statement(&self, mut statement: Statement, scope: ScopeId) -> Statement {
        for attribute in mem::take(&mut statement.unexpanded_attributes) {
            match self.expand_operators(attribute.span, attribute.exprs, scope) {
                ExpandOperatorsResult::Error(_) => {}
                ExpandOperatorsResult::Empty
                | ExpandOperatorsResult::Single(_)
                | ExpandOperatorsResult::List(_) => {
                    self.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(attribute.span, "expected attribute")],
                    );
                }
                ExpandOperatorsResult::Operator(operator_span, _, _, _, _) => {
                    self.compiler.add_error(
                        "operators are not allowed in attributes",
                        vec![Note::primary(operator_span, "try removing this")],
                    );
                }
                ExpandOperatorsResult::Syntax(syntax_span, syntax_name, syntax, inputs) => {
                    let name = Expression {
                        span: attribute.span,
                        kind: ExpressionKind::Name(None, syntax_name),
                    };

                    let input = Expression {
                        span: attribute.span,
                        kind: ExpressionKind::List(
                            std::iter::once(name)
                                .chain(inputs)
                                .chain(std::iter::once(statement.expr))
                                .collect(),
                        ),
                    };

                    statement.expr = self
                        .expand_syntax(
                            syntax_span,
                            syntax,
                            input,
                            Some(Context::StatementAttributes(&mut statement.attributes)),
                            scope,
                        )
                        .await;
                }
            }
        }

        statement.expr = match statement.expr.kind {
            ExpressionKind::List(exprs) => Expression {
                span: statement.expr.span,
                kind: self
                    .expand_list(
                        statement.expr.span,
                        exprs,
                        Some(Context::Statement(&statement.attributes)),
                        scope,
                    )
                    .await,
            },
            _ => self.expand_expr(statement.expr, scope).await,
        };

        statement
    }

    #[async_recursion]
    async fn expand_expr(&self, expr: Expression, inherited_scope: ScopeId) -> Expression {
        Expression {
            span: expr.span,
            kind: match expr.kind {
                ExpressionKind::Error(_)
                | ExpressionKind::EmptySideEffect
                | ExpressionKind::Underscore
                | ExpressionKind::Text(_)
                | ExpressionKind::Number(_) => expr.kind,
                ExpressionKind::Name(scope, name) => {
                    let scope = scope.unwrap_or(inherited_scope);
                    ExpressionKind::Name(Some(scope), name)
                }
                ExpressionKind::Variable(_) | ExpressionKind::RepeatedVariable(_) => {
                    self.compiler.add_error(
                        "unexpected syntax variable",
                        vec![Note::primary(
                            expr.span,
                            "this is not allowed outside `syntax` definitions",
                        )],
                    );

                    ExpressionKind::error(self.compiler)
                }
                ExpressionKind::List(exprs) => {
                    self.expand_list(expr.span, exprs, None, inherited_scope)
                        .await
                }
                ExpressionKind::Block(scope, statements) => {
                    let scope =
                        scope.unwrap_or_else(|| self.child_scope(expr.span, inherited_scope));

                    ExpressionKind::Block(
                        Some(scope),
                        stream::iter(statements)
                            .then(|statement| self.expand_statement(statement, scope))
                            .collect()
                            .await,
                    )
                }
                ExpressionKind::AssignToName(name, expr) => ExpressionKind::AssignToName(
                    name,
                    Box::new(self.expand_expr(*expr, inherited_scope).await),
                ),
                ExpressionKind::Assign(lhs, rhs) => ExpressionKind::Assign(
                    Box::new(self.expand_expr(*lhs, inherited_scope).await),
                    Box::new(self.expand_expr(*rhs, inherited_scope).await),
                ),
                ExpressionKind::Function(scope, lhs, rhs) => {
                    let scope = scope.expect("should have been created by syntax definition");

                    ExpressionKind::Function(
                        Some(scope),
                        Box::new(self.expand_expr(*lhs, scope).await),
                        Box::new(self.expand_expr(*rhs, scope).await),
                    )
                }
                ExpressionKind::Tuple(exprs) => ExpressionKind::Tuple(
                    stream::iter(exprs)
                        .then(|expr| self.expand_expr(expr, inherited_scope))
                        .collect()
                        .await,
                ),
                ExpressionKind::External(namespace, identifier, inputs) => {
                    ExpressionKind::External(
                        Box::new(self.expand_expr(*namespace, inherited_scope).await),
                        Box::new(self.expand_expr(*identifier, inherited_scope).await),
                        stream::iter(inputs)
                            .then(|expr| self.expand_expr(expr, inherited_scope))
                            .collect()
                            .await,
                    )
                }
                ExpressionKind::Annotate(lhs, rhs) => ExpressionKind::Annotate(
                    Box::new(self.expand_expr(*lhs, inherited_scope).await),
                    Box::new(self.expand_expr(*rhs, inherited_scope).await),
                ),
                ExpressionKind::Type(scope, fields) => {
                    let scope = scope.expect("should have been created by syntax definition");

                    ExpressionKind::Type(
                        Some(scope),
                        OptionFuture::from(fields.map(|fields| async {
                            Box::new(self.expand_expr(*fields, scope).await)
                        }))
                        .await,
                    )
                }
                ExpressionKind::Trait(scope, value) => {
                    let scope = scope.expect("should have been created by syntax definition");

                    ExpressionKind::Trait(
                        Some(scope),
                        OptionFuture::from(value.map(|value| async {
                            Box::new(self.expand_expr(*value, scope).await)
                        }))
                        .await,
                    )
                }
                ExpressionKind::TypeFunction(scope, lhs, rhs) => {
                    let scope = scope.expect("should have been created by syntax definition");

                    ExpressionKind::TypeFunction(
                        Some(scope),
                        Box::new(self.expand_expr(*lhs, scope).await),
                        Box::new(self.expand_expr(*rhs, scope).await),
                    )
                }
                ExpressionKind::Where(lhs, rhs) => ExpressionKind::Where(
                    Box::new(self.expand_expr(*lhs, inherited_scope).await),
                    Box::new(self.expand_expr(*rhs, inherited_scope).await),
                ),
                ExpressionKind::Instance(expr) => ExpressionKind::Instance(Box::new(
                    self.expand_expr(*expr, inherited_scope).await,
                )),
                ExpressionKind::Use(expr) => {
                    ExpressionKind::Use(Box::new(self.expand_expr(*expr, inherited_scope).await))
                }
                ExpressionKind::When(input, arms) => ExpressionKind::When(
                    Box::new(self.expand_expr(*input, inherited_scope).await),
                    Box::new(self.expand_expr(*arms, inherited_scope).await),
                ),
                ExpressionKind::Or(lhs, rhs) => ExpressionKind::Or(
                    Box::new(self.expand_expr(*lhs, inherited_scope).await),
                    Box::new(self.expand_expr(*rhs, inherited_scope).await),
                ),
                ExpressionKind::End(expr) => {
                    ExpressionKind::End(Box::new(self.expand_expr(*expr, inherited_scope).await))
                }
            },
        }
    }

    async fn expand_list(
        &self,
        span: Span,
        exprs: Vec<Expression>,
        context: Option<Context<'_>>,
        scope: ScopeId,
    ) -> ExpressionKind {
        match self.expand_operators(span, exprs, scope) {
            ExpandOperatorsResult::Error(trace) => ExpressionKind::Error(trace),
            ExpandOperatorsResult::Empty => ExpressionKind::List(Vec::new()),
            ExpandOperatorsResult::Single(expr) => self.expand_expr(expr, scope).await.kind,
            ExpandOperatorsResult::List(exprs) => ExpressionKind::List(
                stream::iter(exprs)
                    .then(|expr| self.expand_expr(expr, scope))
                    .collect()
                    .await,
            ),
            ExpandOperatorsResult::Operator(operator_span, operator_name, syntax, left, right) => {
                let name = Expression {
                    span: operator_span,
                    kind: ExpressionKind::Name(None, operator_name),
                };

                let input = Expression {
                    span,
                    kind: ExpressionKind::List(vec![left, name, right]),
                };

                self.expand_operator(operator_span, syntax, input, context, scope)
                    .await
                    .kind
            }
            ExpandOperatorsResult::Syntax(syntax_span, syntax_name, syntax, inputs) => {
                let name = Expression {
                    span: syntax_span,
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

                self.expand_syntax(syntax_span, syntax, input, context, scope)
                    .await
                    .kind
            }
        }
    }

    async fn expand_syntax(
        &self,
        span: Span,
        syntax: Syntax,
        input: Expression,
        context: Option<Context<'_>>,
        scope: ScopeId,
    ) -> Expression {
        self.expand_syntax_inner(span, syntax, input, false, context, scope)
            .await
    }

    async fn expand_operator(
        &self,
        span: Span,
        syntax: Syntax,
        input: Expression,
        context: Option<Context<'_>>,
        scope: ScopeId,
    ) -> Expression {
        self.expand_syntax_inner(span, syntax, input, true, context, scope)
            .await
    }

    async fn expand_syntax_inner(
        &self,
        span: Span,
        syntax: Syntax,
        input: Expression,
        operator: bool,
        context: Option<Context<'_>>,
        scope: ScopeId,
    ) -> Expression {
        self.expanded.lock().insert(span);

        match syntax {
            Syntax::Defined(id) => {
                let mut declarations = self.declarations.lock();

                if operator {
                    let decl = declarations.operators.get_mut(&id).unwrap();
                    decl.uses.insert(span);
                }

                let decl = declarations.syntaxes.get_mut(&id).unwrap();
                decl.uses.insert(span);

                for rule in &decl.value.rules {
                    if let Some(vars) = input.unify(&rule.pattern) {
                        let mut body = rule.body.clone();
                        body.expand(&vars, self);
                        return body;
                    }
                }

                self.report_syntax_error(span, Some(decl.span));

                Expression {
                    span,
                    kind: ExpressionKind::error(self.compiler),
                }
            }
            Syntax::Builtin(syntax) => {
                // TODO: Allow builtin syntax definitions to have multiple rules
                let vars = match input.unify(&syntax.pattern()) {
                    Some(vars) => vars,
                    None => {
                        self.report_syntax_error(span, None);

                        return Expression {
                            span,
                            kind: ExpressionKind::error(self.compiler),
                        };
                    }
                };

                syntax.expand(span, vars, context, scope, self).await
            }
        }
    }

    pub(crate) fn report_syntax_error(&self, use_span: Span, syntax_span: Option<Span>) {
        self.compiler.add_error(
            "syntax did not match any rules",
            std::iter::once(Note::primary(use_span, "incorrect syntax"))
                .chain(syntax_span.map(|span| Note::secondary(span, "syntax defined here")))
                .collect::<Vec<_>>(),
        );
    }
}

impl<'a, 'l> Expander<'a, 'l> {
    fn update_scopes(&self, expr: &mut Expression, scope: ScopeId) {
        // NOTE: Make sure to update this any time a new expression with its own
        // scope is added
        expr.traverse_mut_with(scope, |expr, inherited_scope| match &mut expr.kind {
            ExpressionKind::Name(scope, _) => *scope.get_or_insert(inherited_scope),
            ExpressionKind::Block(scope, _)
            | ExpressionKind::Function(scope, _, _)
            | ExpressionKind::Type(scope, _)
            | ExpressionKind::Trait(scope, _)
            | ExpressionKind::TypeFunction(scope, _, _) => {
                *scope.get_or_insert_with(|| self.child_scope(expr.span, inherited_scope))
            }
            _ => inherited_scope,
        });
    }
}
