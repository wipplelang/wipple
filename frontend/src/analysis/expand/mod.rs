mod operators;
mod syntax;

pub use syntax::{Expression, ExpressionKind, Statement};

use crate::{
    diagnostics::Note,
    helpers::{Backtrace, InternedString, Shared},
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
    pub on_mismatch: VecDeque<(Option<(Span, InternedString)>, InternedString)>,
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum PatternKind {
    Error(Backtrace),
    Wildcard,
    Number(InternedString),
    Text(InternedString),
    Name(Option<ScopeId>, InternedString),
    Destructure(Vec<(InternedString, Pattern)>),
    Variant((Span, Option<ScopeId>, InternedString), Vec<Pattern>),
    Annotate(Box<Pattern>, Box<Expression>),
    Or(Box<Pattern>, Box<Pattern>),
    Where(Box<Pattern>, Box<Expression>),
    Tuple(Vec<Pattern>),
}

impl PatternKind {
    pub(crate) fn error(compiler: &Compiler) -> Self {
        PatternKind::Error(compiler.backtrace())
    }
}

#[allow(unused)]
impl Pattern {
    pub(crate) fn traverse_mut(&mut self, mut f: impl FnMut(&mut Pattern)) {
        self.traverse_mut_with_inner((), &mut |expr, ()| f(expr));
    }

    pub(crate) fn traverse_mut_with<T: Clone>(
        &mut self,
        context: T,
        mut f: impl FnMut(&mut Pattern, T) -> T,
    ) {
        self.traverse_mut_with_inner(context, &mut f);
    }

    fn traverse_mut_with_inner<T: Clone>(
        &mut self,
        context: T,
        f: &mut impl FnMut(&mut Pattern, T) -> T,
    ) {
        let context = f(self, context);

        match &mut self.kind {
            PatternKind::Error(_)
            | PatternKind::Wildcard
            | PatternKind::Number(_)
            | PatternKind::Text(_)
            | PatternKind::Name(_, _) => {}
            PatternKind::Destructure(fields) => {
                for (_, pattern) in fields {
                    pattern.traverse_mut_with_inner(context.clone(), f);
                }
            }
            PatternKind::Variant(_, patterns) | PatternKind::Tuple(patterns) => {
                for pattern in patterns {
                    pattern.traverse_mut_with_inner(context.clone(), f);
                }
            }
            PatternKind::Annotate(pattern, _) | PatternKind::Where(pattern, _) => {
                pattern.traverse_mut_with_inner(context, f);
            }
            PatternKind::Or(lhs, rhs) => {
                lhs.traverse_mut_with_inner(context.clone(), f);
                rhs.traverse_mut_with_inner(context, f);
            }
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct TypeParameter {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Bound {
    pub span: Span,
    pub trait_span: Span,
    pub trait_scope: ScopeId,
    pub trait_name: InternedString,
    pub parameters: Vec<Expression>,
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
            // Expand any new syntaxes and operators found in the program

            statements = stream::iter(statements)
                .then(|statement| expander.expand_statement(statement, scope))
                .collect::<Vec<_>>()
                .await;

            let mut block = Expression {
                span: file.span,
                scope: Some(scope),
                kind: ExpressionKind::Block(Some(scope), statements),
            };

            expander.update_scopes(&mut block, scope);

            statements = match block.kind {
                ExpressionKind::Block(_, statements) => statements,
                _ => unreachable!(),
            };

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
                        scope: Some(scope),
                        kind: ExpressionKind::Name(None, syntax_name),
                    };

                    let input = Expression {
                        span: attribute.span,
                        scope: Some(scope),
                        kind: ExpressionKind::List(std::iter::once(name).chain(inputs).collect()),
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
                        scope: Some(scope),
                        kind: ExpressionKind::Name(None, syntax_name),
                    };

                    let input = Expression {
                        span: attribute.span,
                        scope: Some(scope),
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
                scope: statement.expr.scope,
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
            scope: expr.scope,
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
                ExpressionKind::AssignToPattern(pattern, expr) => ExpressionKind::AssignToPattern(
                    pattern,
                    Box::new(self.expand_expr(*expr, inherited_scope).await),
                ),
                ExpressionKind::Assign(lhs, rhs) => ExpressionKind::Assign(
                    Box::new(self.expand_expr(*lhs, inherited_scope).await),
                    Box::new(self.expand_expr(*rhs, inherited_scope).await),
                ),
                ExpressionKind::Function(pattern, (lhs, rhs)) => {
                    let pattern = OptionFuture::from(pattern.map(
                        |(function_scope, pattern, expr)| async move {
                            let function_scope = function_scope
                                .expect("should have been created by syntax definition");

                            (
                                Some(function_scope),
                                pattern,
                                Box::new(self.expand_expr(*expr, function_scope).await),
                            )
                        },
                    ))
                    .await;

                    ExpressionKind::Function(
                        pattern,
                        (
                            Box::new(self.expand_expr(*lhs, inherited_scope).await),
                            Box::new(self.expand_expr(*rhs, inherited_scope).await),
                        ),
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
                ExpressionKind::Type(fields) => ExpressionKind::Type(
                    OptionFuture::from(fields.map(|fields| async {
                        Box::new(self.expand_expr(*fields, inherited_scope).await)
                    }))
                    .await,
                ),
                ExpressionKind::Trait(value) => ExpressionKind::Trait(
                    OptionFuture::from(value.map(|value| async {
                        Box::new(self.expand_expr(*value, inherited_scope).await)
                    }))
                    .await,
                ),
                ExpressionKind::TypeFunction(scope, (params, bounds), rhs) => {
                    let scope = scope.expect("should have been created by syntax definition");

                    ExpressionKind::TypeFunction(
                        Some(scope),
                        (params, bounds),
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
                    scope: Some(scope),
                    kind: ExpressionKind::Name(None, operator_name),
                };

                let input = Expression {
                    span,
                    scope: Some(scope),
                    kind: ExpressionKind::List(vec![left, name, right]),
                };

                self.expand_operator(operator_span, syntax, input, context, scope)
                    .await
                    .kind
            }
            ExpandOperatorsResult::Syntax(syntax_span, syntax_name, syntax, inputs) => {
                let name = Expression {
                    span: syntax_span,
                    scope: Some(scope),
                    kind: ExpressionKind::Name(None, syntax_name),
                };

                let input = Expression {
                    span,
                    scope: Some(scope),
                    kind: ExpressionKind::List(std::iter::once(name).chain(inputs).collect()),
                };

                self.expand_syntax(syntax_span, syntax, input, context, scope)
                    .await
                    .kind
            }
        }
    }

    async fn expand_completely(&self, mut expr: Expression, scope: ScopeId) -> Expression {
        let mut expanded = self.expanded.lock().clone();

        let mut expansion_count = 0usize;
        loop {
            // Expand any new syntaxes and operators found in the program

            expr = self.expand_expr(expr, scope).await;
            self.update_scopes(&mut expr, scope);

            // Prevent infinite recursion

            let expanded = mem::take(&mut expanded);

            if expanded.is_empty() {
                break;
            }

            if expansion_count
                > self
                    .attributes
                    .lock()
                    .recursion_limit
                    .unwrap_or(Compiler::DEFAULT_RECURSION_LIMIT)
            {
                self.compiler.add_error(
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

        expr
    }

    async fn expand_pattern(
        &self,
        mut expr: Expression,
        scope: ScopeId,
    ) -> Result<Pattern, Expression> {
        expr = self.expand_completely(expr, scope).await;
        self.compiler.parse_pattern_from_expander_expr(expr, false)
    }

    async fn expand_type_function(
        &self,
        mut lhs: Expression,
        scope: ScopeId,
    ) -> (Vec<TypeParameter>, Vec<Bound>) {
        lhs = self.expand_completely(lhs, scope).await;

        self.compiler
            .parse_type_function_from_expander_expr(lhs)
            .unwrap_or_default()
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
                    scope: Some(scope),
                    kind: ExpressionKind::error(self.compiler),
                }
            }
            Syntax::Builtin(syntax) => {
                let pattern = Expression {
                    span: Span::builtin(),
                    scope: Some(scope),
                    kind: ExpressionKind::List(syntax.pattern()),
                };

                // TODO: Allow builtin syntax definitions to have multiple rules
                let vars = match input.unify(&pattern) {
                    Some(vars) => vars,
                    None => {
                        self.report_syntax_error(span, None);

                        return Expression {
                            span,
                            scope: Some(scope),
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
    fn update_scopes_for_pattern(
        &self,
        pattern: &mut Pattern,
        expr: &mut Expression,
        scope: ScopeId,
    ) {
        let mut declared_variables = HashSet::new();
        pattern.traverse_mut(|pattern| {
            if let PatternKind::Name(_, name) = pattern.kind {
                declared_variables.insert(name);
            }
        });

        expr.traverse_mut(|expr| {
            if let ExpressionKind::Name(name_scope, name) = &mut expr.kind {
                if declared_variables.contains(name) {
                    *name_scope = Some(scope);
                }
            }
        });
    }

    fn update_scopes_for_type_function(
        &self,
        params: &[TypeParameter],
        rhs: &mut Expression,
        scope: ScopeId,
    ) {
        let declared_tys = params
            .iter()
            .map(|param| param.name)
            .collect::<HashSet<_>>();

        rhs.traverse_mut(|expr| {
            if let ExpressionKind::Name(name_scope, name) = &mut expr.kind {
                if declared_tys.contains(name) {
                    *name_scope = Some(scope);
                }
            }
        });
    }
}

impl<'l> Compiler<'l> {
    // HACK: Allows parsing patterns in both the expander and the AST builder
    pub(crate) fn parse_pattern_from_expander_expr(
        &self,
        expr: Expression,
        show_errors: bool,
    ) -> Result<Pattern, Expression> {
        let original_expr = expr.clone();

        let kind = (|| {
            Some(match expr.kind {
                ExpressionKind::Error(trace) => PatternKind::Error(trace),
                ExpressionKind::Underscore => PatternKind::Wildcard,
                ExpressionKind::Name(scope, name) => PatternKind::Name(scope, name),
                ExpressionKind::Block(_, statements) => {
                    PatternKind::Destructure(
                        statements
                            .into_iter()
                            .filter_map(
                                |statement| -> Option<
                                    Box<dyn Iterator<Item = (InternedString, Pattern)>>,
                                > {
                                    // TODO: Handle attributes

                                    match statement.expr.kind {
                                        ExpressionKind::Error(_) => None,
                                        ExpressionKind::List(exprs) => Some(Box::new(
                                            exprs.into_iter().filter_map(|expr| {
                                                let (name, pattern) = match expr.kind {
                                                    ExpressionKind::Error(_) => return None,
                                                    ExpressionKind::Name(scope, name) => (
                                                        name,
                                                        Pattern {
                                                            span: expr.span,
                                                            kind: PatternKind::Name(scope, name),
                                                        },
                                                    ),
                                                    _ => {
                                                        if show_errors {
                                                            self.add_error(
                                                                "invalid pattern in destructuring pattern", vec![Note::primary(
                                                                    expr.span,
                                                                    "expected name here",
                                                                )],
                                                            );
                                                        }

                                                        return None;
                                                    }
                                                };

                                                Some((name, pattern))
                                            }),
                                        )),
                                        ExpressionKind::Name(scope, name) => {
                                            Some(Box::new(std::iter::once((
                                                name,
                                                Pattern {
                                                    span: statement.expr.span,
                                                    kind: PatternKind::Name(scope, name),
                                                },
                                            ))))
                                        }
                                        ExpressionKind::AssignToPattern(Pattern { kind: PatternKind::Name(_, name), .. }, expr) => {
                                            let pattern = self.parse_pattern_from_expander_expr(*expr, show_errors).ok()?;
                                            Some(Box::new(std::iter::once((name, pattern))))
                                        }
                                        ExpressionKind::Assign(left, _) => {
                                            if show_errors {
                                                self.add_error(
                                                    "invalid pattern in destructuring pattern", vec![Note::primary(
                                                        left.span,
                                                        "expected name here",
                                                    )],
                                                );
                                            }

                                            None
                                        }
                                        _ => {
                                            if show_errors {
                                                self.add_error(
                                                    "invalid pattern in destructuring pattern", vec![Note::primary(
                                                        expr.span,
                                                        "try removing this",
                                                    )],
                                                );
                                            }

                                            None
                                        }
                                    }
                                },
                            )
                            .flatten()
                            .collect(),
                    )
                }
                ExpressionKind::List(exprs) => {
                    let mut exprs = exprs.into_iter();

                    let name = match exprs.next() {
                        Some(expr) => expr,
                        None => return Some(PatternKind::Tuple(Vec::new())),
                    };

                    let name_span = name.span;

                    let (name_scope, name) = match name.kind {
                        ExpressionKind::Name(scope, name) => (scope, name),
                        _ => {
                            if show_errors {
                                self.add_error(
                                    "expected variant name",
                                    vec![Note::primary(
                                        expr.span,
                                        "only variants may be used in this kind of pattern",
                                    )],
                                );
                            }

                            return None;
                        }
                    };

                    let rest = exprs
                        .map(|expr| {
                            self.parse_pattern_from_expander_expr(expr, show_errors)
                                .ok()
                        })
                        .collect::<Option<_>>()?;

                    PatternKind::Variant((name_span, name_scope, name), rest)
                }
                ExpressionKind::Number(number) => PatternKind::Number(number),
                ExpressionKind::Text(text) => PatternKind::Text(text),
                ExpressionKind::Annotate(expr, ty) => {
                    let inner = self
                        .parse_pattern_from_expander_expr(*expr, show_errors)
                        .ok()?;

                    PatternKind::Annotate(Box::new(inner), ty)
                }
                ExpressionKind::Or(lhs, rhs) => PatternKind::Or(
                    Box::new(
                        self.parse_pattern_from_expander_expr(*lhs, show_errors)
                            .ok()?,
                    ),
                    Box::new(
                        self.parse_pattern_from_expander_expr(*rhs, show_errors)
                            .ok()?,
                    ),
                ),
                ExpressionKind::Where(pattern, condition) => PatternKind::Where(
                    Box::new(
                        self.parse_pattern_from_expander_expr(*pattern, show_errors)
                            .ok()?,
                    ),
                    condition,
                ),
                ExpressionKind::Tuple(exprs) => PatternKind::Tuple(
                    exprs
                        .into_iter()
                        .map(|expr| {
                            self.parse_pattern_from_expander_expr(expr, show_errors)
                                .ok()
                        })
                        .collect::<Option<_>>()?,
                ),
                _ => {
                    if show_errors {
                        self.add_error(
                            "expected pattern",
                            vec![Note::primary(
                                expr.span,
                                "values may not appear on the left-hand side of a variable assignment",
                            )],
                        );
                    }

                    return None;
                }
            })
        })();

        kind.map(|kind| Pattern {
            span: expr.span,
            kind,
        })
        .ok_or(original_expr)
    }

    pub(crate) fn parse_type_function_from_expander_expr(
        &self,
        lhs: Expression,
    ) -> Option<(Vec<TypeParameter>, Vec<Bound>)> {
        macro_rules! build_parameter_list {
            ($tys:expr) => {
                $tys.into_iter()
                    .map(|expr| match expr.kind {
                        ExpressionKind::Error(_) => None,
                        ExpressionKind::Name(_, name) => Some(TypeParameter {
                            span: expr.span,
                            name,
                        }),
                        _ => {
                            self.add_error(
                                "expected type parameter",
                                vec![Note::primary(expr.span, "try removing this")],
                            );

                            None
                        }
                    })
                    .collect::<Option<Vec<_>>>()
            };
        }

        macro_rules! build_bound {
            ($span:expr, $list:expr) => {
                (|| {
                    let mut list = $list.into_iter();

                    let trait_name = list.next().unwrap();
                    let trait_span = trait_name.span;
                    let (trait_scope, trait_name) = match trait_name.kind {
                        ExpressionKind::Error(_) => return None,
                        ExpressionKind::Name(scope, name) => (scope.unwrap(), name),
                        _ => {
                            self.add_error(
                                "expected trait name in `where` clause",
                                vec![Note::primary(trait_name.span, "try adding a name here")],
                            );

                            return None;
                        }
                    };

                    Some(Bound {
                        span: $span,
                        trait_span,
                        trait_scope,
                        trait_name,
                        parameters: list.collect(),
                    })
                })()
            };
        }

        match lhs.kind {
            ExpressionKind::Error(_) => None,
            ExpressionKind::Name(_, name) => Some((
                vec![TypeParameter {
                    span: lhs.span,
                    name,
                }],
                Vec::new(),
            )),
            ExpressionKind::List(tys) => Some((build_parameter_list!(tys)?, Vec::new())),
            ExpressionKind::Where(lhs, bounds) => {
                let tys = match lhs.kind {
                    ExpressionKind::Error(_) => return None,
                    ExpressionKind::Name(_, name) => vec![TypeParameter {
                        span: lhs.span,
                        name,
                    }],
                    ExpressionKind::List(tys) => build_parameter_list!(tys)?,
                    _ => {
                        self.add_error(
                            "expected type parameters on left-hand side of `where` clause",
                            vec![Note::primary(
                                lhs.span,
                                "try providing a list of names here",
                            )],
                        );

                        return None;
                    }
                };

                let bounds_span = bounds.span;

                let bounds = (|| {
                    match bounds.kind {
                        ExpressionKind::List(bounds) => {
                            if bounds
                                .iter()
                                .any(|bound| matches!(bound.kind, ExpressionKind::List(_)))
                            {
                                // The bounds clause matches the pattern `(T A) (T B) ...`
                                bounds
                                    .into_iter()
                                    .map(|bound| match bound.kind {
                                        ExpressionKind::Name(trait_scope, trait_name) => Some(Bound {
                                            span: bound.span,
                                            trait_span: bound.span,
                                            trait_scope: trait_scope.unwrap(),
                                            trait_name,
                                            parameters: Vec::new(),
                                        }),
                                        ExpressionKind::List(list) => build_bound!(bound.span, list),
                                        _ => {
                                            self.add_error(
                                                "expected bound", vec![Note::primary(
                                                    bounds_span,
                                                    "`where` bound must be in the format `(T A B ...)`",
                                                )],
                                            );

                                            None
                                        }
                                    })
                                    .collect::<Option<_>>()
                            } else {
                                // The bounds clause matches the pattern `T A B ...`
                                Some(vec![build_bound!(bounds_span, bounds)?])
                            }
                        }
                        ExpressionKind::Name(trait_scope, trait_name) => {
                            // The bounds clause matches the pattern `T`
                            Some(vec![Bound {
                                span: bounds_span,
                                trait_span: bounds_span,
                                trait_scope: trait_scope.unwrap(),
                                trait_name,
                                parameters: Vec::new(),
                            }])
                        }
                        _ => {
                            self.add_error(
                                "expected bounds",
                                vec![Note::primary(
                                    bounds_span,
                                    "`where` bounds must be in the format `(T A) (T B) ...`",
                                )],
                            );

                            None
                        }
                    }
                })()?;

                Some((tys, bounds))
            }
            _ => {
                self.add_error(
                    "expected type parameters",
                    vec![Note::primary(
                        lhs.span,
                        "try providing a list of names and optionally a `where` clause",
                    )],
                );

                None
            }
        }
    }
}

enum WithScopesResult<'a> {
    NameLike(InternedString, &'a mut Option<ScopeId>, ScopeId),
    BlockLike(&'a mut Option<ScopeId>, ScopeId),
    Inherited(ScopeId),
}

impl Expression {
    fn owned_scope(&mut self) -> Option<&mut Option<ScopeId>> {
        // NOTE: Make sure to update this any time a new expression with its own
        // scope is added
        match &mut self.kind {
            ExpressionKind::Block(scope, _)
            | ExpressionKind::Function(Some((scope, _, _)), (_, _))
            | ExpressionKind::TypeFunction(scope, _, _) => Some(scope),
            _ => None,
        }
    }
}

impl<'a, 'l> Expander<'a, 'l> {
    fn with_scopes(
        &self,
        expr: &mut Expression,
        scope: ScopeId,
        mut f: impl FnMut(Span, WithScopesResult) -> ScopeId,
    ) {
        expr.traverse_mut_with(scope, |expr, inherited_scope| {
            let span = expr.span;

            if let ExpressionKind::Name(scope, name) = &mut expr.kind {
                f(
                    span,
                    WithScopesResult::NameLike(*name, scope, inherited_scope),
                )
            } else if let Some(scope) = expr.owned_scope() {
                f(span, WithScopesResult::BlockLike(scope, inherited_scope))
            } else {
                f(span, WithScopesResult::Inherited(inherited_scope))
            }
        });
    }

    fn update_scopes(&self, expr: &mut Expression, scope: ScopeId) {
        self.with_scopes(expr, scope, |span, result| match result {
            WithScopesResult::NameLike(_, scope, inherited_scope) => {
                *scope.get_or_insert(inherited_scope)
            }
            WithScopesResult::BlockLike(scope, inherited_scope) => {
                *scope.get_or_insert_with(|| self.child_scope(span, inherited_scope))
            }
            WithScopesResult::Inherited(inherited_scope) => inherited_scope,
        });
    }
}
