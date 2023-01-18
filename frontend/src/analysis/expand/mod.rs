#![allow(
    clippy::type_complexity,
    clippy::collapsible_match,
    clippy::too_many_arguments
)]

mod builtins;

use crate::{
    diagnostics::*,
    helpers::{Backtrace, InternedString, Shared},
    parse::{self, Span},
    Compiler, FilePath, TemplateId,
};
use async_recursion::async_recursion;
use futures::{future::BoxFuture, stream, StreamExt};
use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    fmt::Debug,
    sync::Arc,
};
use strum::EnumString;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub attributes: FileAttributes,
    pub declarations: Declarations<Template>,
    pub exported: ScopeValues,
    pub scopes: Vec<(Span, ScopeValues)>,
    pub statements: Vec<Statement>,
    pub dependencies: Vec<(Arc<File>, Option<HashMap<InternedString, Span>>)>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct FileAttributes {
    #[cfg_attr(feature = "arbitrary", arbitrary(with = |_: &mut arbitrary::Unstructured| Ok(true)))]
    pub no_std: bool,

    #[cfg_attr(feature = "arbitrary", arbitrary(with = |_: &mut arbitrary::Unstructured| Ok(None)))]
    pub recursion_limit: Option<usize>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Statement {
    pub attributes: StatementAttributes,
    pub node: Node,
    pub treat_as_expr: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct StatementAttributes {
    pub language_item: Option<LanguageItem>,
    pub help: VecDeque<InternedString>,
    pub on_unimplemented: Option<InternedString>,
    pub on_mismatch: VecDeque<(Option<(Span, InternedString)>, InternedString)>,
    pub specialize: bool,
    pub allow_overlapping_instances: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[strum(serialize_all = "kebab-case")]
pub enum LanguageItem {
    Boolean,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Node {
    pub span: Span,
    pub kind: NodeKind,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum NodeKind {
    Error(Backtrace),
    Placeholder,
    TemplateDeclaration(TemplateId),
    Empty,
    Underscore,
    Name(InternedString),
    Text(InternedString),
    Number(InternedString),
    List(Vec<Node>),
    Block(Vec<Statement>),
    Assign(Box<Node>, Box<Node>),
    Template(Vec<InternedString>, Box<Node>),
    Operator(OperatorPrecedence, Vec<InternedString>, Box<Node>),
    Function(Box<Node>, Box<Node>),
    External(Box<Node>, Box<Node>, Vec<Node>),
    Annotate(Box<Node>, Box<Node>),
    Type(Option<Vec<Statement>>),
    Trait(Option<Box<Node>>),
    TypeFunction(Box<Node>, Box<Node>),
    Where(Box<Node>, Box<Node>),
    Instance(Box<Node>, Vec<Node>),
    UseFile(Option<FilePath>),
    UseExpr(Box<Node>),
    When(Box<Node>, Vec<Statement>),
    Or(Box<Node>, Box<Node>),
    Tuple(Vec<Node>),
    End(Box<Node>),
}

impl NodeKind {
    fn error(compiler: &Compiler) -> Self {
        NodeKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Declarations<T> {
    pub operators: BTreeMap<TemplateId, Operator>,
    pub templates: BTreeMap<TemplateId, TemplateDeclaration<T>>,
}

impl<T> Default for Declarations<T> {
    fn default() -> Self {
        Self {
            operators: Default::default(),
            templates: Default::default(),
        }
    }
}

impl<T> Declarations<T> {
    fn merge(&mut self, other: Self) {
        self.operators.extend(other.operators);

        for (id, decl) in other.templates {
            self.templates.entry(id).or_insert(decl);
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct TemplateDeclaration<T> {
    pub name: InternedString,
    pub span: Span,
    pub uses: HashSet<Span>,
    #[cfg_attr(feature = "serde", serde(skip))]
    pub template: T,
    pub attributes: TemplateAttributes,
}

impl<T> TemplateDeclaration<T> {
    pub fn new(name: impl AsRef<str>, span: Span, template: T) -> Self {
        TemplateDeclaration {
            name: InternedString::new(name),
            span,
            uses: Default::default(),
            template,
            attributes: Default::default(),
        }
    }

    pub(crate) fn keyword(name: impl AsRef<str>, span: Span, template: T) -> Self {
        let mut decl = TemplateDeclaration::new(name, span, template);
        decl.attributes.keyword = true;
        decl
    }
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct TemplateAttributes {
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
            declarations: Default::default(),
            dependencies: Default::default(),
            scopes: Default::default(),
            load: Arc::new(load),
        };

        let scope = Scope::default();
        builtins::load_builtins(&mut expander, file.path, &scope);

        let attributes = expander
            .expand_file_attributes(file.attributes, &scope)
            .await;

        if !attributes.no_std {
            let std_path = expander.compiler.loader.std_path();
            if let Some(std_path) = std_path {
                if let Some(file) = (expander.load)(expander.compiler, file.span, std_path).await {
                    expander.add_dependency(file, &scope);
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

        let (statements, exported) = expander.expand_block(file.statements, &scope).await;

        File {
            path: file.path,
            span: file.span,
            statements,
            attributes,
            declarations: expander.declarations.into_unique(),
            exported,
            scopes: expander.scopes.into_unique(),
            dependencies: expander.dependencies.into_unique().into_values().collect(),
        }
    }
}

#[derive(Clone)]
pub struct Expander<'a, 'l> {
    compiler: &'a Compiler<'l>,
    declarations: Shared<Declarations<Template>>,
    dependencies: Shared<HashMap<FilePath, (Arc<File>, Option<HashMap<InternedString, Span>>)>>,
    scopes: Shared<Vec<(Span, ScopeValues)>>,
    load: Arc<
        dyn Fn(&'a Compiler<'l>, Span, FilePath) -> BoxFuture<'a, Option<Arc<File>>> + Send + Sync,
    >,
}

#[derive(Debug, Clone, Default)]
pub struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    values: Shared<ScopeValues>,
}

pub type ScopeValues = HashMap<InternedString, ScopeValue>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum ScopeValue {
    Operator(Operator),
    Template(TemplateId),
}

impl<'a> Scope<'a> {
    fn child(&'a self) -> Scope<'a> {
        Scope {
            parent: Some(self),
            ..Default::default()
        }
    }

    fn get(&'a self, name: InternedString) -> Option<ScopeValue> {
        let mut parent = Some(self);
        let mut result = None;

        while let Some(scope) = parent {
            if let Some(value) = scope.values.lock().get(&name).cloned() {
                result = Some(value);
                break;
            }

            parent = scope.parent;
        }

        result
    }
}

pub enum Template {
    Syntax(Vec<InternedString>, Node),
    Function(
        Arc<
            dyn for<'a> Fn(
                    &'a Expander,
                    Span,
                    Vec<Node>,
                    Option<&'a mut FileAttributes>,
                    Option<&'a mut StatementAttributes>,
                    &'a Scope,
                ) -> BoxFuture<'a, Node>
                + Send
                + Sync,
        >,
    ),
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Template {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Template::Syntax(Vec::arbitrary(u)?, Node::arbitrary(u)?))
    }
}

impl Clone for Template {
    fn clone(&self) -> Self {
        match self {
            Template::Syntax(inputs, node) => Template::Syntax(inputs.clone(), node.clone()),
            Template::Function(expand) => Template::Function(expand.clone()),
        }
    }
}

impl std::fmt::Debug for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Syntax(inputs, node) => {
                f.debug_tuple("Syntax").field(inputs).field(node).finish()
            }
            Self::Function(_) => f.debug_tuple("Function").finish(),
        }
    }
}

impl Template {
    fn syntax(inputs: Vec<InternedString>, node: Node) -> Self {
        Template::Syntax(inputs, node)
    }

    fn function(
        f: impl for<'a> Fn(
                &'a Expander,
                Span,
                Vec<Node>,
                Option<&'a mut FileAttributes>,
                Option<&'a mut StatementAttributes>,
                &'a Scope,
            ) -> BoxFuture<'a, Node>
            + 'static
            + Send
            + Sync,
    ) -> Self {
        Template::Function(Arc::new(f))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Operator {
    pub precedence: OperatorPrecedence,
    pub template: TemplateId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum OperatorPrecedence {
    Cast,
    Power,
    Multiplication,
    Addition,
    Comparison,
    Conjunction,
    Disjunction,
    Accessor,
    Dot,
    Comma,
    Where,
    Function,
    TypeFunction,
    Annotation,
    Assignment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum OperatorAssociativity {
    Left,
    Right,
    None { error: bool },
}

impl OperatorPrecedence {
    pub fn associativity(&self) -> OperatorAssociativity {
        match self {
            OperatorPrecedence::Cast => OperatorAssociativity::Left,
            OperatorPrecedence::Conjunction => OperatorAssociativity::Left,
            OperatorPrecedence::Disjunction => OperatorAssociativity::Left,
            OperatorPrecedence::Comparison => OperatorAssociativity::Left,
            OperatorPrecedence::Addition => OperatorAssociativity::Left,
            OperatorPrecedence::Multiplication => OperatorAssociativity::Left,
            OperatorPrecedence::Power => OperatorAssociativity::Right,
            OperatorPrecedence::Accessor => OperatorAssociativity::Right,
            OperatorPrecedence::Dot => OperatorAssociativity::Left,
            OperatorPrecedence::Comma => OperatorAssociativity::None { error: false },
            OperatorPrecedence::Where => OperatorAssociativity::None { error: true },
            OperatorPrecedence::Function => OperatorAssociativity::Right,
            OperatorPrecedence::TypeFunction => OperatorAssociativity::None { error: true },
            OperatorPrecedence::Annotation => OperatorAssociativity::Left,
            OperatorPrecedence::Assignment => OperatorAssociativity::None { error: true },
        }
    }
}

impl Expander<'_, '_> {
    fn add_dependency(&self, file: Arc<File>, scope: &Scope) {
        self.dependencies
            .lock()
            .insert(file.path, (file.clone(), None));

        scope.values.lock().extend(file.exported.clone());

        self.declarations.lock().merge(file.declarations.clone());
    }

    async fn expand_file_attributes(
        &self,
        file_attributes: Vec<parse::Expr>,
        scope: &Scope<'_>,
    ) -> FileAttributes {
        macro_rules! assert_empty {
            ($node:expr, $span:expr) => {
                if !matches!($node.kind, NodeKind::Empty | NodeKind::Error(_)) {
                    self.compiler.add_error(
                        "invalid file attribute",
                        vec![Note::primary(
                            $span,
                            "this attribute must expand to an empty expression",
                        )],
                    );
                }
            };
        }

        let mut attributes = FileAttributes::default();
        'attributes: for attribute in file_attributes {
            match attribute.kind {
                parse::ExprKind::Name(name) => {
                    if let Some(ScopeValue::Template(template)) = scope.get(name) {
                        let node = self
                            .expand_template(
                                name,
                                attribute.span,
                                attribute.span,
                                template,
                                Vec::new(),
                                Some(&mut attributes),
                                None,
                                scope,
                            )
                            .await;

                        assert_empty!(node, attribute.span);
                    }
                }
                parse::ExprKind::List(lines) => {
                    let exprs = lines
                        .into_iter()
                        .flat_map(|line| line.exprs)
                        .collect::<Vec<_>>();

                    for expr in &exprs {
                        if let parse::ExprKind::Name(name) = expr.kind {
                            if let Some(ScopeValue::Operator(_)) = scope.get(name) {
                                self.compiler.add_error(
                                    "unexpected operator",
                                    vec![Note::primary(
                                        attribute.span,
                                        "operators aren't allowed within attributes",
                                    )],
                                );

                                continue 'attributes;
                            }
                        }
                    }

                    let mut exprs = exprs.into_iter();

                    let first = exprs.next().unwrap();
                    let name = match first.kind {
                        parse::ExprKind::Name(name) => name,
                        _ => {
                            self.compiler.add_error(
                                "expected name in attribute",
                                vec![Note::primary(
                                    attribute.span,
                                    "expected name of template here",
                                )],
                            );

                            continue 'attributes;
                        }
                    };

                    let template = match scope.get(name) {
                        Some(ScopeValue::Template(template)) => template,
                        Some(ScopeValue::Operator(_)) => {
                            unreachable!("operators trigger an error above");
                        }
                        None => {
                            self.compiler.add_error(
                                format!("cannot find template `{}`", name),
                                vec![Note::primary(first.span, "no such template")],
                            );

                            continue 'attributes;
                        }
                    };

                    let inputs = stream::iter(exprs)
                        .map(|expr| self.expand_expr(expr, scope))
                        .buffered(1)
                        .collect::<Vec<_>>()
                        .await;

                    let node = self
                        .expand_template(
                            name,
                            attribute.span,
                            first.span,
                            template,
                            inputs,
                            Some(&mut attributes),
                            None,
                            scope,
                        )
                        .await;

                    assert_empty!(node, attribute.span);
                }
                _ => {
                    self.compiler.add_error(
                        "invalid attribute",
                        vec![Note::primary(
                            attribute.span,
                            "expected a call to a template here",
                        )],
                    );
                }
            }
        }

        attributes
    }

    #[async_recursion]
    async fn expand_expr(&self, expr: parse::Expr, scope: &Scope<'_>) -> Node {
        match expr.kind {
            parse::ExprKind::Underscore => Node {
                span: expr.span,
                kind: NodeKind::Underscore,
            },
            parse::ExprKind::Name(name) => Node {
                span: expr.span,
                kind: NodeKind::Name(name),
            },
            parse::ExprKind::Text(text) => Node {
                span: expr.span,
                kind: NodeKind::Text(text),
            },
            parse::ExprKind::Number(number) => Node {
                span: expr.span,
                kind: NodeKind::Number(number),
            },
            parse::ExprKind::List(list) => {
                self.expand_list(
                    expr.span,
                    list.into_iter().flat_map(|line| line.exprs).collect(),
                    scope,
                )
                .await
            }
            parse::ExprKind::Block(statements) => {
                let (statements, scope_values) = self.expand_block(statements, scope).await;

                self.scopes.lock().push((expr.span, scope_values));

                Node {
                    span: expr.span,
                    kind: NodeKind::Block(statements),
                }
            }
        }
    }

    async fn expand_block(
        &self,
        statements: Vec<parse::Statement>,
        scope: &Scope<'_>,
    ) -> (Vec<Statement>, ScopeValues) {
        let scope = scope.child();

        let statements = stream::iter(statements)
            .then(|statement| async {
                let mut lines = statement.lines.into_iter();

                let (first_attributes, first_exprs) = match lines.next() {
                    Some(line) => (line.attributes, line.exprs),
                    None => return None,
                };

                let exprs = std::iter::once(first_exprs)
                    .chain(lines.map(|line| {
                        for attribute in line.attributes {
                            self.compiler.add_error(
                                "unexpected attribute",
                                vec![Note::primary(
                                    attribute.span,
                                    "try placing this attribute before the statement",
                                )],
                            );
                        }

                        line.exprs
                    }))
                    .flatten()
                    .collect::<Vec<_>>();

                if exprs.is_empty() {
                    for attribute in first_attributes {
                        self.compiler.add_error(
                            "unexpected attribute",
                            vec![Note::primary(
                                attribute.span,
                                "try adding an expression after this attribute",
                            )],
                        );
                    }

                    return None;
                }

                let span = Span::join(exprs.first().unwrap().span, exprs.last().unwrap().span);

                let treat_as_expr = exprs.len() == 1
                    && matches!(exprs.first().unwrap().kind, parse::ExprKind::List(_));

                let mut node = self.expand_list(span, exprs, &scope).await;

                let mut attributes = StatementAttributes::default();
                'attributes: for attribute in first_attributes.into_iter().rev() {
                    match attribute.kind {
                        parse::ExprKind::Name(name) => {
                            if let Some(ScopeValue::Template(template)) = scope.get(name) {
                                node = self
                                    .expand_template(
                                        name,
                                        attribute.span,
                                        attribute.span,
                                        template,
                                        vec![node],
                                        None,
                                        Some(&mut attributes),
                                        &scope,
                                    )
                                    .await;
                            }
                        }
                        parse::ExprKind::List(lines) => {
                            let exprs = lines
                                .into_iter()
                                .flat_map(|line| line.exprs)
                                .collect::<Vec<_>>();

                            for expr in &exprs {
                                if let parse::ExprKind::Name(name) = expr.kind {
                                    if let Some(ScopeValue::Operator(_)) = scope.get(name) {
                                        self.compiler.add_error(
                                            "unexpected operator",
                                            vec![Note::primary(
                                                attribute.span,
                                                "operators aren't allowed within attributes",
                                            )],
                                        );

                                        continue 'attributes;
                                    }
                                }
                            }

                            let mut exprs = exprs.into_iter();

                            let first = match exprs.next() {
                                Some(expr) => expr,
                                None => continue 'attributes,
                            };

                            let name = match first.kind {
                                parse::ExprKind::Name(name) => name,
                                _ => {
                                    self.compiler.add_error(
                                        "expected name in attribute",
                                        vec![Note::primary(
                                            attribute.span,
                                            "expected name of template here",
                                        )],
                                    );

                                    continue 'attributes;
                                }
                            };

                            let template = match scope.get(name) {
                                Some(ScopeValue::Template(template)) => template,
                                Some(ScopeValue::Operator(_)) => {
                                    unreachable!("operators trigger an error above");
                                }
                                None => {
                                    self.compiler.add_error(
                                        format!("cannot find template `{}`", name),
                                        vec![Note::primary(first.span, "no such template")],
                                    );

                                    continue 'attributes;
                                }
                            };

                            let inputs = stream::iter(exprs)
                                .then(|expr| self.expand_expr(expr, &scope))
                                .chain(stream::once(async { node }))
                                .collect::<Vec<_>>()
                                .await;

                            node = self
                                .expand_template(
                                    name,
                                    attribute.span,
                                    first.span,
                                    template,
                                    inputs,
                                    None,
                                    Some(&mut attributes),
                                    &scope,
                                )
                                .await;
                        }
                        _ => {
                            self.compiler.add_error(
                                "invalid attribute",
                                vec![Note::primary(
                                    attribute.span,
                                    "expected a call to a template here",
                                )],
                            );
                        }
                    }
                }

                (!matches!(
                    node.kind,
                    NodeKind::Placeholder | NodeKind::TemplateDeclaration(_)
                ))
                .then_some(Statement {
                    attributes,
                    node,
                    treat_as_expr,
                })
            })
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .flatten()
            .collect();

        (statements, scope.values.into_unique())
    }

    fn operators_in_list<'a>(
        &self,
        exprs: impl IntoIterator<Item = (usize, &'a parse::Expr)>,
        scope: &Scope<'_>,
    ) -> VecDeque<(usize, InternedString, Span, Operator)> {
        let mut operators = VecDeque::new();
        for (index, expr) in exprs {
            if let parse::ExprKind::Name(name) = &expr.kind {
                if let Some(ScopeValue::Operator(operator)) = scope.get(*name) {
                    operators.push_back((index, *name, expr.span, operator))
                }
            }
        }

        operators
    }

    #[async_recursion]
    async fn expand_list(
        &self,
        list_span: Span,
        mut exprs: Vec<parse::Expr>,
        scope: &Scope<'_>,
    ) -> Node {
        match exprs.len() {
            0 => Node {
                span: list_span,
                kind: NodeKind::Empty,
            },
            1 => {
                let expr = exprs.pop().unwrap();

                if let parse::ExprKind::Name(name) = &expr.kind {
                    match scope.get(*name) {
                        Some(ScopeValue::Template(template)) => {
                            let inputs = stream::iter(exprs)
                                .skip(1)
                                .then(|expr| self.expand_expr(expr, scope))
                                .collect()
                                .await;

                            return self
                                .expand_template(
                                    *name, list_span, expr.span, template, inputs, None, None,
                                    scope,
                                )
                                .await;
                        }
                        Some(ScopeValue::Operator(_)) => {
                            self.compiler.add_error(
                                "expected values on both sides of operator",
                                vec![Note::primary(
                                    expr.span,
                                    "try providing values on either side of this",
                                )],
                            );

                            return Node {
                                span: list_span,
                                kind: NodeKind::error(self.compiler),
                            };
                        }
                        _ => {}
                    }
                }

                self.expand_expr(expr, scope).await
            }
            _ => {
                let operators = self.operators_in_list(exprs.iter().enumerate(), scope);

                if operators.is_empty() {
                    let mut exprs = exprs.into_iter();
                    let first = exprs.next().unwrap();

                    if let parse::ExprKind::Name(name) = first.kind {
                        if let Some(ScopeValue::Template(template)) = scope.get(name) {
                            let inputs = stream::iter(exprs)
                                .then(|expr| self.expand_expr(expr, scope))
                                .collect()
                                .await;

                            return self
                                .expand_template(
                                    name, list_span, first.span, template, inputs, None, None,
                                    scope,
                                )
                                .await;
                        }
                    }

                    Node {
                        span: list_span,
                        kind: NodeKind::List(
                            stream::once(async { first })
                                .chain(stream::iter(exprs))
                                .then(|expr| self.expand_expr(expr, scope))
                                .collect()
                                .await,
                        ),
                    }
                } else {
                    let (mut max_index, mut max_name, mut max_span, mut max_operator) =
                        operators.front().copied().unwrap();

                    for (index, name, span, operator) in operators.iter().skip(1).copied() {
                        macro_rules! replace {
                            () => {{
                                max_index = index;
                                max_name = name;
                                max_span = span;
                                max_operator = operator;
                            }};
                        }

                        match operator.precedence.cmp(&max_operator.precedence) {
                            Ordering::Greater => replace!(),
                            Ordering::Less => continue,
                            Ordering::Equal => match operator.precedence.associativity() {
                                OperatorAssociativity::Left => {
                                    if index > max_index {
                                        replace!();
                                    }
                                }
                                OperatorAssociativity::Right => {
                                    if index < max_index {
                                        replace!()
                                    }
                                }
                                OperatorAssociativity::None { error } => {
                                    if error {
                                        self.compiler.add_error("operator ambiguity", vec![
                                                Note::primary(
                                                    exprs[index].span,
                                                    "only one of this operator may be provided at a time",
                                                ),
                                                Note::secondary(
                                                    exprs[max_index].span,
                                                    "first use of this operator",
                                                ),
                                            ],
                                        );

                                        return Node {
                                            span: list_span,
                                            kind: NodeKind::error(self.compiler),
                                        };
                                    }
                                }
                            },
                        }
                    }

                    if matches!(
                        max_operator.precedence.associativity(),
                        OperatorAssociativity::None { error: false }
                    ) {
                        let mut grouped_exprs = vec![(None, Vec::new())];
                        {
                            let mut operators = operators.clone();

                            for (index, expr) in exprs.into_iter().enumerate() {
                                let operator_index = operators.front().map(|(index, ..)| *index);

                                if Some(index) == operator_index {
                                    operators.pop_front();
                                    grouped_exprs.push((Some(index), Vec::new()));
                                } else {
                                    grouped_exprs.last_mut().unwrap().1.push(expr);
                                }
                            }
                        }

                        // Allow trailing operators
                        if grouped_exprs.last().unwrap().1.is_empty() {
                            grouped_exprs.pop();
                        }

                        let exprs = stream::iter(grouped_exprs)
                            .then({
                                let operators = Arc::new(operators);

                                move |(operator_index, exprs)| {
                                    let operators = operators.clone();

                                    async move {
                                        if exprs.is_empty() {
                                            if let Some(operator_index) = operator_index {
                                                let operator_span = operators
                                                    .iter()
                                                    .find_map(|(index, _, span, _)| {
                                                        (*index == operator_index).then_some(*span)
                                                    })
                                                    .unwrap();

                                                self.compiler.add_error("expected values on right side of operator", vec![Note::primary(
                                                        operator_span,
                                                        "try providing a value to the right of this",
                                                    )],
                                                );
                                            } else {
                                                let operator_span = operators.front().unwrap().2;

                                                self.compiler.add_error("expected values on left side of operator", vec![Note::primary(
                                                        operator_span,
                                                        "try providing a value to the left of this",
                                                    )],
                                                );
                                            }

                                            return Node {
                                                span: list_span,
                                                kind: NodeKind::error(self.compiler),
                                            };
                                        }

                                        let span = Span::join(
                                            exprs.first().unwrap().span,
                                            exprs.last().unwrap().span,
                                        );

                                        self.expand_list(span, exprs, scope).await
                                    }
                                }
                            })
                            .collect::<Vec<_>>()
                            .await;

                        debug_assert!(!exprs.is_empty());

                        let span =
                            Span::join(exprs.first().unwrap().span, exprs.last().unwrap().span);

                        self.expand_template(
                            max_name,
                            span,
                            max_span,
                            max_operator.template,
                            exprs,
                            None,
                            None,
                            scope,
                        )
                        .await
                    } else {
                        let mut rhs = exprs.split_off(max_index + 1);
                        let mut lhs = exprs;
                        lhs.pop().unwrap();

                        if rhs.is_empty() {
                            self.compiler.add_error(
                                "expected values on right side of operator",
                                vec![Note::primary(
                                    max_span,
                                    "try providing a value to the right of this",
                                )],
                            );

                            Node {
                                span: list_span,
                                kind: NodeKind::error(self.compiler),
                            }
                        } else if lhs.is_empty() {
                            self.compiler.add_error(
                                "expected values on left side of operator",
                                vec![Note::primary(
                                    max_span,
                                    "try providing a value to the left of this",
                                )],
                            );

                            Node {
                                span: list_span,
                                kind: NodeKind::error(self.compiler),
                            }
                        } else {
                            let span =
                                Span::join(lhs.first().unwrap().span, rhs.last().unwrap().span);

                            macro_rules! expand_list {
                                ($exprs:ident) => {
                                    (|| async {
                                        // Prevent flattening of a single parenthesized expression that
                                        // doesn't contain any operators
                                        if $exprs.len() == 1 {
                                            let expr = $exprs.first().unwrap();

                                            if let parse::ExprKind::List(lines) = &expr.kind {
                                                let exprs = lines
                                                    .iter()
                                                    .flat_map(|line| &line.exprs)
                                                    .enumerate();

                                                if exprs.clone().next().is_none() {
                                                    return Node {
                                                        span: expr.span,
                                                        kind: NodeKind::Empty,
                                                    };
                                                }

                                                if self.operators_in_list(exprs.clone(), scope).is_empty() {
                                                    let expr = $exprs.pop().unwrap();

                                                    return Node {
                                                        span: expr.span,
                                                        kind: NodeKind::List(vec![
                                                            self.expand_expr(expr, scope).await,
                                                        ]),
                                                    };
                                                }
                                            }
                                        }

                                        let span = Span::join(
                                            $exprs.first().unwrap().span,
                                            $exprs.last().unwrap().span,
                                        );

                                        self.expand_list(span, $exprs, scope).await
                                    })()
                                };
                            }

                            let lhs = expand_list!(lhs).await;
                            let rhs = expand_list!(rhs).await;

                            self.expand_template(
                                max_name,
                                span,
                                max_span,
                                max_operator.template,
                                vec![lhs, rhs],
                                None,
                                None,
                                scope,
                            )
                            .await
                        }
                    }
                }
            }
        }
    }

    async fn expand_template(
        &self,
        name: InternedString,
        span: Span,
        template_span: Span,
        id: TemplateId,
        exprs: Vec<Node>,
        file_attributes: Option<&mut FileAttributes>,
        statement_attributes: Option<&mut StatementAttributes>,
        scope: &Scope<'_>,
    ) -> Node {
        let template = {
            let mut declarations = self.declarations.lock();

            let decl = declarations
                .templates
                .get_mut(&id)
                .unwrap_or_else(|| panic!("template `{}` ({:?}) not registered", name, id));

            decl.uses.insert(template_span);

            decl.template.clone()
        };

        match template {
            Template::Syntax(inputs, mut body) => {
                if exprs.len() != inputs.len() {
                    self.report_wrong_template_arity(&name, span, exprs.len(), inputs.len());

                    return Node {
                        span,
                        kind: NodeKind::error(self.compiler),
                    };
                }

                fn replace(node: &mut Node, map: &HashMap<InternedString, Node>) {
                    match &mut node.kind {
                        NodeKind::Name(name) => {
                            if let Some(replacement) = map.get(name) {
                                *node = replacement.clone();
                            }
                        }
                        NodeKind::List(nodes) => {
                            for node in nodes {
                                replace(node, map);
                            }
                        }
                        NodeKind::Block(statements) => {
                            for statement in statements {
                                replace(&mut statement.node, map);
                            }
                        }
                        NodeKind::Assign(lhs, rhs) => {
                            replace(lhs, map);
                            replace(rhs, map);
                        }
                        NodeKind::Template(_, node) => replace(node, map),
                        NodeKind::Function(lhs, rhs) => {
                            replace(lhs, map);
                            replace(rhs, map);
                        }
                        NodeKind::External(lib, identifier, inputs) => {
                            replace(lib, map);
                            replace(identifier, map);

                            for input in inputs {
                                replace(input, map);
                            }
                        }
                        NodeKind::Annotate(lhs, rhs) => {
                            replace(lhs, map);
                            replace(rhs, map);
                        }
                        NodeKind::Type(fields) => {
                            if let Some(fields) = fields {
                                for field in fields {
                                    replace(&mut field.node, map);
                                }
                            }
                        }
                        NodeKind::Trait(node) => {
                            if let Some(node) = node {
                                replace(node, map);
                            }
                        }
                        NodeKind::TypeFunction(lhs, rhs) => {
                            replace(lhs, map);
                            replace(rhs, map);
                        }
                        NodeKind::Where(lhs, rhs) => {
                            replace(lhs, map);
                            replace(rhs, map);
                        }
                        NodeKind::Instance(tr, params) => {
                            replace(tr, map);

                            for param in params {
                                replace(param, map);
                            }
                        }
                        NodeKind::When(input, arms) => {
                            replace(input, map);

                            for arm in arms {
                                replace(&mut arm.node, map);
                            }
                        }
                        NodeKind::Or(lhs, rhs) => {
                            replace(lhs, map);
                            replace(rhs, map);
                        }
                        _ => {}
                    }
                }

                replace(
                    &mut body,
                    &inputs.into_iter().zip(exprs).collect::<HashMap<_, _>>(),
                );

                body
            }
            Template::Function(expand) => {
                expand(
                    self,
                    span,
                    exprs,
                    file_attributes,
                    statement_attributes,
                    scope,
                )
                .await
            }
        }
    }

    fn report_wrong_template_arity(
        &self,
        template_name: &str,
        span: Span,
        actual: usize,
        expected: usize,
    ) {
        self.compiler.add_error(
            format!(
                "template `{}` expects {} inputs, but only {} were given",
                template_name, expected, actual,
            ),
            vec![Note::primary(
                span,
                if actual > expected {
                    "try removing some of these inputs"
                } else {
                    "try adding some inputs"
                },
            )],
        );
    }
}
