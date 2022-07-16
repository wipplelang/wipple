#![allow(
    clippy::type_complexity,
    clippy::collapsible_match,
    clippy::too_many_arguments
)]

mod builtins;

use crate::{
    diagnostics::*,
    helpers::InternedString,
    parse::{self, Span},
    Compiler, FilePath, Loader, TemplateId,
};
use async_recursion::async_recursion;
use futures::{future::BoxFuture, stream, StreamExt};
use parking_lot::Mutex;
use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap, VecDeque},
    fmt::Debug,
    sync::Arc,
};
use strum::EnumString;

pub struct File<L: Loader> {
    pub path: FilePath,
    pub span: Span,
    pub attributes: FileAttributes,
    pub declarations: Declarations<L>,
    pub exported: ScopeValues,
    pub statements: Vec<Statement>,
    pub dependencies: Vec<Arc<File<L>>>,
}

impl<L: Loader> Debug for File<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("File")
            .field("path", &self.path)
            .field("span", &self.span)
            .field("attributes", &self.attributes)
            .field("declarations", &self.declarations)
            .field("exported", &self.exported)
            .field("statements", &self.statements)
            .field("dependencies", &self.dependencies)
            .finish()
    }
}

impl<L: Loader> Clone for File<L> {
    fn clone(&self) -> Self {
        Self {
            path: self.path,
            span: self.span,
            attributes: self.attributes.clone(),
            declarations: self.declarations.clone(),
            exported: self.exported.clone(),
            statements: self.statements.clone(),
            dependencies: self.dependencies.clone(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct FileAttributes {
    pub no_std: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub attributes: StatementAttributes,
    pub node: Node,
    pub treat_as_expr: bool,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct StatementAttributes {
    pub language_item: Option<LanguageItem>,
    pub help: VecDeque<InternedString>,
    pub on_unimplemented: Option<InternedString>,
}

#[derive(Debug, Clone, Copy, PartialEq, EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum LanguageItem {
    Boolean,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub span: Span,
    pub kind: NodeKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Error,
    Placeholder,
    Empty,
    Underscore,
    Name(InternedString),
    Text(InternedString),
    Number(f64),
    List(Vec<Node>),
    Block(Vec<Statement>),
    Assign(Box<Node>, Box<Node>),
    Template(Vec<InternedString>, Box<Node>),
    Operator(OperatorPrecedence, Vec<InternedString>, Box<Node>),
    Function(Box<Node>, Box<Node>),
    External(Box<Node>, Box<Node>, Vec<Node>),
    Annotate(Box<Node>, Box<Node>),
    Type(Option<Vec<Statement>>),
    Trait(Box<Node>),
    TypeFunction(Box<Node>, Box<Node>),
    Where(Box<Node>, Box<Node>),
    Instance(Box<Node>, Vec<Node>),
    Use(Box<Node>),
    When(Box<Node>, Vec<Statement>),
    Return(Box<Node>),
    Or(Box<Node>, Box<Node>),
    Loop(Box<Node>),
    Break(Box<Node>),
    Continue,
    Tuple(Vec<Node>),
}

pub struct Declarations<L: Loader> {
    templates: BTreeMap<TemplateId, Template<L>>,
}

impl<L: Loader> Default for Declarations<L> {
    fn default() -> Self {
        Declarations {
            templates: Default::default(),
        }
    }
}

impl<L: Loader> Declarations<L> {
    fn merge(&mut self, other: Self) {
        self.templates.extend(other.templates);
    }
}

impl<L: Loader> std::fmt::Debug for Declarations<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Declarations")
            .field("templates", &self.templates)
            .finish()
    }
}

impl<L: Loader> Clone for Declarations<L> {
    fn clone(&self) -> Self {
        Self {
            templates: self.templates.clone(),
        }
    }
}

impl<L: Loader> Compiler<L> {
    pub async fn expand(
        &self,
        file: parse::File,
        load: impl Fn(&Compiler<L>, FilePath) -> BoxFuture<Option<Arc<File<L>>>> + 'static + Send + Sync,
    ) -> Option<File<L>> {
        let mut expander = Expander {
            compiler: self.clone(),
            declarations: Default::default(),
            dependencies: Default::default(),
            load: Arc::new(load),
        };

        let scope = Scope::default();
        builtins::load_builtins(&mut expander, &scope);

        let attributes = expander
            .expand_file_attributes(file.attributes, &scope)
            .await;

        if !attributes.no_std {
            let std_path = expander.compiler.loader.std_path();
            if let Some(std_path) = std_path {
                let file = (expander.load)(&expander.compiler, std_path).await?;
                expander.add_dependency(file, &scope);
            } else {
                expander.compiler.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "standard library is missing, but this file requires it",
                    vec![Note::primary(
                        file.span.with_end(file.span.start),
                        "try adding [[no-std]] to this file to prevent automatically loading the standard library",
                    )],
                ));
            }
        }

        let (statements, exported) = expander.expand_block(file.statements, &scope).await;

        Some(File {
            path: file.path,
            span: file.span,
            statements,
            attributes,
            declarations: Arc::try_unwrap(expander.declarations).unwrap().into_inner(),
            exported,
            dependencies: Arc::try_unwrap(expander.dependencies)
                .unwrap_or_else(|_| unreachable!())
                .into_inner(),
        })
    }
}

#[derive(Clone)]
struct Expander<L: Loader> {
    compiler: Compiler<L>,
    declarations: Arc<Mutex<Declarations<L>>>,
    dependencies: Arc<Mutex<Vec<Arc<File<L>>>>>,
    load: Arc<dyn Fn(&Compiler<L>, FilePath) -> BoxFuture<Option<Arc<File<L>>>> + Send + Sync>,
}

#[derive(Debug, Clone, Default)]
pub struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    values: Arc<Mutex<ScopeValues>>,
}

pub type ScopeValues = HashMap<InternedString, ScopeValue>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

struct Template<L: Loader> {
    span: Span,
    body: TemplateBody<L>,
}

impl<L: Loader> Clone for Template<L> {
    fn clone(&self) -> Self {
        Self {
            span: self.span,
            body: self.body.clone(),
        }
    }
}

impl<L: Loader> std::fmt::Debug for Template<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Template")
            .field("span", &self.span)
            .field("body", &self.body)
            .finish()
    }
}

enum TemplateBody<L: Loader> {
    Syntax(Vec<InternedString>, Node),
    Function(
        Arc<
            dyn for<'a> Fn(
                    &'a Expander<L>,
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

impl<L: Loader> Clone for TemplateBody<L> {
    fn clone(&self) -> Self {
        match self {
            TemplateBody::Syntax(inputs, node) => {
                TemplateBody::Syntax(inputs.clone(), node.clone())
            }
            TemplateBody::Function(expand) => TemplateBody::Function(expand.clone()),
        }
    }
}

impl<L: Loader> std::fmt::Debug for TemplateBody<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Syntax(inputs, node) => {
                f.debug_tuple("Syntax").field(inputs).field(node).finish()
            }
            Self::Function(_) => f.debug_tuple("Function").finish(),
        }
    }
}

impl<L: Loader> Template<L> {
    fn syntax(span: Span, inputs: Vec<InternedString>, node: Node) -> Self {
        Template {
            span,
            body: TemplateBody::Syntax(inputs, node),
        }
    }

    fn function(
        span: Span,
        f: impl for<'a> Fn(
                &'a Expander<L>,
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
        Template {
            span,
            body: TemplateBody::Function(Arc::new(f)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Operator {
    pub precedence: OperatorPrecedence,
    pub template: TemplateId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OperatorPrecedence {
    Power,
    Multiplication,
    Addition,
    Comparison,
    Conjunction,
    Disjunction,
    Dot,
    Comma,
    Where,
    Function,
    TypeFunction,
    Annotation,
    Assignment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None { error: bool },
}

impl OperatorPrecedence {
    pub fn associativity(&self) -> OperatorAssociativity {
        match self {
            OperatorPrecedence::Conjunction => OperatorAssociativity::Left,
            OperatorPrecedence::Disjunction => OperatorAssociativity::Left,
            OperatorPrecedence::Comparison => OperatorAssociativity::Left,
            OperatorPrecedence::Addition => OperatorAssociativity::Left,
            OperatorPrecedence::Multiplication => OperatorAssociativity::Left,
            OperatorPrecedence::Power => OperatorAssociativity::Right,
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

impl<L: Loader> Expander<L> {
    fn add_dependency(&self, file: Arc<File<L>>, scope: &Scope) {
        self.dependencies.lock().push(file.clone());
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
                if !matches!($node.kind, NodeKind::Empty | NodeKind::Error) {
                    self.compiler.diagnostics.add(Diagnostic::new(
                        DiagnosticLevel::Error,
                        "invalid file attribute",
                        vec![Note::primary(
                            $span,
                            "this attribute must expand to an empty expression",
                        )],
                    ));
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
                                self.compiler.diagnostics.add(Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    "unexpected operator",
                                    vec![Note::primary(
                                        attribute.span,
                                        "operators aren't allowed within attributes",
                                    )],
                                ));

                                continue 'attributes;
                            }
                        }
                    }

                    let mut exprs = exprs.into_iter();

                    let first = exprs.next().unwrap();
                    let name = match first.kind {
                        parse::ExprKind::Name(name) => name,
                        _ => {
                            self.compiler.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "expected name in attribute",
                                vec![Note::primary(
                                    attribute.span,
                                    "expected name of template here",
                                )],
                            ));

                            continue 'attributes;
                        }
                    };

                    let template = match scope.get(name) {
                        Some(ScopeValue::Template(template)) => template,
                        Some(ScopeValue::Operator(_)) => {
                            unreachable!("operators trigger an error above");
                        }
                        None => {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                format!("cannot find template `{}`", name),
                                vec![Note::primary(first.span, "no such template")],
                            ));

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
                    self.compiler.diagnostics.add(Diagnostic::new(
                        DiagnosticLevel::Error,
                        "invalid attribute",
                        vec![Note::primary(
                            attribute.span,
                            "expected a call to a template here",
                        )],
                    ));
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
                let (statements, _) = self.expand_block(statements, scope).await;

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
                            self.compiler.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "unexpected attribute",
                                vec![Note::primary(
                                    attribute.span,
                                    "try placing this attribute before the statement",
                                )],
                            ));
                        }

                        line.exprs
                    }))
                    .flatten()
                    .collect::<Vec<_>>();

                if exprs.is_empty() {
                    for attribute in first_attributes {
                        self.compiler.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "unexpected attribute",
                            vec![Note::primary(
                                attribute.span,
                                "try adding an expression after this attribute",
                            )],
                        ));
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
                                        self.compiler.diagnostics.add(Diagnostic::new(
                                            DiagnosticLevel::Error,
                                            "unexpected operator",
                                            vec![Note::primary(
                                                attribute.span,
                                                "operators aren't allowed within attributes",
                                            )],
                                        ));

                                        continue 'attributes;
                                    }
                                }
                            }

                            let mut exprs = exprs.into_iter();

                            let first = exprs.next().unwrap();
                            let name = match first.kind {
                                parse::ExprKind::Name(name) => name,
                                _ => {
                                    self.compiler.diagnostics.add(Diagnostic::new(
                                        DiagnosticLevel::Error,
                                        "expected name in attribute",
                                        vec![Note::primary(
                                            attribute.span,
                                            "expected name of template here",
                                        )],
                                    ));

                                    continue 'attributes;
                                }
                            };

                            let template = match scope.get(name) {
                                Some(ScopeValue::Template(template)) => template,
                                Some(ScopeValue::Operator(_)) => {
                                    unreachable!("operators trigger an error above");
                                }
                                None => {
                                    self.compiler.diagnostics.add(Diagnostic::error(
                                        format!("cannot find template `{}`", name),
                                        vec![Note::primary(first.span, "no such template")],
                                    ));

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
                                    template,
                                    inputs,
                                    None,
                                    Some(&mut attributes),
                                    &scope,
                                )
                                .await;
                        }
                        _ => {
                            self.compiler.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "invalid attribute",
                                vec![Note::primary(
                                    attribute.span,
                                    "expected a call to a template here",
                                )],
                            ));
                        }
                    }
                }

                (!matches!(node.kind, NodeKind::Placeholder)).then(|| Statement {
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

        (
            statements,
            Arc::try_unwrap(scope.values).unwrap().into_inner(),
        )
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
                                    *name, list_span, template, inputs, None, None, scope,
                                )
                                .await;
                        }
                        Some(ScopeValue::Operator(_)) => {
                            self.compiler.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "expected values on both sides of operator",
                                vec![Note::primary(
                                    expr.span,
                                    "try providing values on either side of this",
                                )],
                            ));

                            return Node {
                                span: list_span,
                                kind: NodeKind::Error,
                            };
                        }
                        _ => {}
                    }
                }

                self.expand_expr(expr, scope).await
            }
            _ => {
                let mut operators = VecDeque::new();
                for (index, expr) in exprs.iter().enumerate() {
                    if let parse::ExprKind::Name(name) = &expr.kind {
                        if let Some(ScopeValue::Operator(operator)) = scope.get(*name) {
                            operators.push_back((index, *name, expr.span, operator))
                        }
                    }
                }

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
                                    name, list_span, template, inputs, None, None, scope,
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
                                        self.compiler.diagnostics.add(Diagnostic::new(
                                            DiagnosticLevel::Error,
                                            "operator ambiguity",
                                            vec![
                                                Note::primary(
                                                    exprs[index].span,
                                                    "only one of this operator may be provided at a time",
                                                ),
                                                Note::secondary(
                                                    exprs[max_index].span,
                                                    "first use of this operator",
                                                ),
                                            ],
                                        ));

                                        return Node {
                                            span: list_span,
                                            kind: NodeKind::Error,
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
                                                dbg!(operator_index, &operators);

                                                let operator_span = operators
                                                    .iter()
                                                    .find_map(|(index, _, span, _)| {
                                                        (*index == operator_index).then(|| *span)
                                                    })
                                                    .unwrap();

                                                self.compiler.diagnostics.add(Diagnostic::new(
                                                    DiagnosticLevel::Error,
                                                        "expected values on right side of operator",
                                                        vec![Note::primary(
                                                        operator_span,
                                                        "try providing a value to the right of this",
                                                    )],
                                                ));
                                            } else {
                                                let operator_span = operators.front().unwrap().2;

                                                self.compiler.diagnostics.add(Diagnostic::new(
                                                    DiagnosticLevel::Error,
                                                        "expected values on left side of operator",
                                                        vec![Note::primary(
                                                        operator_span,
                                                        "try providing a value to the left of this",
                                                    )],
                                                ));
                                            }

                                            return Node {
                                                span: list_span,
                                                kind: NodeKind::Error,
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
                            max_operator.template,
                            exprs,
                            None,
                            None,
                            scope,
                        )
                        .await
                    } else {
                        let rhs = exprs.split_off(max_index + 1);
                        let mut lhs = exprs;
                        lhs.pop().unwrap();

                        if rhs.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "expected values on right side of operator",
                                vec![Note::primary(
                                    max_span,
                                    "try providing a value to the right of this",
                                )],
                            ));

                            Node {
                                span: list_span,
                                kind: NodeKind::Error,
                            }
                        } else if lhs.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "expected values on left side of operator",
                                vec![Note::primary(
                                    max_span,
                                    "try providing a value to the left of this",
                                )],
                            ));

                            Node {
                                span: list_span,
                                kind: NodeKind::Error,
                            }
                        } else {
                            let span =
                                Span::join(lhs.first().unwrap().span, rhs.last().unwrap().span);

                            let lhs = self
                                .expand_list(
                                    Span::join(lhs.first().unwrap().span, lhs.last().unwrap().span),
                                    lhs,
                                    scope,
                                )
                                .await;

                            let rhs = self
                                .expand_list(
                                    Span::join(rhs.first().unwrap().span, rhs.last().unwrap().span),
                                    rhs,
                                    scope,
                                )
                                .await;

                            self.expand_template(
                                max_name,
                                span,
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
        id: TemplateId,
        exprs: Vec<Node>,
        file_attributes: Option<&mut FileAttributes>,
        statement_attributes: Option<&mut StatementAttributes>,
        scope: &Scope<'_>,
    ) -> Node {
        let template = self
            .declarations
            .lock()
            .templates
            .get(&id)
            .unwrap_or_else(|| panic!("template `{}` ({:?}) not registered", name, id))
            .clone();

        match template.body {
            TemplateBody::Syntax(inputs, mut body) => {
                if exprs.len() != inputs.len() {
                    self.report_wrong_template_arity(&name, span, exprs.len(), inputs.len());

                    return Node {
                        span,
                        kind: NodeKind::Error,
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
                        NodeKind::External(abi, identifier, inputs) => {
                            replace(abi, map);
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
                        NodeKind::Trait(node) => replace(node, map),
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
                        NodeKind::Return(value) => {
                            replace(value, map);
                        }
                        NodeKind::Or(lhs, rhs) => {
                            replace(lhs, map);
                            replace(rhs, map);
                        }
                        NodeKind::Loop(body) => {
                            replace(body, map);
                        }
                        NodeKind::Break(value) => {
                            replace(value, map);
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
            TemplateBody::Function(expand) => {
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
        self.compiler.diagnostics.add(Diagnostic::error(
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
        ));
    }
}
