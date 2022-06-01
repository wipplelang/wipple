#![allow(clippy::type_complexity, clippy::collapsible_match)]

mod builtins;

use crate::{
    diagnostics::*,
    helpers::InternedString,
    parse::{self, Span},
    Compiler, FilePath, Loader, TemplateId,
};
use rust_decimal::Decimal;
use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{BTreeMap, HashMap, VecDeque},
    rc::Rc,
};

#[derive(Debug)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub statements: Vec<Node>,
    pub dependencies: Vec<Dependency>,
}

#[derive(Debug)]
pub struct Dependency {
    pub span: Span,
    pub name: InternedString,
}

pub struct Info<L: Loader> {
    templates: BTreeMap<TemplateId, Template<L>>,
}

impl<L: Loader> Default for Info<L> {
    fn default() -> Self {
        Info {
            templates: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub span: Span,
    pub kind: NodeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Error,
    Empty,
    Underscore,
    Name(InternedString),
    Text(InternedString),
    Number(Decimal),
    List(Vec<Node>),
    Block(Vec<Node>),
    Assign(Box<Node>, Box<Node>),
    Template(Vec<InternedString>, Box<Node>),
    Operator(OperatorPrecedence, Vec<InternedString>, Box<Node>),
    Function(Box<Node>, Box<Node>),
    External(Box<Node>, Box<Node>, Vec<Node>),
    Annotate(Box<Node>, Box<Node>),
    Type(Option<Vec<Node>>),
    Trait(Box<Node>),
    TypeFunction(Box<Node>, Box<Node>),
    WhereClause(Box<Node>, Box<Node>),
    Instance(Box<Node>, Vec<Node>),
    ListLiteral(Vec<Node>),
    Use(Box<Node>),
    When(Box<Node>, Vec<Node>),
}

impl<L: Loader> Compiler<L> {
    pub fn expand(
        &mut self,
        file: parse::File,
        info: &mut Info<L>,
        load: impl Fn(&mut Self, FilePath, &mut Info<L>) -> Option<Rc<ScopeValues>>,
    ) -> (File, ScopeValues) {
        let mut expander = Expander {
            compiler: self,
            info,
            dependencies: Default::default(),
            load: &load,
        };

        let scope = Scope::default();
        builtins::load_builtins(&mut expander, &scope);

        // TODO: Respect `[: no-prelude :]` instead of checking the path
        if file.path != FilePath::Prelude {
            let prelude_scope =
                (expander.load)(expander.compiler, FilePath::Prelude, expander.info)
                    .expect("failed to load prelude");

            scope.values.borrow_mut().extend((*prelude_scope).clone());
        }

        let (statements, exported) = expander.expand_block(file.statements, &scope);

        (
            File {
                path: file.path,
                span: file.span,
                statements,
                dependencies: expander.dependencies,
            },
            exported,
        )
    }
}

struct Expander<'a, L: Loader> {
    compiler: &'a mut Compiler<L>,
    info: &'a mut Info<L>,
    dependencies: Vec<Dependency>,
    load: &'a dyn Fn(&mut Compiler<L>, FilePath, &mut Info<L>) -> Option<Rc<ScopeValues>>,
}

#[derive(Debug, Clone, Default)]
pub struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    values: RefCell<ScopeValues>,
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
            if let Some(value) = scope.values.borrow().get(&name).cloned() {
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

enum TemplateBody<L: Loader> {
    Syntax(Vec<InternedString>, Node),
    Function(Rc<dyn Fn(&mut Expander<L>, Span, Vec<Node>, &Scope) -> Node>),
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

impl<L: Loader> Template<L> {
    fn syntax(span: Span, inputs: Vec<InternedString>, node: Node) -> Self {
        Template {
            span,
            body: TemplateBody::Syntax(inputs, node),
        }
    }

    fn function(
        span: Span,
        f: impl Fn(&mut Expander<L>, Span, Vec<Node>, &Scope) -> Node + 'static,
    ) -> Self {
        Template {
            span,
            body: TemplateBody::Function(Rc::new(f)),
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
    Conjunction,
    Disjunction,
    Power,
    Multiplication,
    Addition,
    Dot,
    Function,
    Where,
    TypeFunction,
    Annotation,
    Assignment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None,
}

impl OperatorPrecedence {
    pub fn associativity(&self) -> OperatorAssociativity {
        match self {
            OperatorPrecedence::Conjunction => OperatorAssociativity::Left,
            OperatorPrecedence::Disjunction => OperatorAssociativity::Left,
            OperatorPrecedence::Addition => OperatorAssociativity::Left,
            OperatorPrecedence::Multiplication => OperatorAssociativity::Left,
            OperatorPrecedence::Power => OperatorAssociativity::Right,
            OperatorPrecedence::Dot => OperatorAssociativity::Left,
            OperatorPrecedence::Function => OperatorAssociativity::Right,
            OperatorPrecedence::Where => OperatorAssociativity::None,
            OperatorPrecedence::TypeFunction => OperatorAssociativity::None,
            OperatorPrecedence::Annotation => OperatorAssociativity::Left,
            OperatorPrecedence::Assignment => OperatorAssociativity::None,
        }
    }
}

impl<L: Loader> Expander<'_, L> {
    fn expand_expr(&mut self, expr: parse::Expr, scope: &Scope) -> Node {
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
            parse::ExprKind::List(list) => self.expand_list(
                expr.span,
                list.into_iter().flat_map(|line| line.exprs).collect(),
                scope,
            ),
            parse::ExprKind::ListLiteral(list) => Node {
                span: expr.span,
                kind: NodeKind::ListLiteral(
                    list.into_iter()
                        .flat_map(|line| line.exprs)
                        .map(|expr| self.expand_expr(expr, scope))
                        .collect(),
                ),
            },
            parse::ExprKind::Block(statements) => {
                let (statements, _) = self.expand_block(statements, scope);

                Node {
                    span: expr.span,
                    kind: NodeKind::Block(statements),
                }
            }
        }
    }

    fn expand_block(
        &mut self,
        statements: Vec<parse::Statement>,
        scope: &Scope,
    ) -> (Vec<Node>, ScopeValues) {
        let scope = scope.child();

        let statements = statements
            .into_iter()
            .filter_map(|statement| {
                let exprs = statement
                    .lines
                    .into_iter()
                    .flat_map(|line| line.exprs)
                    .collect::<Vec<_>>();

                if exprs.is_empty() {
                    return None;
                }

                let span = Span::join(exprs.first().unwrap().span, exprs.last().unwrap().span);

                Some(self.expand_list(span, exprs, &scope))
            })
            .collect();

        (statements, scope.values.into_inner())
    }

    fn expand_list(&mut self, list_span: Span, mut exprs: Vec<parse::Expr>, scope: &Scope) -> Node {
        match exprs.len() {
            0 => Node {
                span: list_span,
                kind: NodeKind::Empty,
            },
            1 => {
                let expr = exprs.pop().unwrap();

                if let parse::ExprKind::Name(name) = &expr.kind {
                    if let Some(ScopeValue::Template(template)) = scope.get(*name) {
                        let inputs = exprs
                            .into_iter()
                            .skip(1)
                            .map(|expr| self.expand_expr(expr, scope))
                            .collect();

                        return self.expand_template(*name, list_span, template, inputs, scope);
                    }
                }

                self.expand_expr(expr, scope)
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
                            let inputs = exprs.map(|expr| self.expand_expr(expr, scope)).collect();
                            return self.expand_template(name, list_span, template, inputs, scope);
                        }
                    }

                    Node {
                        span: list_span,
                        kind: NodeKind::List(
                            std::iter::once(first)
                                .chain(exprs)
                                .map(|expr| self.expand_expr(expr, scope))
                                .collect(),
                        ),
                    }
                } else {
                    let (mut max_index, mut max_name, mut max_span, mut max_operator) =
                        operators.pop_front().unwrap();

                    for (index, name, span, operator) in operators {
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
                            Ordering::Equal => {
                                if operator.precedence.associativity()
                                    != max_operator.precedence.associativity()
                                {
                                    self.compiler.diagnostics.add(Diagnostic::new(
                                        DiagnosticLevel::Error,
                                        "operator ambiguity",
                                        vec![
                                            Note::primary(
                                                exprs[max_index].span,
                                                "ambiguous whether to parse this operator first...",
                                            ),
                                            Note::primary(exprs[index].span, "...or this one"),
                                        ],
                                    ));

                                    return Node {
                                        span: list_span,
                                        kind: NodeKind::Error,
                                    };
                                }

                                match operator.precedence.associativity() {
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
                                    OperatorAssociativity::None => {
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
                            }
                        }
                    }

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
                        let span = Span::join(lhs.first().unwrap().span, rhs.last().unwrap().span);

                        let lhs = self.expand_list(
                            Span::join(lhs.first().unwrap().span, lhs.last().unwrap().span),
                            lhs,
                            scope,
                        );

                        let rhs = self.expand_list(
                            Span::join(rhs.first().unwrap().span, rhs.last().unwrap().span),
                            rhs,
                            scope,
                        );

                        self.expand_template(
                            max_name,
                            span,
                            max_operator.template,
                            vec![lhs, rhs],
                            scope,
                        )
                    }
                }
            }
        }
    }

    fn expand_template(
        &mut self,
        name: InternedString,
        span: Span,
        template: TemplateId,
        exprs: Vec<Node>,
        scope: &Scope,
    ) -> Node {
        let template = self
            .info
            .templates
            .get(&template)
            .expect("template not registered")
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
                                replace(statement, map);
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
                        NodeKind::External(namespace, identifier, inputs) => {
                            replace(namespace, map);
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
                                    replace(field, map);
                                }
                            }
                        }
                        NodeKind::Trait(node) => replace(node, map),
                        NodeKind::TypeFunction(lhs, rhs) => {
                            replace(lhs, map);
                            replace(rhs, map);
                        }
                        NodeKind::WhereClause(lhs, rhs) => {
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
                                replace(arm, map);
                            }
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
            TemplateBody::Function(expand) => expand(self, span, exprs, scope),
        }
    }

    fn report_wrong_template_arity(
        &mut self,
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
