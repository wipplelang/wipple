#![allow(clippy::type_complexity)]

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

pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub statements: Vec<Node>,
    pub dependencies: Vec<Dependency>,
}

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
    span: Span,
    kind: NodeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Error,
    Name(InternedString),
    Text(InternedString),
    Number(Decimal),
    List(Vec<Node>),
    Block(Vec<Node>),
    Use(InternedString),
    // TODO: Assign, declarations, etc.
}

impl<L: Loader> Compiler<L> {
    pub fn expand(
        &mut self,
        file: parse::File,
        info: &mut Info<L>,
        load: impl Fn(&mut Self, FilePath, &mut Info<L>) -> Option<Rc<Scope<'static>>>,
    ) -> (File, Scope<'static>) {
        let mut expander = Expander {
            compiler: self,
            info,
            dependencies: Default::default(),
            load: &load,
        };

        let scope = Scope::default();

        // TODO: Load builtins

        let statements = expander.expand_block(file.statements, &scope);

        (
            File {
                path: file.path,
                span: file.span,
                statements,
                dependencies: expander.dependencies,
            },
            scope,
        )
    }
}

struct Expander<'a, L: Loader> {
    compiler: &'a mut Compiler<L>,
    info: &'a mut Info<L>,
    dependencies: Vec<Dependency>,
    load: &'a dyn Fn(&mut Compiler<L>, FilePath, &mut Info<L>) -> Option<Rc<Scope<'static>>>,
}

#[derive(Debug, Clone, Default)]
pub struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    values: RefCell<HashMap<InternedString, ScopeValue>>,
}

#[derive(Debug, Clone, Copy)]
enum ScopeValue {
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
    name: InternedString,
    span: Span,
    inputs: Vec<InternedString>,
    body: TemplateBody<L>,
}

impl<L: Loader> Clone for Template<L> {
    fn clone(&self) -> Self {
        Self {
            name: self.name,
            span: self.span,
            inputs: self.inputs.clone(),
            body: self.body.clone(),
        }
    }
}

enum TemplateBody<L: Loader> {
    Node(Node),
    Builtin(Rc<dyn Fn(&mut Expander<L>, Span, Vec<Node>, &Scope) -> Node>),
}

impl<L: Loader> Clone for TemplateBody<L> {
    fn clone(&self) -> Self {
        match self {
            TemplateBody::Node(node) => TemplateBody::Node(node.clone()),
            TemplateBody::Builtin(expand) => TemplateBody::Builtin(expand.clone()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Operator {
    pub precedence: OperatorPrecedence,
    pub associativity: OperatorAssociativity,
    pub template: TemplateId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum OperatorPrecedence {
    Addition = 1,
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
            OperatorPrecedence::Addition => OperatorAssociativity::Left,
        }
    }
}

impl<L: Loader> Expander<'_, L> {
    fn expand_expr(&mut self, expr: parse::Expr, scope: &Scope) -> Node {
        match expr.kind {
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
            parse::ExprKind::Quote(_) => todo!("literals"),
            parse::ExprKind::List(list) => self.expand_list(
                expr.span,
                list.into_iter().flat_map(|line| line.exprs).collect(),
                scope,
            ),
            parse::ExprKind::Block(statements) => Node {
                span: expr.span,
                kind: NodeKind::Block(self.expand_block(statements, scope)),
            },
        }
    }

    fn expand_block(&mut self, statements: Vec<parse::Statement>, scope: &Scope) -> Vec<Node> {
        let scope = Scope {
            parent: Some(scope),
            ..Default::default()
        };

        statements
            .into_iter()
            .map(|statement| {
                let exprs = statement
                    .lines
                    .into_iter()
                    .flat_map(|line| line.exprs)
                    .collect::<Vec<_>>();

                let span = Span::join(exprs.first().unwrap().span, exprs.last().unwrap().span);

                self.expand_list(span, exprs, &scope)
            })
            .collect()
    }

    fn expand_list(&mut self, list_span: Span, mut exprs: Vec<parse::Expr>, scope: &Scope) -> Node {
        if exprs.is_empty() {
            return Node {
                span: list_span,
                kind: NodeKind::List(Vec::new()),
            };
        }

        let mut operators = VecDeque::new();
        for (index, expr) in exprs.iter().enumerate() {
            if let parse::ExprKind::Name(name) = &expr.kind {
                if let Some(ScopeValue::Operator(operator)) = scope.get(*name) {
                    operators.push_back((index, expr.span, operator))
                }
            }
        }

        if operators.is_empty() {
            if let parse::ExprKind::Name(name) = &exprs.first().unwrap().kind {
                if let Some(ScopeValue::Template(template)) = scope.get(*name) {
                    let inputs = exprs
                        .into_iter()
                        .skip(1)
                        .map(|expr| self.expand_expr(expr, scope))
                        .collect();

                    return self.expand_template(list_span, template, inputs, scope);
                }
            }

            todo!()
        } else {
            let (mut max_index, mut max_span, mut max_operator) = operators.pop_front().unwrap();

            for (index, span, operator) in operators {
                macro_rules! replace {
                    () => {{
                        max_index = index;
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

                let lhs =
                    self.expand_list(Span::join(lhs.first().unwrap().span, max_span), lhs, scope);

                let rhs =
                    self.expand_list(Span::join(max_span, rhs.last().unwrap().span), rhs, scope);

                self.expand_template(span, max_operator.template, vec![lhs, rhs], scope)
            }
        }
    }

    fn expand_template(
        &mut self,
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

        match &template.body {
            TemplateBody::Node(_) => todo!("syntax templates"),
            TemplateBody::Builtin(expand) => expand(self, span, exprs, scope),
        }
    }
}
