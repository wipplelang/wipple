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
use rust_decimal::Decimal;
use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{BTreeMap, HashMap, VecDeque},
    rc::Rc,
};
use strum::EnumString;

#[derive(Debug)]
pub struct File<L> {
    pub path: FilePath,
    pub span: Span,
    pub attributes: FileAttributes,
    pub declarations: Declarations<L>,
    pub exported: ScopeValues,
    pub statements: Vec<Statement>,
    pub dependencies: Vec<Rc<File<L>>>,
}

impl<L> Clone for File<L> {
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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FileAttributes {
    pub no_std: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Statement {
    pub attributes: StatementAttributes,
    pub node: Node,
    pub treat_as_expr: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct StatementAttributes {
    pub language_item: Option<LanguageItem>,
    pub help: VecDeque<InternedString>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum LanguageItem {
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub span: Span,
    pub kind: NodeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Error,
    Placeholder,
    Empty,
    Underscore,
    Name(InternedString),
    Text(InternedString),
    Number(Decimal),
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
    ListLiteral(Vec<Node>),
    Use(Box<Node>),
    When(Box<Node>, Vec<Statement>),
    Return(Box<Node>),
    Or(Box<Node>, Box<Node>),
    Loop(Box<Node>),
    Break(Box<Node>),
    Continue,
}

pub struct Declarations<L> {
    templates: BTreeMap<TemplateId, Template<L>>,
}

impl<L> Default for Declarations<L> {
    fn default() -> Self {
        Declarations {
            templates: Default::default(),
        }
    }
}

impl<L> Declarations<L> {
    fn merge(&mut self, other: Self) {
        self.templates.extend(other.templates);
    }
}

impl<L> std::fmt::Debug for Declarations<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Declarations")
            .field("templates", &self.templates)
            .finish()
    }
}

impl<L> Clone for Declarations<L> {
    fn clone(&self) -> Self {
        Self {
            templates: self.templates.clone(),
        }
    }
}

impl<L: Loader> Compiler<'_, L> {
    pub fn expand(
        &mut self,
        file: parse::File,
        mut load: impl FnMut(&mut Compiler<L>, FilePath) -> Option<Rc<File<L>>>,
    ) -> Option<File<L>> {
        let mut expander = Expander {
            compiler: self,
            declarations: Default::default(),
            dependencies: Default::default(),
            load: &mut load,
        };

        let scope = Scope::default();
        builtins::load_builtins(&mut expander, &scope);

        let attributes = expander.expand_file_attributes(file.attributes, &scope);

        if !attributes.no_std {
            if let Some(std_path) = expander.compiler.loader.std_path() {
                let file = (expander.load)(expander.compiler, std_path)?;
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

        let (statements, exported) = expander.expand_block(file.statements, &scope);

        Some(File {
            path: file.path,
            span: file.span,
            statements,
            attributes,
            declarations: expander.declarations,
            exported,
            dependencies: expander.dependencies,
        })
    }
}

struct Expander<'a, 'l, L> {
    compiler: &'a mut Compiler<'l, L>,
    declarations: Declarations<L>,
    dependencies: Vec<Rc<File<L>>>,
    load: &'a mut dyn FnMut(&mut Compiler<L>, FilePath) -> Option<Rc<File<L>>>,
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

struct Template<L> {
    span: Span,
    body: TemplateBody<L>,
}

impl<L> Clone for Template<L> {
    fn clone(&self) -> Self {
        Self {
            span: self.span,
            body: self.body.clone(),
        }
    }
}

impl<L> std::fmt::Debug for Template<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Template")
            .field("span", &self.span)
            .field("body", &self.body)
            .finish()
    }
}

enum TemplateBody<L> {
    Syntax(Vec<InternedString>, Node),
    Function(
        Rc<
            dyn Fn(
                &mut Expander<L>,
                Span,
                Vec<Node>,
                Option<&mut FileAttributes>,
                Option<&mut StatementAttributes>,
                &Scope,
            ) -> Node,
        >,
    ),
}

impl<L> Clone for TemplateBody<L> {
    fn clone(&self) -> Self {
        match self {
            TemplateBody::Syntax(inputs, node) => {
                TemplateBody::Syntax(inputs.clone(), node.clone())
            }
            TemplateBody::Function(expand) => TemplateBody::Function(expand.clone()),
        }
    }
}

impl<L> std::fmt::Debug for TemplateBody<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Syntax(inputs, node) => {
                f.debug_tuple("Syntax").field(inputs).field(node).finish()
            }
            Self::Function(_) => f.debug_tuple("Function").finish(),
        }
    }
}

impl<L> Template<L> {
    fn syntax(span: Span, inputs: Vec<InternedString>, node: Node) -> Self {
        Template {
            span,
            body: TemplateBody::Syntax(inputs, node),
        }
    }

    fn function(
        span: Span,
        f: impl Fn(
                &mut Expander<L>,
                Span,
                Vec<Node>,
                Option<&mut FileAttributes>,
                Option<&mut StatementAttributes>,
                &Scope,
            ) -> Node
            + 'static,
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
    Power,
    Multiplication,
    Addition,
    Comparison,
    Conjunction,
    Disjunction,
    Dot,
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
    None,
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
            OperatorPrecedence::Where => OperatorAssociativity::None,
            OperatorPrecedence::Function => OperatorAssociativity::Right,
            OperatorPrecedence::TypeFunction => OperatorAssociativity::None,
            OperatorPrecedence::Annotation => OperatorAssociativity::Left,
            OperatorPrecedence::Assignment => OperatorAssociativity::None,
        }
    }
}

impl<L> Expander<'_, '_, L> {
    fn add_dependency(&mut self, file: Rc<File<L>>, scope: &Scope) {
        self.dependencies.push(file.clone());
        scope.values.borrow_mut().extend(file.exported.clone());
        self.declarations.merge(file.declarations.clone());
    }

    fn expand_file_attributes(
        &mut self,
        file_attributes: Vec<parse::Expr>,
        scope: &Scope,
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
                        let node = self.expand_template(
                            name,
                            attribute.span,
                            template,
                            Vec::new(),
                            Some(&mut attributes),
                            None,
                            scope,
                        );

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

                    let inputs = exprs
                        .map(|expr| self.expand_expr(expr, scope))
                        .collect::<Vec<_>>();

                    let node = self.expand_template(
                        name,
                        attribute.span,
                        template,
                        inputs,
                        Some(&mut attributes),
                        None,
                        scope,
                    );

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
    ) -> (Vec<Statement>, ScopeValues) {
        let scope = scope.child();

        let statements = statements
            .into_iter()
            .filter_map(|statement| {
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

                let mut node = self.expand_list(span, exprs, &scope);

                let mut attributes = StatementAttributes::default();
                'attributes: for attribute in first_attributes.into_iter().rev() {
                    match attribute.kind {
                        parse::ExprKind::Name(name) => {
                            if let Some(ScopeValue::Template(template)) = scope.get(name) {
                                node = self.expand_template(
                                    name,
                                    attribute.span,
                                    template,
                                    vec![node],
                                    None,
                                    Some(&mut attributes),
                                    &scope,
                                );
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

                            let inputs = exprs
                                .map(|expr| self.expand_expr(expr, &scope))
                                .chain(std::iter::once(node))
                                .collect::<Vec<_>>();

                            node = self.expand_template(
                                name,
                                attribute.span,
                                template,
                                inputs,
                                None,
                                Some(&mut attributes),
                                &scope,
                            );
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

                        return self.expand_template(
                            *name, list_span, template, inputs, None, None, scope,
                        );
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
                            return self.expand_template(
                                name, list_span, template, inputs, None, None, scope,
                            );
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
                            None,
                            None,
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
        id: TemplateId,
        exprs: Vec<Node>,
        file_attributes: Option<&mut FileAttributes>,
        statement_attributes: Option<&mut StatementAttributes>,
        scope: &Scope,
    ) -> Node {
        let template = self
            .declarations
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
            TemplateBody::Function(expand) => expand(
                self,
                span,
                exprs,
                file_attributes,
                statement_attributes,
                scope,
            ),
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
