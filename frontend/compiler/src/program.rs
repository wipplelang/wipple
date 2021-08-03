use crate::*;
use codemap_diagnostic::Diagnostic;
use std::{borrow::Cow, collections::HashMap, rc::Rc};
use wipple_bytecode::{constant::Constant, module};

pub(crate) trait Expression {
    fn compile<'a>(&'a self, context: &mut Context<'a>) -> Option<Compiled<'a>>;
}

pub(crate) enum Compiled<'a> {
    Type(typecheck::Type),
    // TODO: Type variable
    Attribute(Attribute),
    Template(Template),
    Operator(Operator),
    Precedence(Precedence),
    Variable(Variable<'a>),
    Block(module::Block<'a>),
    Empty,
}

#[derive(Default)]
pub(crate) struct Context<'a> {
    pub file: module::File<'a>,
    pub scope: Scope<'a>,
    pub codemap: CodeMap,
    pub diagnostics: Vec<Diagnostic>,
    pub success: bool,
}

#[derive(Default)]
pub(crate) struct Scope<'a> {
    pub parent: Option<Rc<Scope<'a>>>,
    pub builtins: HashMap<Cow<'a, str>, Builtin>,
    // TODO: Type variables
    pub types: HashMap<Cow<'a, str>, typecheck::Type>,
    pub attributes: HashMap<Cow<'a, str>, Attribute>,
    pub templates: HashMap<Cow<'a, str>, Template>,
    pub operators: HashMap<Cow<'a, str>, Operator>,
    pub precedences: HashMap<Cow<'a, str>, Precedence>,
    pub variables: HashMap<Cow<'a, str>, Variable<'a>>,
    pub relations: typecheck::RelationGraph,
}

pub(crate) type Builtin =
    for<'a> fn(input: &Compiled<'a>, context: &mut Context<'a>) -> Option<Compiled<'a>>;

pub(crate) struct Attribute {
    // TODO
}

pub(crate) struct Template {
    // TODO
}

pub(crate) struct Operator {
    // TODO
}

pub(crate) struct Precedence {
    // TODO
}

pub(crate) enum Variable<'a> {
    Constant(Constant<'a>),
    Alias(Cow<'a, str>, Rc<Scope<'a>>),
}
