use crate::*;
use codemap_diagnostic::Diagnostic;
use std::{borrow::Cow, collections::HashMap, rc::Rc};
use wipple_bytecode::{constant::Constant, module};

pub trait Expression {
    fn compile<'a>(&'a self, context: &mut Context<'a>) -> Option<Compiled<'a>>;
}

pub enum Compiled<'a> {
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
pub struct Context<'a> {
    pub file: module::File<'a>,
    pub scope: Scope<'a>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Default)]
pub struct Scope<'a> {
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

pub type Builtin =
    for<'a> fn(input: &Compiled<'a>, context: &mut Context<'a>) -> Option<Compiled<'a>>;

pub struct Attribute {
    // TODO
}

pub struct Template {
    // TODO
}

pub struct Operator {
    // TODO
}

pub struct Precedence {
    // TODO
}

pub enum Variable<'a> {
    Constant(Constant<'a>),
    Alias(Cow<'a, str>, Rc<Scope<'a>>),
}
