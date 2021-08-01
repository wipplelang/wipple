use std::{borrow::Cow, collections::HashMap, rc::Rc};
use wipple_bytecode::binary;

pub(crate) trait Expression {
    fn compile<'a>(&'a self, context: &mut Context<'a>) -> Option<Compiled<'a>>;
}

pub(crate) enum Compiled<'a> {
    Trait(Trait),
    Attribute(Attribute),
    Template(Template),
    Operator(Operator),
    Precedence(Precedence),
    Constant(Constant<'a>),
    Code(binary::Block),
    Empty,
}

pub(crate) enum Constant<'a> {
    Number(f64),
    Text(Cow<'a, str>),
}

pub(crate) struct Context<'a> {
    pub binary: Binary<'a>,
    pub scope: Scope<'a>,
}

pub(crate) struct Binary<'a> {
    pub constants: Vec<Constant<'a>>,

    /// Multiple (block) entrypoints will be compiled down into a block
    /// executing all of the entrypoints in order
    pub entrypoints: Vec<binary::Index>,
}

pub(crate) struct Scope<'a> {
    pub parent: Option<Rc<Scope<'a>>>,
    pub traits: HashMap<Cow<'a, str>, Trait>,
    pub attributes: HashMap<Cow<'a, str>, Attribute>,
    pub templates: HashMap<Cow<'a, str>, Template>,
    pub operators: HashMap<Cow<'a, str>, Operator>,
    pub precedences: HashMap<Cow<'a, str>, Precedence>,
    pub variables: HashMap<Cow<'a, str>, Compiled<'a>>,
    pub relations: RelationGraph,
}

pub(crate) enum Variable {
    Value(Value),
    Block(binary::Block),
}

pub(crate) struct Value {
    pub index: binary::Index,
    pub r#trait: Trait,
    pub info: DiagnosticInfo,
}

pub(crate) struct Trait {
    pub id: Id,
    pub inner: Option<Box<Trait>>,
    pub info: DiagnosticInfo,
}

pub(crate) struct Id {
    // TODO
}

pub(crate) struct DiagnosticInfo {
    // TODO
}

pub(crate) struct Operator {
    // TODO
}

pub(crate) struct Precedence {
    // TODO
}

pub(crate) struct Template {
    // TODO
}

pub(crate) struct Attribute {
    // TODO
}

pub(crate) type Builtin =
    for<'a> fn(input: &Compiled<'a>, context: &mut Context<'a>) -> Option<Compiled<'a>>;

pub(crate) struct RelationGraph {
    // TODO
}

pub(crate) struct Relation {
    pub from: Trait,
    pub to: Trait,
    pub derive: Rc<dyn Fn(Value, &mut Context) -> Option<Value>>,
}
