#![allow(clippy::too_many_arguments)]

use super::TraceOptions;
use crate::{
    codegen::{
        CodegenError, Options, imports::import_is_wasm_instruction, ir,
        types::ir_named_type_representation,
    },
    db::{Db, Node},
    span::Span,
    visit::IsMutated,
};
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Write as _},
    mem,
};

pub fn write_to_string(
    db: &Db,
    program: &ir::Program,
    options: Options<'_>,
) -> Result<String, CodegenError> {
    let mut script = String::new();
    Backend::new(&mut script, options).write(db, program)?;
    Ok(script)
}

pub struct Backend<'a> {
    output: &'a mut (dyn fmt::Write + 'a),
    options: Options<'a>,
    types: Vec<Ty<'a>>,
    functions: Vec<Func<'a>>,
    impls: BTreeMap<
        (Option<&'a ir::DefinitionKey>, Node),
        (&'a ir::Value, &'a BTreeMap<Node, ir::Type>),
    >,
    strings: Vec<(usize, String)>,
    locals: Vec<Local>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Import<'a> {
    name: &'a str,
    inputs: Vec<Result<&'a ir::Type, &'static str>>,
    output: Option<Result<&'a ir::Type, &'static str>>,
}

impl Import<'static> {
    fn trace() -> Self {
        Import {
            name: "trace",
            inputs: vec![Err("externref")],
            output: None,
        }
    }

    fn make_string() -> Self {
        Import {
            name: "make-string",
            inputs: vec![Err("i32"), Err("i32")],
            output: Some(Err("externref")),
        }
    }

    fn string_equality() -> Self {
        Import {
            name: "string-equality",
            inputs: vec![Err("externref"), Err("externref")],
            output: Some(Err("i32")),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Ty<'a> {
    Number,
    Box,
    Env(Option<&'a ir::DefinitionKey>, Node),
    Nominal(Cow<'a, ir::Type>),
    Variant(&'a ir::Type, usize),
    Impl(Cow<'a, ir::Type>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Local {
    Node(Node),
    Env,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Func<'a> {
    Import(Import<'a>),
    Declared(DeclaredFunc<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum DeclaredFunc<'a> {
    Definition(&'a ir::DefinitionKey),
    Closure {
        definition: Option<&'a ir::DefinitionKey>,
        node: Node,
    },
}

impl<'a> From<&'a ir::DefinitionKey> for Func<'a> {
    fn from(definition: &'a ir::DefinitionKey) -> Self {
        Func::Declared(DeclaredFunc::Definition(definition))
    }
}

struct Writer {
    inner: String,
    line: usize,
    column: usize,
}

impl Writer {
    pub fn new() -> Self {
        Writer {
            inner: String::new(),
            line: 0,
            column: 0,
        }
    }
}

impl<'a> Backend<'a> {
    pub fn new(w: &'a mut (dyn fmt::Write + 'a), options: Options<'a>) -> Self {
        let mut types = Vec::new();
        offset(&mut types, Ty::Number);
        offset(&mut types, Ty::Box);

        let mut functions = Vec::new();
        offset(&mut functions, Func::Import(Import::trace()));
        offset(&mut functions, Func::Import(Import::make_string()));
        offset(&mut functions, Func::Import(Import::string_equality()));

        Backend {
            output: w,
            options,
            types,
            functions,
            impls: Default::default(),
            strings: Default::default(),
            locals: Default::default(),
        }
    }

    pub fn write(mut self, db: &Db, program: &'a ir::Program) -> Result<(), CodegenError> {
        let mut decls = Writer::new();
        let mut funcs = Writer::new();
        let mut main = Writer::new();

        writeln!(decls, "(module")?;

        for definition in [&program.top_level]
            .into_iter()
            .chain(program.definitions.values())
        {
            for import in &definition.imports {
                let import = Import {
                    name: &import.name,
                    inputs: import.inputs.iter().map(Ok).collect(),
                    output: Some(Ok(&import.output)),
                };

                self.import(import.clone());
            }
        }

        writeln!(main, "(func (export \"main\")")?;
        self.write_definition(db, &mut main, None, &program.top_level)?;
        writeln!(main, "(return)")?;
        writeln!(main, ")")?;

        let mut visited_functions = BTreeSet::new();
        loop {
            let mut progress = false;
            for (index, func) in self.functions.clone().into_iter().enumerate() {
                if !visited_functions.insert(func.clone()) {
                    continue;
                }

                match func {
                    Func::Import(import) => {
                        writeln!(decls, "(elem declare func {index})")?;

                        write!(funcs, "(func (import \"runtime\" {:?})", import.name)?;

                        for ty in import.inputs {
                            let ty = match ty {
                                Ok(ty) => self.structural_ty(Cow::Borrowed(ty)),
                                Err(s) => s.to_string(),
                            };

                            write!(funcs, " (param {ty})")?;
                        }

                        if let Some(ty) = import.output {
                            let ty = match ty {
                                Ok(ty) => self.structural_ty(Cow::Borrowed(ty)),
                                Err(s) => s.to_string(),
                            };

                            write!(funcs, " (result {ty})")?;
                        }

                        writeln!(funcs, ")")?;
                    }
                    Func::Declared(func) => {
                        self.write_function(db, &mut decls, &mut funcs, index, func, program)?;
                    }
                }

                progress = true;
            }

            if !progress {
                break;
            }
        }

        writeln!(decls, "(rec")?;

        let mut visited_tys = BTreeSet::new();
        loop {
            let mut progress = false;
            for ty in self.types.clone() {
                if !visited_tys.insert(ty.clone()) {
                    continue;
                }

                self.write_type(db, &mut decls, ty)?;
                progress = true;
            }

            if !progress {
                break;
            }
        }

        writeln!(decls, ")")?;

        self.write_memory(&mut funcs)?;

        // TODO
        // let line_offset = imports.line + decls.line + type_decls.line;

        writeln!(self.output, "{}{}{})", decls.inner, funcs.inner, main.inner)?;

        Ok(())
    }

    fn write_definition(
        &mut self,
        db: &Db,
        w: &mut Writer,
        key: Option<&'a ir::DefinitionKey>,
        definition: &'a ir::Definition,
    ) -> Result<(), CodegenError> {
        let locals = mem::take(&mut self.locals);

        self.write_locals(db, w, &definition.instructions, &definition.types, &|_| {
            true
        })?;

        self.write_instructions(w, &definition.instructions, key, &definition.types)?;

        self.locals = locals;

        Ok(())
    }

    fn write_locals(
        &mut self,
        db: &Db,
        w: &mut Writer,
        instructions: &'a [ir::Instruction],
        types: &'a BTreeMap<Node, ir::Type>,
        declare: &dyn Fn(Node) -> bool,
    ) -> Result<(), CodegenError> {
        let mut nodes = BTreeSet::new();
        for instruction in instructions {
            instruction.clone().for_each_node(false, &mut |node| {
                nodes.insert(*node);
            });
        }

        for node in nodes {
            if declare(node) {
                offset(&mut self.locals, Local::Node(node));

                write!(w, "(local")?;

                if db.get::<IsMutated>(node).is_some() {
                    write!(w, " (ref null {})", offset(&mut self.types, Ty::Box))?;
                } else {
                    let ty = self.ty(node, types)?;
                    write!(w, " {}", self.structural_ty(Cow::Borrowed(ty)))?;
                }

                writeln!(w, ")")?;
            }
        }

        Ok(())
    }

    fn write_instructions(
        &mut self,
        w: &mut Writer,
        instructions: &'a [ir::Instruction],
        definition: Option<&'a ir::DefinitionKey>,
        types: &'a BTreeMap<Node, ir::Type>,
    ) -> Result<(), CodegenError> {
        for instruction in instructions {
            match instruction {
                ir::Instruction::If {
                    node,
                    branches,
                    else_branch,
                } => {
                    for (conditions, instructions, then_node) in branches {
                        write!(w, "(if ")?;
                        self.write_conditions(w, conditions, definition, types)?;
                        writeln!(w, " (then")?;
                        self.write_instructions(w, instructions, definition, types)?;
                        if let Some(node) = node
                            && let Some(then_node) = then_node
                        {
                            writeln!(
                                w,
                                "(local.set {} (local.get {}))",
                                offset(&mut self.locals, Local::Node(*node)),
                                offset(&mut self.locals, Local::Node(*then_node)),
                            )?;
                        }
                        write!(w, ") (else")?;
                    }

                    if let Some((instructions, else_node)) = else_branch {
                        self.write_instructions(w, instructions, definition, types)?;
                        if let Some(node) = node
                            && let Some(else_node) = else_node
                        {
                            writeln!(
                                w,
                                "(local.set {} (local.get {}))",
                                offset(&mut self.locals, Local::Node(*node)),
                                offset(&mut self.locals, Local::Node(*else_node)),
                            )?;
                        }
                    } else {
                        write!(w, "(unreachable)")?;
                    }

                    for _ in branches {
                        writeln!(w, "))")?;
                    }
                }
                ir::Instruction::Return { value } => {
                    writeln!(
                        w,
                        "(return (local.get {}))",
                        offset(&mut self.locals, Local::Node(*value))
                    )?;
                }
                ir::Instruction::ReturnCall { function, inputs } => {
                    self.write_call(w, *function, inputs, true, types)?;
                }
                ir::Instruction::Value { node, value } => {
                    write!(
                        w,
                        "(local.set {} ",
                        offset(&mut self.locals, Local::Node(*node))
                    )?;
                    self.write_value(w, Some(*node), value, definition, types)?;
                    writeln!(w, ")")?;
                }
                ir::Instruction::Trace { span } => {
                    if self.can_trace(span) {
                        let index = self.import(Import::trace());

                        let string = self.string(
                            &serde_json::json!({
                                "path": span.path,
                                "start": span.start,
                                "end": span.end,
                            })
                            .to_string(),
                        )?;

                        writeln!(w, "(call {index} {string})")?;
                    }
                }
            }
        }

        Ok(())
    }

    fn write_value(
        &mut self,
        w: &mut Writer,
        node: Option<Node>,
        value: &'a ir::Value,
        definition: Option<&'a ir::DefinitionKey>,
        types: &'a BTreeMap<Node, ir::Type>,
    ) -> Result<(), CodegenError> {
        match value {
            ir::Value::Bound(node) => {
                return Err(anyhow::format_err!("bound {node:?} not resolved"));
            }
            ir::Value::Call { function, inputs } => {
                self.write_call(w, *function, inputs, false, types)?;
            }
            ir::Value::Constant(key) => {
                write!(w, "(call {})", offset(&mut self.functions, Func::from(key)))?;
            }
            ir::Value::Function { captures, .. } => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;

                let ty = self.ty(node, types)?;

                let env_ty = offset(&mut self.types, Ty::Env(definition, node));

                let index = offset(
                    &mut self.functions,
                    Func::Declared(DeclaredFunc::Closure { definition, node }),
                );

                self.impls.insert((definition, node), (value, types));

                write!(
                    w,
                    "(struct.new {}",
                    offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty)))
                )?;
                write!(w, "(ref.func {index})")?;
                write!(w, " (struct.new {env_ty}")?;
                for capture in captures {
                    write!(
                        w,
                        "(local.get {})",
                        offset(&mut self.locals, Local::Node(*capture))
                    )?;
                }
                write!(w, "))")?;
            }
            ir::Value::Field {
                input, field_index, ..
            } => {
                let ty = self.ty(*input, types)?;
                write!(
                    w,
                    "(struct.get {} {} (local.get {}))",
                    offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty))),
                    field_index,
                    offset(&mut self.locals, Local::Node(*input))
                )?;
            }
            ir::Value::Tuple(elements) => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                let ty = self.ty(node, types)?;
                write!(
                    w,
                    "(struct.new {}",
                    offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty)))
                )?;
                for element in elements {
                    write!(
                        w,
                        "(local.get {})",
                        offset(&mut self.locals, Local::Node(*element))
                    )?;
                }
                write!(w, ")")?;
            }
            ir::Value::Marker => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                let ty = self.ty(node, types)?;
                write!(
                    w,
                    "(struct.new {})",
                    offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty)))
                )?;
            }
            ir::Value::MutableVariable(variable) => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                let ty = self.ty(node, types)?;

                write!(
                    w,
                    "(struct.get {} 0 (local.get {}))",
                    offset(&mut self.types, Ty::Box),
                    offset(&mut self.locals, Local::Node(*variable))
                )?;

                if let (Some(from), _) = self.conversions(ty) {
                    write!(w, "{from}")?;
                }
            }
            ir::Value::Number(number) => {
                write!(w, "(f64.const {number})")?;
            }
            ir::Value::Runtime { name, inputs } => {
                if import_is_wasm_instruction(name) {
                    write!(w, "({name}")?;

                    for input in inputs {
                        write!(
                            w,
                            " (local.get {})",
                            offset(&mut self.locals, Local::Node(*input))
                        )?;
                    }

                    write!(w, ")")?;
                } else {
                    let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;

                    let index = self.import_for(name, inputs, node, types)?;

                    write!(w, "(call {index}")?;
                    for input in inputs {
                        write!(
                            w,
                            "(local.get {})",
                            offset(&mut self.locals, Local::Node(*input))
                        )?;
                    }
                    write!(w, ")")?;
                }
            }
            ir::Value::String(string) => {
                let string = self.string(string)?;
                write!(w, "{string}")?;
            }
            ir::Value::Structure(fields) => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                let ty = self.ty(node, types)?;
                write!(
                    w,
                    "(struct.new {}",
                    offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty)))
                )?;
                for (_, value) in fields {
                    write!(
                        w,
                        " (local.get {})",
                        offset(&mut self.locals, Local::Node(*value))
                    )?;
                }
                write!(w, ")")?;
            }
            ir::Value::TupleElement { input, index } => {
                let ty = self.ty(*input, types)?;
                write!(
                    w,
                    "(struct.get {} {} (local.get {}))",
                    offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty))),
                    index,
                    offset(&mut self.locals, Local::Node(*input))
                )?;
            }
            ir::Value::Unreachable => {
                write!(w, "(unreachable)")?;
            }
            ir::Value::Variable(node) => {
                write!(
                    w,
                    "(local.get {})",
                    offset(&mut self.locals, Local::Node(*node))
                )?;
            }
            ir::Value::Variant { index, elements } => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                let ty = self.ty(node, types)?;
                write!(
                    w,
                    "(struct.new {} ",
                    offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty)))
                )?;
                offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty)));
                write!(
                    w,
                    "(struct.new {}",
                    offset(&mut self.types, Ty::Variant(ty, *index))
                )?;
                for element in elements {
                    write!(
                        w,
                        " (local.get {})",
                        offset(&mut self.locals, Local::Node(*element))
                    )?;
                }
                write!(w, ") (i32.const {index}))")?;
            }
            ir::Value::VariantElement {
                input,
                variant,
                index,
            } => {
                let ty = self.ty(*input, types)?;
                offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty)));
                let variant_ty = offset(&mut self.types, Ty::Variant(ty, *variant));

                write!(
                    w,
                    "(struct.get {} {} (ref.cast (ref null {}) (struct.get {} 0 (local.get {}))))",
                    variant_ty,
                    index,
                    variant_ty,
                    offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty))),
                    offset(&mut self.locals, Local::Node(*input))
                )?;
            }
        }

        Ok(())
    }

    fn write_call(
        &mut self,
        w: &mut Writer,
        function: Node,
        inputs: &[Node],
        tail: bool,
        types: &'a BTreeMap<Node, ir::Type>,
    ) -> Result<(), anyhow::Error> {
        let op = if tail { "return_call_ref" } else { "call_ref" };
        let ty = self.ty(function, types)?;
        write!(
            w,
            "({} {}",
            op,
            offset(&mut self.types, Ty::Impl(Cow::Borrowed(ty)))
        )?;

        write!(
            w,
            "(struct.get {} 1 (local.get {}))",
            offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty))),
            offset(&mut self.locals, Local::Node(function))
        )?;

        for input in inputs {
            write!(
                w,
                "(local.get {})",
                offset(&mut self.locals, Local::Node(*input))
            )?;
        }

        write!(
            w,
            "(struct.get {} 0 (local.get {})))",
            offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty))),
            offset(&mut self.locals, Local::Node(function)),
        )?;

        Ok(())
    }

    fn write_conditions(
        &mut self,
        w: &mut Writer,
        conditions: &'a [ir::Condition],
        definition: Option<&'a ir::DefinitionKey>,
        types: &'a BTreeMap<Node, ir::Type>,
    ) -> Result<(), CodegenError> {
        if conditions.is_empty() {
            write!(w, "(i32.const 1)")?;
        } else {
            for (index, condition) in conditions.iter().enumerate() {
                write!(w, "(if (result i32) ")?;

                self.write_condition(w, condition, definition, types)?;

                write!(w, "(then ")?;

                if index + 1 == conditions.len() {
                    write!(w, "(i32.const 1)")?;
                }
            }

            for _ in conditions {
                write!(w, ") (else (i32.const 0)))")?;
            }
        }

        Ok(())
    }

    fn write_condition(
        &mut self,
        w: &mut Writer,
        condition: &'a ir::Condition,
        definition: Option<&'a ir::DefinitionKey>,
        types: &'a BTreeMap<Node, ir::Type>,
    ) -> Result<(), CodegenError> {
        match condition {
            ir::Condition::Or(branches) => {
                if branches.is_empty() {
                    write!(w, "(i32.const 1)")?;
                } else {
                    for (index, conditions) in branches.iter().enumerate() {
                        write!(w, "(if (result i32) ")?;
                        self.write_conditions(w, conditions, definition, types)?;
                        write!(w, "(then (i32.const 1)) (else ")?;

                        if index + 1 == branches.len() {
                            write!(w, "(i32.const 0)")?;
                        }
                    }

                    for _ in branches {
                        write!(w, "))")?;
                    }
                }
            }
            ir::Condition::EqualToNumber { input, value } => {
                write!(
                    w,
                    "(f64.eq (local.get {}) (f64.const {}))",
                    offset(&mut self.locals, Local::Node(*input)),
                    value
                )?;
            }
            ir::Condition::EqualToString { input, value } => {
                let index = self.import(Import::string_equality());

                let string = self.string(value)?;
                write!(
                    w,
                    "(call {index} (local.get {}) {})",
                    offset(&mut self.locals, Local::Node(*input)),
                    string
                )?;
            }
            ir::Condition::EqualToVariant { input, variant } => {
                let ty = self.ty(*input, types)?;
                write!(
                    w,
                    "(i32.eq (struct.get {} 1 (local.get {})) (i32.const {}))",
                    offset(&mut self.types, Ty::Nominal(Cow::Borrowed(ty))),
                    offset(&mut self.locals, Local::Node(*input)),
                    variant
                )?;
            }
            ir::Condition::Initialize {
                variable,
                node,
                value,
                mutable,
            } => {
                write!(
                    w,
                    "(block (result i32) (local.set {} ",
                    offset(&mut self.locals, Local::Node(*variable))
                )?;
                let mut to = None;
                if *mutable {
                    write!(w, "(struct.new {} ", offset(&mut self.types, Ty::Box))?;

                    let node = node
                        .as_ref()
                        .ok_or_else(|| anyhow::format_err!("missing node"))?;

                    let value_ty = self.ty(*node, types)?;

                    (_, to) = self.conversions(value_ty);
                }

                self.write_value(w, *node, value, definition, types)?;

                if let Some(to) = to {
                    write!(w, "{to}")?;
                }

                if *mutable {
                    write!(w, ")")?;
                }
                write!(w, ") (i32.const 1))")?;
            }
            ir::Condition::Mutate { input, variable } => {
                let ty = self.ty(*variable, types)?;

                write!(
                    w,
                    "(block (result i32) (struct.set {} 0 (local.get {}) ",
                    offset(&mut self.types, Ty::Box),
                    offset(&mut self.locals, Local::Node(*variable))
                )?;
                write!(
                    w,
                    "(local.get {})",
                    offset(&mut self.locals, Local::Node(*input))
                )?;
                if let (_, Some(to)) = self.conversions(ty) {
                    write!(w, "{to}")?;
                }
                write!(w, ") (i32.const 1))")?;
            }
        }

        Ok(())
    }

    fn write_function(
        &mut self,
        db: &Db,
        decls: &mut Writer,
        funcs: &mut Writer,
        index: usize,
        func: DeclaredFunc<'a>,
        program: &'a ir::Program,
    ) -> Result<(), CodegenError> {
        match func {
            DeclaredFunc::Definition(key) => {
                let definition = program
                    .definitions
                    .get(key)
                    .ok_or_else(|| anyhow::format_err!("missing definition: {key:?}"))?;

                let ty = definition
                    .ty
                    .as_ref()
                    .ok_or_else(|| anyhow::format_err!("unresolved definition type: {key:?}"))?;

                offset(&mut self.functions, Func::from(key));

                writeln!(
                    funcs,
                    "(func (result {})",
                    self.structural_ty(Cow::Borrowed(ty))
                )?;

                self.write_definition(db, funcs, Some(key), definition)?;

                writeln!(funcs, ")")?;
            }
            DeclaredFunc::Closure { definition, node } => {
                writeln!(decls, "(elem declare func {index})")?;

                let (value, types) = self
                    .impls
                    .get(&(definition, node))
                    .copied()
                    .ok_or_else(|| anyhow::format_err!("missing implementation"))?;

                let ir::Value::Function {
                    inputs,
                    captures,
                    instructions,
                } = value
                else {
                    unreachable!()
                };

                let ty = self.ty(node, types)?;

                let ir::Type::Function(input_types, output_type) = &ty else {
                    return Err(anyhow::format_err!("{node:?} is not a function"));
                };

                let locals = mem::take(&mut self.locals);

                offset(&mut self.locals, Local::Env);

                for input in inputs {
                    offset(&mut self.locals, Local::Node(*input));
                }

                writeln!(
                    funcs,
                    "(func (type {}) (param anyref)",
                    offset(&mut self.types, Ty::Impl(Cow::Borrowed(ty)))
                )?;

                for ty in input_types {
                    writeln!(funcs, "(param {})", self.structural_ty(Cow::Borrowed(ty)))?;
                }

                writeln!(
                    funcs,
                    "(result {})",
                    self.structural_ty(Cow::Borrowed(output_type))
                )?;

                for &capture in captures {
                    offset(&mut self.locals, Local::Node(capture));

                    write!(funcs, "(local ")?;
                    if db.get::<IsMutated>(capture).is_some() {
                        let box_ref = offset(&mut self.types, Ty::Box);
                        write!(funcs, "(ref null {box_ref})")?;
                    } else {
                        let ty = self.ty(capture, types)?;
                        write!(funcs, "{}", self.structural_ty(Cow::Borrowed(ty)))?;
                    }
                    writeln!(funcs, ")")?;
                }

                self.write_locals(db, funcs, instructions, types, &|child| {
                    !inputs.contains(&child) && !captures.contains(&child)
                })?;

                let env_ty = offset(&mut self.types, Ty::Env(definition, node));
                for (index, capture) in captures.iter().enumerate() {
                    writeln!(
                        funcs,
                        "(local.set {} (struct.get {} {} (ref.cast (ref null {}) (local.get {}))))",
                        offset(&mut self.locals, Local::Node(*capture)),
                        env_ty,
                        index,
                        env_ty,
                        offset(&mut self.locals, Local::Env)
                    )?;
                }

                self.write_instructions(funcs, instructions, definition, types)?;

                writeln!(funcs, ")")?;

                self.locals = locals;
            }
        }

        Ok(())
    }

    fn write_type(&mut self, db: &Db, w: &mut Writer, ty: Ty<'a>) -> Result<(), CodegenError> {
        match ty {
            Ty::Number => {
                writeln!(w, "(type (struct (field f64)))")?;
            }
            Ty::Box => {
                writeln!(w, "(type (struct (field (mut anyref))))")?;
            }
            Ty::Env(definition, node) => {
                let (value, types) = *self
                    .impls
                    .get(&(definition, node))
                    .ok_or_else(|| anyhow::format_err!("missing implementation"))?;

                let ir::Value::Function { captures, .. } = value else {
                    unreachable!()
                };

                write!(w, "(type (struct")?;
                for &capture in captures {
                    if db.get::<IsMutated>(capture).is_some() {
                        write!(w, "(field (ref null {}))", offset(&mut self.types, Ty::Box))?;
                    } else {
                        let ty = self.ty(capture, types)?;
                        write!(w, "(field {})", self.structural_ty(Cow::Borrowed(ty)))?;
                    }
                }
                writeln!(w, "))")?;
            }
            Ty::Nominal(ty) => match ty.into_owned() {
                ir::Type::Named {
                    definition,
                    parameters,
                    ..
                } => {
                    let representation =
                        ir_named_type_representation(db, definition, parameters)
                            .ok_or_else(|| anyhow::format_err!("missing representation"))?;

                    match representation {
                        ir::TypeRepresentation::Intrinsic => {}
                        ir::TypeRepresentation::Marker => {
                            writeln!(w, "(type (struct))")?;
                        }
                        ir::TypeRepresentation::Structure(fields) => {
                            write!(w, "(type (struct")?;
                            for field in fields {
                                write!(w, " (field {})", self.structural_ty(Cow::Owned(field)))?;
                            }
                            writeln!(w, "))")?;
                        }
                        ir::TypeRepresentation::Enumeration(_) => {
                            writeln!(w, "(type (struct (field anyref) (field i32)))")?;
                        }
                    }
                }
                ir::Type::Tuple(elements) => {
                    write!(w, "(type (struct")?;
                    for element in elements {
                        write!(w, " (field {})", self.structural_ty(Cow::Owned(element)))?;
                    }
                    writeln!(w, "))")?;
                }
                ty @ ir::Type::Function(_, _) => {
                    write!(
                        w,
                        "(type (struct (field (ref null {})) (field anyref)))",
                        offset(&mut self.types, Ty::Impl(Cow::Owned(ty))),
                    )?;
                    writeln!(w)?;
                }
                ir::Type::Parameter(node) => {
                    return Err(anyhow::format_err!("parameter {node:?} not resolved"));
                }
            },
            Ty::Variant(ty, index) => {
                let ir::Type::Named {
                    definition,
                    parameters,
                    ..
                } = ty.clone()
                else {
                    return Err(anyhow::format_err!("variant type for non-enum {ty:?}"));
                };

                let ir::TypeRepresentation::Enumeration(variants) =
                    ir_named_type_representation(db, definition, parameters)
                        .ok_or_else(|| anyhow::format_err!("no representation for {ty:?}"))?
                else {
                    return Err(anyhow::format_err!("variant type for non-enum {ty:?}"));
                };

                let elements = variants
                    .get(index)
                    .ok_or_else(|| anyhow::format_err!("missing variant {index} for {ty:?}"))?
                    .clone();

                write!(w, "(type (struct")?;
                for element in elements {
                    write!(w, " (field {})", self.structural_ty(Cow::Owned(element)))?;
                }
                writeln!(w, "))")?;
            }
            Ty::Impl(ty) => {
                let ir::Type::Function(inputs, output) = ty.into_owned() else {
                    return Err(anyhow::format_err!("expected a function"));
                };

                write!(w, "(type (func")?;
                write!(w, " (param anyref)")?;
                for input in inputs {
                    write!(w, " (param {})", self.structural_ty(Cow::Owned(input)))?;
                }
                writeln!(w, " (result {})))", self.structural_ty(Cow::Owned(*output)))?;
            }
        }

        Ok(())
    }

    fn can_trace(&self, span: &Span) -> bool {
        match self.options.trace {
            TraceOptions::None => false,
            TraceOptions::All => true,
            TraceOptions::Files(files) => files.contains(&span.path.as_str()),
        }
    }

    fn ty(
        &mut self,
        node: Node,
        types: &'a BTreeMap<Node, ir::Type>,
    ) -> Result<&'a ir::Type, CodegenError> {
        types
            .get(&node)
            .ok_or_else(|| anyhow::format_err!("missing type for node {node:?}"))
    }

    fn structural_ty(&mut self, ty: Cow<'a, ir::Type>) -> String {
        match ty.mangle_structural() {
            Some(_) => format!("(ref null {})", offset(&mut self.types, Ty::Nominal(ty))),
            None => match ty.as_ref() {
                ir::Type::Named {
                    intrinsic,
                    representation,
                    ..
                } => {
                    if let Some(representation) = representation {
                        representation.to_string()
                    } else if *intrinsic {
                        String::from("externref")
                    } else {
                        unreachable!()
                    }
                }
                _ => unreachable!(),
            },
        }
    }

    fn conversions(&mut self, ty: &'a ir::Type) -> (Option<String>, Option<String>) {
        if let ir::Type::Named {
            intrinsic,
            representation,
            ..
        } = ty
        {
            if let Some("f64") = representation.as_deref() {
                let number = offset(&mut self.types, Ty::Number);
                return (
                    Some(format!(
                        "(ref.cast (ref null {number})) (struct.get {number} 0)"
                    )),
                    Some(format!("(struct.new {number})")),
                );
            } else if *intrinsic {
                return (
                    Some(String::from("(extern.convert_any)")),
                    Some(String::from("(any.convert_extern)")),
                );
            }
        }

        (
            Some(format!(
                "(ref.cast {})",
                self.structural_ty(Cow::Borrowed(ty))
            )),
            None,
        )
    }

    fn import(&mut self, import: Import<'a>) -> usize {
        offset(&mut self.functions, Func::Import(import))
    }

    fn import_for(
        &mut self,
        name: &str,
        inputs: &[Node],
        output: Node,
        types: &'a BTreeMap<Node, ir::Type>,
    ) -> Result<usize, CodegenError> {
        let inputs = inputs
            .iter()
            .map(|input| self.ty(*input, types).map(Ok))
            .collect::<Result<Vec<_>, _>>()?;

        let output = Ok(self.ty(output, types)?);

        self.functions
            .iter()
            .position(|func| {
                let Func::Import(import) = func else {
                    return false;
                };

                import.name == name && import.inputs == inputs && import.output == Some(output)
            })
            .ok_or_else(|| anyhow::format_err!("missing import"))
    }

    fn string(&mut self, string: &str) -> Result<String, CodegenError> {
        let offset = if let Some(&(offset, _)) = self.strings.iter().find(|(_, s)| string == *s) {
            offset
        } else {
            let offset = self
                .strings
                .last()
                .map_or(0, |(offset, s)| *offset + s.len());

            self.strings.push((offset, string.to_string()));

            offset
        };

        let index = self.import(Import::make_string());

        Ok(format!(
            "(call {index} (i32.const {}) (i32.const {}))",
            offset,
            string.len()
        ))
    }

    fn write_memory(&self, w: &mut Writer) -> Result<(), CodegenError> {
        const PAGE_SIZE: usize = 64 * 1024;

        let pages = self
            .strings
            .last()
            .map_or(1, |(offset, s)| (*offset + s.len()).div_ceil(PAGE_SIZE));

        writeln!(w, "(memory (export \"memory\") {pages})")?;

        for (offset, s) in &self.strings {
            writeln!(w, "(data (i32.const {offset}) {s:?})")?;
        }

        Ok(())
    }
}

impl fmt::Write for Writer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }

        self.inner.write_str(s)
    }
}

fn offset<T: Eq>(map: &mut Vec<T>, value: T) -> usize {
    if let Some(index) = map.iter().position(|x| *x == value) {
        index
    } else {
        let index = map.len();
        map.push(value);
        index
    }
}
