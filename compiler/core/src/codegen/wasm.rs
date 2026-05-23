#![allow(clippy::too_many_arguments)]

use crate::{
    codegen::{CodegenError, Options, ir, mangle::Mangle, types::ir_named_type_representation},
    db::{Db, Node},
    span::Span,
    visit::IsMutated,
};
use std::{
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
    types: BTreeSet<ir::Type>,
    functions: BTreeMap<
        Option<&'a ir::DefinitionKey>,
        BTreeMap<Node, (&'a ir::Value, &'a BTreeMap<Node, ir::Type>)>,
    >,
    strings: Vec<(usize, String)>,
    runtime_functions: BTreeMap<
        String,
        BTreeSet<(
            String,
            Vec<Result<ir::Type, &'static str>>,
            Vec<Result<ir::Type, &'static str>>,
        )>,
    >,
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
        Backend {
            output: w,
            options,
            types: Default::default(),
            functions: Default::default(),
            strings: Default::default(),
            runtime_functions: Default::default(),
        }
    }

    pub fn write(mut self, db: &Db, program: &'a ir::Program) -> Result<(), CodegenError> {
        let mut imports = Writer::new();
        let mut decls = Writer::new();
        let mut funcs = Writer::new();

        writeln!(imports, "(module")?;
        writeln!(decls, "(rec")?;

        writeln!(funcs, "(func $main (export \"main\")")?;
        self.write_definition(db, &mut funcs, None, &program.top_level)?;
        writeln!(funcs, "(return)")?;
        writeln!(funcs, ")")?;

        for (key, definition) in &program.definitions {
            let ty = definition
                .ty
                .as_ref()
                .ok_or_else(|| anyhow::format_err!("unresolved definition type: {key:?}"))?;

            writeln!(
                funcs,
                "(func ${} (result {})",
                key.mangle(),
                self.structural_ty(ty)
            )?;

            self.write_definition(db, &mut funcs, Some(key), definition)?;

            writeln!(funcs, ")")?;
        }

        while !self.functions.is_empty() {
            for (definition, functions) in mem::take(&mut self.functions) {
                for (node, (value, types)) in functions {
                    self.write_function(
                        db,
                        &mut imports,
                        &mut decls,
                        &mut funcs,
                        node,
                        value,
                        definition,
                        types,
                    )?;
                }
            }
        }

        writeln!(decls, "(type $number (struct (field f64)))")?;
        writeln!(decls, "(type $box (struct (field (mut anyref))))")?;

        for ty in mem::take(&mut self.types) {
            self.write_type(db, &mut decls, ty)?;
        }
        writeln!(decls, ")")?;

        self.write_memory(&mut decls)?;

        for (name, runtime_functions) in mem::take(&mut self.runtime_functions) {
            for (mangled, inputs, outputs) in runtime_functions {
                write!(imports, "(func ${mangled} (import \"runtime\" \"{name}\")")?;
                for input in inputs {
                    let input = match input {
                        Ok(ty) => &self.structural_ty(&ty),
                        Err(s) => s,
                    };

                    write!(imports, " (param {input})")?;
                }
                for output in outputs {
                    let output = match output {
                        Ok(ty) => &self.structural_ty(&ty),
                        Err(s) => s,
                    };

                    write!(imports, " (result {output})")?;
                }
                writeln!(imports, ")")?;
            }
        }

        // TODO
        // let line_offset = imports.line + decls.line + type_decls.line;

        writeln!(
            self.output,
            "{}{}{})",
            imports.inner, decls.inner, funcs.inner
        )?;

        Ok(())
    }

    fn write_definition(
        &mut self,
        db: &Db,
        w: &mut Writer,
        key: Option<&'a ir::DefinitionKey>,
        definition: &'a ir::Definition,
    ) -> Result<(), CodegenError> {
        self.write_locals(db, w, &definition.instructions, &definition.types, &|_| {
            true
        })?;

        self.write_instructions(w, &definition.instructions, key, &definition.types)?;

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
                write!(w, "(local ${}", node.mangle())?;

                if db.get::<IsMutated>(node).is_some() {
                    write!(w, " (ref null $box)")?;
                } else {
                    let ty = self.ty(node, types)?;
                    write!(w, " {}", self.structural_ty(&ty))?;
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
                                "(local.set ${} (local.get ${}))",
                                node.mangle(),
                                then_node.mangle()
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
                                "(local.set ${} (local.get ${}))",
                                node.mangle(),
                                else_node.mangle()
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
                    writeln!(w, "(return (local.get ${}))", value.mangle())?;
                }
                ir::Instruction::ReturnCall { function, inputs } => {
                    self.write_call(w, *function, inputs, true, types)?;
                }
                ir::Instruction::Value { node, value } => {
                    write!(w, "(local.set ${} ", node.mangle())?;
                    self.write_value(w, Some(*node), value, definition, types)?;
                    writeln!(w, ")")?;
                }
                ir::Instruction::Trace { span } => {
                    if self.can_trace(span) {
                        let name = self.runtime("trace", vec![Err("externref")], vec![])?;
                        let string = self.string(&serde_json::json!(span.start).to_string())?;
                        writeln!(w, "(call ${name} {string})")?;
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
            ir::Value::Concat { segments, trailing } => {
                for (string, node) in segments {
                    let name = self.runtime(
                        "concat",
                        vec![Err("externref"), Err("externref")],
                        vec![Err("externref")],
                    )?;

                    let string = self.string(string)?;
                    write!(w, "(call ${name} {string} ")?;
                    write!(w, "(call ${name} (local.get ${})", node.mangle())?;
                }

                let trailing = self.string(trailing)?;
                write!(w, "{trailing}")?;

                for _ in segments {
                    write!(w, "))")?;
                }
            }
            ir::Value::Constant(key) => {
                write!(w, "(call ${})", key.mangle())?;
            }
            ir::Value::Function { captures, .. } => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;

                let ty = self.ty(node, types)?;

                self.functions
                    .entry(definition)
                    .or_default()
                    .insert(node, (value, types));

                let mut env_ty = String::from("$env");
                if let Some(definition) = definition {
                    write!(env_ty, "{}", definition.mangle())?;
                }
                write!(env_ty, "{}", node.mangle())?;

                write!(w, "(struct.new ${}", ty.mangle_nominal())?;
                write!(w, "(ref.func $")?;
                if let Some(definition) = definition {
                    write!(w, "{}", definition.mangle())?;
                }
                write!(w, "{})", node.mangle())?;
                write!(w, "(struct.new {env_ty}")?;
                for capture in captures {
                    write!(w, "(local.get ${})", capture.mangle())?;
                }
                write!(w, "))")?;
            }
            ir::Value::Field {
                input, field_index, ..
            } => {
                let ty = self.ty(*input, types)?;
                write!(
                    w,
                    "(struct.get ${} {} (local.get ${}))",
                    ty.mangle_nominal(),
                    field_index,
                    input.mangle()
                )?;
            }
            ir::Value::Tuple(elements) => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                let ty = self.ty(node, types)?;
                write!(w, "(struct.new ${}", ty.mangle_nominal())?;
                for element in elements {
                    write!(w, "(local.get ${})", element.mangle())?;
                }
                write!(w, ")")?;
            }
            ir::Value::Marker => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                let ty = self.ty(node, types)?;
                write!(w, "(struct.new ${})", ty.mangle_nominal())?;
            }
            ir::Value::MutableVariable(variable) => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                let ty = self.ty(node, types)?;

                write!(w, "(struct.get $box 0 (local.get ${}))", variable.mangle())?;

                if let (Some(from), _) = self.conversions(&ty) {
                    write!(w, "{from}")?;
                }
            }
            ir::Value::Number(number) => {
                write!(w, "(f64.const {number})")?;
            }
            ir::Value::Runtime { name, inputs } => {
                if name.contains(".") {
                    // Treat as a Wasm instruction

                    write!(w, "({name}")?;

                    for input in inputs {
                        write!(w, " (local.get ${})", input.mangle())?;
                    }

                    write!(w, ")")?;
                } else {
                    // Treat as an external function call

                    let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                    let output_ty = self.ty(node, types)?;

                    let input_tys = inputs
                        .iter()
                        .map(|node| self.ty(*node, types))
                        .collect::<Result<Vec<_>, _>>()?;

                    let maybe_output_ty = match &output_ty {
                        ir::Type::Named {
                            parameters, abi, ..
                        } if abi.as_deref() == Some("maybe") => parameters.first(),
                        _ => None,
                    };

                    let name = if let Some(maybe_output_ty) = maybe_output_ty {
                        self.runtime(
                            name,
                            input_tys.into_iter().map(Ok).collect(),
                            vec![Ok(maybe_output_ty.clone()), Err("i32")],
                        )?
                    } else {
                        self.runtime(
                            name,
                            input_tys.into_iter().map(Ok).collect(),
                            vec![Ok(output_ty.clone())],
                        )?
                    };

                    write!(w, "(call ${name}")?;
                    for input in inputs {
                        write!(w, "(local.get ${})", input.mangle())?;
                    }
                    write!(w, ")")?;

                    if maybe_output_ty.is_some() {
                        write!(
                            w,
                            "(if (param {}) (result {}) (then ",
                            self.structural_ty(maybe_output_ty.unwrap_or(&output_ty)),
                            self.structural_ty(&output_ty)
                        )?;

                        write!(
                            w,
                            "(struct.new ${} (struct.new ${}_variant1) (i32.const 1))",
                            output_ty.mangle_nominal(),
                            output_ty.mangle_nominal(),
                        )?;

                        write!(w, ") (else ")?;

                        write!(
                            w,
                            "(drop) (struct.new ${} (struct.new ${}_variant0) (i32.const 0))",
                            output_ty.mangle_nominal(),
                            output_ty.mangle_nominal(),
                        )?;

                        write!(w, "))")?;
                    }
                }
            }
            ir::Value::String(string) => {
                let string = self.string(string)?;
                write!(w, "{string}")?;
            }
            ir::Value::Structure(fields) => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                let ty = self.ty(node, types)?;
                write!(w, "(struct.new ${}", ty.mangle_nominal())?;
                for (_, value) in fields {
                    write!(w, " (local.get ${})", value.mangle())?;
                }
                write!(w, ")")?;
            }
            ir::Value::TupleElement { input, index } => {
                let ty = self.ty(*input, types)?;
                write!(
                    w,
                    "(struct.get ${} {} (local.get ${}))",
                    ty.mangle_nominal(),
                    index,
                    input.mangle()
                )?;
            }
            ir::Value::Unreachable => {
                write!(w, "(unreachable)")?;
            }
            ir::Value::Variable(node) => {
                write!(w, "(local.get ${})", node.mangle())?;
            }
            ir::Value::Variant { index, elements } => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;
                let ty = self.ty(node, types)?;
                write!(w, "(struct.new ${} ", ty.mangle_nominal())?;
                write!(w, "(struct.new ${}_variant{}", ty.mangle_nominal(), index)?;
                for element in elements {
                    write!(w, " (local.get ${})", element.mangle())?;
                }
                write!(w, ") (i32.const {index}))")?;
            }
            ir::Value::VariantElement {
                input,
                variant,
                index,
            } => {
                let ty = self.ty(*input, types)?;

                write!(
                    w,
                    "(struct.get ${}_variant{} {} (ref.cast (ref null ${}_variant{}) (struct.get ${} 0 (local.get ${}))))",
                    ty.mangle_nominal(),
                    variant,
                    index,
                    ty.mangle_nominal(),
                    variant,
                    ty.mangle_nominal(),
                    input.mangle()
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
        write!(w, "({} $impl_{}", op, ty.mangle_nominal())?;

        write!(
            w,
            "(struct.get ${} 1 (local.get ${}))",
            ty.mangle_nominal(),
            function.mangle()
        )?;

        for input in inputs {
            write!(w, "(local.get ${})", input.mangle())?;
        }

        write!(
            w,
            "(struct.get ${} 0 (local.get ${})))",
            ty.mangle_nominal(),
            function.mangle(),
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
                    "(f64.eq (local.get ${}) (f64.const {}))",
                    input.mangle(),
                    value
                )?;
            }
            ir::Condition::EqualToString { input, value } => {
                let name = self.runtime(
                    "string-equality",
                    vec![Err("externref"), Err("externref")],
                    vec![Err("i32")],
                )?;

                let string = self.string(value)?;
                write!(
                    w,
                    "(call ${name} (local.get ${}) {})",
                    input.mangle(),
                    string
                )?;
            }
            ir::Condition::EqualToVariant { input, variant } => {
                let ty = self.ty(*input, types)?;
                write!(
                    w,
                    "(i32.eq (struct.get ${} 1 (local.get ${})) (i32.const {}))",
                    ty.mangle_nominal(),
                    input.mangle(),
                    variant
                )?;
            }
            ir::Condition::Initialize {
                variable,
                node,
                value,
                mutable,
            } => {
                write!(w, "(block (result i32) (local.set ${} ", variable.mangle())?;
                let mut to = None;
                if *mutable {
                    write!(w, "(struct.new $box ")?;

                    let node = node
                        .as_ref()
                        .ok_or_else(|| anyhow::format_err!("missing node"))?;

                    let value_ty = self.ty(*node, types)?;

                    (_, to) = self.conversions(&value_ty);
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
                    "(block (result i32) (struct.set $box 0 (local.get ${}) ",
                    variable.mangle()
                )?;
                write!(w, "(local.get ${})", input.mangle())?;
                if let (_, Some(to)) = self.conversions(&ty) {
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
        imports: &mut Writer,
        decls: &mut Writer,
        funcs: &mut Writer,
        node: Node,
        value: &'a ir::Value,
        definition: Option<&'a ir::DefinitionKey>,
        types: &'a BTreeMap<Node, ir::Type>,
    ) -> Result<(), CodegenError> {
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

        let mut env_ty = String::from("$env");
        if let Some(definition) = definition {
            write!(env_ty, "{}", definition.mangle())?;
        }
        write!(env_ty, "{}", node.mangle())?;

        write!(decls, "(type {env_ty} (struct")?;
        for &capture in captures {
            if db.get::<IsMutated>(capture).is_some() {
                write!(decls, "(field (ref null $box))")?;
            } else {
                let ty = self.ty(capture, types)?;
                write!(decls, "(field {})", self.structural_ty(&ty))?;
            }
        }
        writeln!(decls, "))")?;

        write!(imports, "(elem declare func $")?;
        if let Some(definition) = definition {
            write!(imports, "{}", definition.mangle())?;
        }
        writeln!(imports, "{})", node.mangle())?;

        write!(funcs, "(func $")?;
        if let Some(definition) = definition {
            write!(funcs, "{}", definition.mangle())?;
        }
        write!(funcs, "{}", node.mangle())?;
        writeln!(
            funcs,
            "(type $impl_{}) (param $env anyref)",
            ty.mangle_nominal()
        )?;

        for (input, ty) in inputs.iter().zip(input_types.iter()) {
            writeln!(
                funcs,
                "(param ${} {})",
                input.mangle(),
                self.structural_ty(ty)
            )?;
        }

        writeln!(funcs, "(result {})", self.structural_ty(output_type))?;

        for &capture in captures {
            write!(funcs, "(local ${} ", capture.mangle())?;
            if db.get::<IsMutated>(capture).is_some() {
                write!(funcs, "(ref null $box)")?;
            } else {
                let ty = self.ty(capture, types)?;
                write!(funcs, "{}", self.structural_ty(&ty))?;
            }
            writeln!(funcs, ")")?;
        }

        self.write_locals(db, funcs, instructions, types, &|child| {
            !inputs.contains(&child) && !captures.contains(&child)
        })?;

        for (index, capture) in captures.iter().enumerate() {
            let mut env_ty = String::from("$env");
            if let Some(definition) = definition {
                write!(env_ty, "{}", definition.mangle())?;
            }
            write!(env_ty, "{}", node.mangle())?;

            writeln!(
                funcs,
                "(local.set ${} (struct.get {} {} (ref.cast (ref null {}) (local.get $env))))",
                capture.mangle(),
                env_ty,
                index,
                env_ty
            )?;
        }

        self.write_instructions(funcs, instructions, definition, types)?;

        writeln!(funcs, ")")?;

        Ok(())
    }

    fn write_type(&mut self, db: &Db, w: &mut Writer, ty: ir::Type) -> Result<(), CodegenError> {
        match &ty {
            ir::Type::Named {
                definition,
                parameters,
                ..
            } => {
                let representation = ir_named_type_representation(db, *definition, parameters)
                    .ok_or_else(|| anyhow::format_err!("no representation for {ty:?}"))?;

                match representation {
                    ir::TypeRepresentation::Intrinsic => {}
                    ir::TypeRepresentation::Marker => {
                        writeln!(w, "(type ${} (struct))", ty.mangle_nominal())?;
                    }
                    ir::TypeRepresentation::Structure(fields) => {
                        write!(w, "(type ${} (struct", ty.mangle_nominal())?;
                        for field in fields {
                            write!(w, " (field {})", self.structural_ty(&field))?;
                        }
                        writeln!(w, "))")?;
                    }
                    ir::TypeRepresentation::Enumeration(variants) => {
                        writeln!(
                            w,
                            "(type ${} (struct (field anyref) (field i32)))",
                            ty.mangle_nominal()
                        )?;

                        for (index, elements) in variants.iter().enumerate() {
                            write!(w, "(type ${}_variant{} (struct", ty.mangle_nominal(), index)?;
                            for element in elements {
                                write!(w, " (field {})", self.structural_ty(element))?;
                            }
                            writeln!(w, "))")?;
                        }
                    }
                }
            }
            ir::Type::Tuple(elements) => {
                write!(w, "(type ${} (struct", ty.mangle_nominal())?;
                for element in elements {
                    write!(w, " (field {})", self.structural_ty(element))?;
                }
                writeln!(w, "))")?;
            }
            ir::Type::Function(inputs, output) => {
                write!(
                    w,
                    "(type ${} (struct (field (ref null $impl",
                    ty.mangle_nominal(),
                )?;
                writeln!(w, "_{})) (field anyref)))", ty.mangle_nominal())?;

                write!(w, "(type $impl_{} (func", ty.mangle_nominal())?;
                write!(w, " (param anyref)")?;
                for input in inputs {
                    write!(w, " (param {})", self.structural_ty(input))?;
                }
                writeln!(w, " (result {})))", self.structural_ty(output))?;
            }
            ir::Type::Parameter(node) => {
                return Err(anyhow::format_err!("parameter {node:?} not resolved"));
            }
        }

        Ok(())
    }

    fn can_trace(&self, span: &Span) -> bool {
        self.options.trace.contains(&span.path.as_str())
    }

    fn ty(
        &mut self,
        node: Node,
        types: &BTreeMap<Node, ir::Type>,
    ) -> Result<ir::Type, CodegenError> {
        let mut ty = types
            .get(&node)
            .cloned()
            .ok_or_else(|| anyhow::format_err!("missing type for node {node:?}"))?;

        ty.traverse_mut(&mut |ty| {
            self.types.insert(ty.clone());
        });

        Ok(ty)
    }

    fn structural_ty(&self, ty: &ir::Type) -> String {
        match ty.mangle_structural() {
            Some(ty) => format!("(ref null ${ty})"),
            None => match ty {
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

    fn conversions(&self, ty: &ir::Type) -> (Option<String>, Option<String>) {
        if let ir::Type::Named {
            intrinsic,
            representation,
            ..
        } = ty
        {
            if let Some("f64") = representation.as_deref() {
                return (
                    Some(String::from(
                        "(ref.cast (ref null $number)) (struct.get $number 0)",
                    )),
                    Some(String::from("(struct.new $number)")),
                );
            } else if *intrinsic {
                return (
                    Some(String::from("(extern.convert_any)")),
                    Some(String::from("(any.convert_extern)")),
                );
            }
        }

        (Some(format!("(ref.cast {})", self.structural_ty(ty))), None)
    }

    fn runtime(
        &mut self,
        name: &str,
        inputs: Vec<Result<ir::Type, &'static str>>,
        outputs: Vec<Result<ir::Type, &'static str>>,
    ) -> Result<String, CodegenError> {
        let mut mangled = name.to_string();
        for ty in inputs.iter().chain(&outputs).flatten() {
            write!(mangled, "_{}", ty.mangle_nominal())?;
        }

        self.runtime_functions
            .entry(name.to_string())
            .or_default()
            .insert((mangled.clone(), inputs, outputs));

        Ok(mangled)
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

        let name = self.runtime(
            "make-string",
            vec![Err("i32"), Err("i32")],
            vec![Err("externref")],
        )?;

        Ok(format!(
            "(call ${name} (i32.const {}) (i32.const {}))",
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
