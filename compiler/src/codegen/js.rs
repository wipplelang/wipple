use crate::{
    codegen::{CodegenResult, Options, ir, mangle::Mangle},
    database::{Db, NodeRef, Span},
};
use parcel_sourcemap::SourceMap;
use std::{
    collections::BTreeSet,
    fmt::{self, Write as _},
};

pub fn write_to_string(program: &ir::Program, options: Options<'_>, db: &Db) -> String {
    let mut script = String::new();
    Backend::new(&mut script, options, db)
        .write(program)
        .unwrap();

    script
}

pub struct Backend<'a> {
    writer: Writer<'a>,
    options: Options<'a>,
    db: &'a Db,
}

struct Writer<'a> {
    inner: &'a mut (dyn fmt::Write + 'a),
    line: usize,
    column: usize,
    sourcemap: Option<SourceMap>,
}

impl<'a> Backend<'a> {
    pub fn new(w: &'a mut (dyn fmt::Write + 'a), options: Options<'a>, db: &'a Db) -> Self {
        Backend {
            writer: Writer {
                inner: w,
                sourcemap: options.sourcemap.then(|| SourceMap::new("")),
                line: 0,
                column: 0,
            },
            options,
            db,
        }
    }

    pub fn write(mut self, program: &ir::Program) -> CodegenResult {
        writeln!(self.writer, "let runtime;")?;
        writeln!(self.writer, "export default (r) => {{")?;
        writeln!(self.writer, "runtime = r;")?;

        self.write_instructions(&program.instructions, &|_| true)?;

        writeln!(self.writer, "}};")?;

        for (key, instructions) in &program.definitions {
            writeln!(self.writer, "function {}() {{", key.mangle())?;
            self.write_instructions(instructions, &|_| true)?;
            writeln!(self.writer, "}}")?;
        }

        if let Some(sourcemap) = &mut self.writer.sourcemap {
            for span in &program.files {
                if let Some(source_index) = sourcemap
                    .get_sources()
                    .iter()
                    .position(|path| path == &span.path)
                {
                    sourcemap
                        .set_source_content(source_index, &span.source)
                        .unwrap();
                }
            }

            let mut vlq = Vec::new();
            sourcemap.write_vlq(&mut vlq).unwrap();
            let vlq = String::from_utf8(vlq).unwrap();

            let json = serde_json::json!({
                "version": 3,
                "sources": sourcemap.get_sources(),
                "sourcesContent": sourcemap.get_sources_content(),
                "names": sourcemap.get_names(),
                "mappings": vlq,
            });

            let base64 =
                base64::Engine::encode(&base64::prelude::BASE64_STANDARD, json.to_string());

            write!(
                self.writer,
                "\n//# sourceMappingURL=data:application/json;base64,{base64}"
            )?;
        }

        Ok(())
    }

    pub fn write_instructions(
        &mut self,
        instructions: &[ir::Instruction],
        declare: &dyn Fn(&NodeRef) -> bool,
    ) -> CodegenResult {
        for node in instructions
            .iter()
            .flat_map(|instruction| instruction.nodes())
            .collect::<BTreeSet<_>>()
        {
            if declare(node) {
                writeln!(self.writer, "let {};", node.mangle())?;
            }
        }

        for instruction in instructions {
            if let Some(sourcemap) = &mut self.writer.sourcemap
                && let Some(node) = instruction.nodes().first()
            {
                let span = self.db.span(node);

                let source = sourcemap
                    .get_sources()
                    .iter()
                    .position(|p| p == &span.path)
                    .map_or_else(|| sourcemap.add_source(&span.path), |index| index as u32);

                let name = node.identifier().map(|identifier| {
                    sourcemap
                        .get_name_index(&identifier)
                        .unwrap_or_else(|| sourcemap.add_name(&identifier))
                });

                sourcemap.add_mapping(
                    self.writer.line as u32,
                    self.writer.column as u32,
                    Some(parcel_sourcemap::OriginalLocation {
                        original_line: span.start.line as u32 - 1,
                        original_column: span.start.column as u32 - 1,
                        source,
                        name,
                    }),
                );
            }

            match instruction {
                ir::Instruction::If {
                    node,
                    branches,
                    else_branch,
                } => {
                    for (index, (conditions, instructions, then_node)) in
                        branches.iter().enumerate()
                    {
                        if index > 0 {
                            write!(self.writer, " else ")?;
                        }
                        write!(self.writer, "if (")?;
                        if conditions.is_empty() {
                            write!(self.writer, "true")?;
                        } else {
                            for (index, condition) in conditions.iter().enumerate() {
                                if index > 0 {
                                    write!(self.writer, " && ")?;
                                }
                                self.write_condition(condition)?;
                            }
                        }
                        writeln!(self.writer, ") {{")?;
                        self.write_instructions(instructions, &|_| false)?;
                        if let Some(node) = node
                            && let Some(then_node) = then_node
                        {
                            writeln!(self.writer, "{} = {};", node.mangle(), then_node.mangle())?;
                        }
                        write!(self.writer, "}}")?;
                    }

                    writeln!(self.writer, " else {{")?;
                    if let Some((instructions, else_node)) = else_branch {
                        self.write_instructions(instructions, &|_| false)?;
                        if let Some(node) = node
                            && let Some(else_node) = else_node
                        {
                            writeln!(self.writer, "{} = {};", node.mangle(), else_node.mangle())?;
                        }
                    } else {
                        writeln!(self.writer, "throw new Error(\"unreachable\");")?;
                    }
                    writeln!(self.writer, "}}")?;
                }
                ir::Instruction::Return { value } => {
                    writeln!(self.writer, "return {};", value.mangle())?;
                }
                ir::Instruction::Value { node, value } => {
                    write!(self.writer, "{} = ", node.mangle())?;
                    self.write_value(value)?;
                    writeln!(self.writer, ";")?;
                }
                ir::Instruction::Trace { location } => {
                    let span = self.db.span(location);

                    if self.can_trace(&span) {
                        writeln!(
                            self.writer,
                            "runtime.trace({:?});",
                            serde_json::json!(span.start).to_string()
                        )?;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn write_value(&mut self, value: &ir::Value) -> CodegenResult {
        match value {
            ir::Value::Bound(node) => panic!("bound {node:?} not resolved"),
            ir::Value::Call { function, inputs } => {
                write!(self.writer, "{}(", function.mangle())?;
                for input in inputs {
                    write!(self.writer, "{}, ", input.mangle())?;
                }
                write!(self.writer, ")")?;
            }
            ir::Value::Concat { segments, trailing } => {
                write!(self.writer, "(")?;
                for (string, node) in segments {
                    write!(self.writer, "{:?} + {} + ", string, node.mangle())?;
                }
                write!(self.writer, "{:?})", trailing)?;
            }
            ir::Value::Constant(key) => {
                write!(self.writer, "{}()", key.mangle())?;
            }
            ir::Value::Function {
                inputs,
                captures,
                instructions,
            } => {
                write!(self.writer, "(")?;
                for input in inputs {
                    write!(self.writer, "{}, ", input.mangle())?;
                }
                write!(self.writer, ") => {{")?;
                self.write_instructions(instructions, &|node| {
                    !inputs.contains(node) && !captures.contains(node)
                })?;
                write!(self.writer, "}}")?;
            }
            ir::Value::Field { input, field } => {
                write!(self.writer, "{}[{:?}]", input.mangle(), field)?;
            }
            ir::Value::Tuple(elements) => {
                write!(self.writer, "[")?;
                for element in elements {
                    write!(self.writer, "{}, ", element.mangle())?;
                }
                write!(self.writer, "]")?;
            }
            ir::Value::Marker => {
                write!(self.writer, "null")?;
            }
            ir::Value::MutableVariable(node) => {
                write!(self.writer, "{}.value", node.mangle())?;
            }
            ir::Value::Number(number) => {
                write!(self.writer, "{}", number)?;
            }
            ir::Value::Runtime { name, inputs } => {
                write!(
                    self.writer,
                    "runtime.{}(",
                    convert_case::ccase!(kebab -> camel, name)
                )?;

                for input in inputs {
                    write!(self.writer, "{}, ", input.mangle())?;
                }

                write!(self.writer, ")")?;
            }
            ir::Value::String(string) => {
                write!(self.writer, "{:?}", string)?;
            }
            ir::Value::Structure(fields) => {
                write!(self.writer, "{{ ")?;
                for (name, value) in fields {
                    write!(self.writer, "{:?}: {}, ", name, value.mangle())?;
                }
                write!(self.writer, " }}")?;
            }
            ir::Value::TupleElement { input, index } => {
                write!(self.writer, "{}[{}]", input.mangle(), index)?;
            }
            ir::Value::Unreachable => {
                write!(
                    self.writer,
                    "(() => {{ throw new Error(\"unreachable\"); }})()"
                )?;
            }
            ir::Value::Variable(node) => {
                write!(self.writer, "{}", node.mangle())?;
            }
            ir::Value::Variant { index, elements } => {
                write!(self.writer, "runtime.variant({}, [", index)?;
                for element in elements {
                    write!(self.writer, "{}, ", element.mangle())?;
                }
                write!(self.writer, "])")?;
            }
            ir::Value::VariantElement { input, index } => {
                write!(self.writer, "{}[{}]", input.mangle(), index)?;
            }
        }

        Ok(())
    }

    pub fn write_condition(&mut self, condition: &ir::Condition) -> CodegenResult {
        match condition {
            ir::Condition::Or(conditions) => {
                if conditions.is_empty() {
                    write!(self.writer, "true")?;
                } else {
                    write!(self.writer, "(")?;
                    for (index, condition) in conditions.iter().enumerate() {
                        if index > 0 {
                            write!(self.writer, " || ")?;
                        }
                        self.write_condition(condition)?;
                    }
                    write!(self.writer, ")")?;
                }
            }
            ir::Condition::EqualToNumber { input, value } => {
                write!(self.writer, "({} === {})", input.mangle(), value)?;
            }
            ir::Condition::EqualToString { input, value } => {
                write!(self.writer, "({} === {:?})", input.mangle(), value)?;
            }
            ir::Condition::EqualToVariant { input, variant } => {
                write!(
                    self.writer,
                    "({}[runtime.variant] === {})",
                    input.mangle(),
                    variant
                )?;
            }
            ir::Condition::Initialize {
                variable,
                value,
                mutable,
            } => {
                write!(self.writer, "(({} = ", variable.mangle())?;
                if *mutable {
                    write!(self.writer, "{{ value: ")?;
                }
                self.write_value(value)?;
                if *mutable {
                    write!(self.writer, " }}")?;
                }
                write!(self.writer, ") || true)")?;
            }
            ir::Condition::Mutate { input, variable } => {
                write!(
                    self.writer,
                    "(({}.value = {}) || true)",
                    variable.mangle(),
                    input.mangle()
                )?;
            }
        }

        Ok(())
    }

    fn can_trace(&self, span: &Span) -> bool {
        self.options.trace.contains(&span.path.as_str())
    }
}

impl fmt::Write for Writer<'_> {
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
