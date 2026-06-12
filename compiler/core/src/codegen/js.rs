use crate::{
    codegen::{CodegenError, Options, TraceOptions, ir},
    db::{Db, Node},
    facts::{DebugInfo, Syntax},
    span::Span,
};
use std::{collections::BTreeSet, fmt::Write};

#[derive(Debug)]
pub struct JsResult {
    pub module: String,
    pub source_map: String,
}

pub fn to_js(
    db: &Db,
    program: &ir::Program,
    options: Options<'_>,
) -> Result<JsResult, CodegenError> {
    let mut writer = Writer {
        string: String::new(),
        line: 0,
        col: 0,
        mapping: None,
        sourcemap: parcel_sourcemap::SourceMap::new(""),
    };

    writeln!(writer, "let env;")?;

    let mut definitions = Vec::new();
    let mut exports = Vec::new();
    for (key, function) in &program.definitions {
        let export = match key {
            ir::DefinitionKey::TopLevel => Some("default"),
            ir::DefinitionKey::Constant(_) => None, // TODO: Allow exporting
        };

        if let Some(export) = export {
            exports.push((export, key));
        }

        let mut ctx = WriteContext {
            db,
            options,
            definitions: &mut definitions,
            writer: &mut writer,
        };

        ctx.write_function(Some(key), function)?;

        writeln!(writer)?;
    }

    writeln!(writer, "export {{")?;
    for (name, key) in exports {
        writeln!(
            writer,
            "{} as {:?},",
            mangle_function(key, &mut definitions),
            name
        )?;
    }
    writeln!(writer, "}};")?;

    Ok(writer.finish(db, program, options))
}

#[derive(Debug)]
struct Writer<'a> {
    string: String,
    line: usize,
    col: usize,
    mapping: Option<Mapping<'a>>,
    sourcemap: parcel_sourcemap::SourceMap,
}

#[derive(Debug, Clone, Copy)]
struct Mapping<'a> {
    span: &'a Span,
    debug_info: Option<&'a DebugInfo>,
}

impl Write for Writer<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        if let Some(mapping) = self.mapping {
            let source = self
                .sourcemap
                .get_sources()
                .iter()
                .position(|p| p == mapping.span.path)
                .map_or_else(
                    || self.sourcemap.add_source(&mapping.span.path),
                    |index| index as u32,
                );

            let name = mapping
                .debug_info
                .is_some_and(|info| info.variable)
                .then_some(&mapping.span.source)
                .map(|s| self.sourcemap.add_name(s));

            self.sourcemap.add_mapping(
                self.line as u32,
                self.col as u32,
                Some(parcel_sourcemap::OriginalLocation {
                    original_line: mapping.span.start.line as u32 - 1,
                    original_column: mapping.span.start.column as u32 - 1,
                    source,
                    name,
                }),
            );
        }

        for c in s.chars() {
            if c == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
        }

        self.string.push_str(s);

        Ok(())
    }
}

impl Writer<'_> {
    fn finish(mut self, db: &Db, program: &ir::Program, options: Options<'_>) -> JsResult {
        // Write source map

        for &file in &program.source_files {
            if let Some(Syntax(syntax)) = db.get(file) {
                let span = syntax.get(db).span(db);

                if let Some(source_index) = self
                    .sourcemap
                    .get_sources()
                    .iter()
                    .position(|path| path == span.path)
                {
                    self.sourcemap
                        .set_source_content(source_index, &span.source)
                        .unwrap();
                }
            }
        }

        let mut vlq = Vec::new();
        self.sourcemap.write_vlq(&mut vlq).unwrap();
        let vlq = String::from_utf8(vlq).unwrap();

        let json = serde_json::json!({
            "version": 3,
            "sourceRoot": options.source_root,
            "sources": self.sourcemap.get_sources(),
            "sourcesContent": self.sourcemap.get_sources_content(),
            "names": self.sourcemap.get_names(),
            "mappings": vlq,
        });

        writeln!(
            self.string,
            "\n//# sourceMappingURL={}.map",
            options.file_name
        )
        .unwrap();

        JsResult {
            module: self.string,
            source_map: json.to_string(),
        }
    }
}

fn collect_locals(function: &ir::Function) -> Result<BTreeSet<Node>, CodegenError> {
    let mut locals = BTreeSet::new();
    for instruction in &function.instructions {
        instruction.clone().for_each_node(false, &mut |node| {
            if !function.inputs.contains(node)
                && !function
                    .closure
                    .as_ref()
                    .is_some_and(|(_, captures)| captures.contains(node))
            {
                locals.insert(*node);
            }

            Ok(())
        })?;
    }

    Ok(locals)
}

#[derive(Debug)]
struct WriteContext<'a, 'w> {
    db: &'a Db,
    options: Options<'a>,
    definitions: &'w mut Vec<&'a ir::DefinitionKey>,
    writer: &'w mut Writer<'a>,
}

impl<'a> WriteContext<'a, '_> {
    fn update_mapping(&mut self, node: Node) {
        if let Some(Syntax(syntax)) = self.db.get(node) {
            self.writer.mapping = Some(Mapping {
                span: syntax.get(self.db).span(self.db),
                debug_info: self.db.get::<DebugInfo>(node),
            });
        }
    }

    fn write_function(
        &mut self,
        key: Option<&'a ir::DefinitionKey>,
        function: &'a ir::Function,
    ) -> Result<(), CodegenError> {
        write!(self.writer, "function ")?;

        if let Some(key) = key {
            write!(self.writer, "{}", mangle_function(key, self.definitions))?;
        }

        if let Some(ir::DefinitionKey::TopLevel) = key {
            assert!(function.inputs.is_empty());

            writeln!(self.writer, "(e) {{")?;
            writeln!(self.writer, "env = e;")?;
        } else {
            write!(self.writer, "(")?;
            for (index, input) in function.inputs.iter().enumerate() {
                if index > 0 {
                    write!(self.writer, ", ")?;
                }
                write!(self.writer, "{}", mangle_local(*input))?;
            }
            writeln!(self.writer, ") {{")?;
        }

        for local in collect_locals(function)? {
            writeln!(self.writer, "let {};", mangle_local(local))?;
        }

        self.write_instructions(&function.instructions)?;

        write!(self.writer, "}}")?;

        Ok(())
    }

    fn write_instructions(
        &mut self,
        instructions: &'a [ir::Instruction],
    ) -> Result<(), CodegenError> {
        for instruction in instructions {
            if let Some(node) = instruction.primary_node() {
                self.update_mapping(node);
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
                            write!(self.writer, "else ")?;
                        }

                        write!(self.writer, "if (")?;
                        self.write_conditions(conditions)?;
                        write!(self.writer, ") {{")?;

                        self.write_instructions(instructions)?;

                        if let Some(node) = *node
                            && let Some(then_node) = *then_node
                        {
                            writeln!(
                                self.writer,
                                "{} = {};",
                                mangle_local(node),
                                mangle_local(then_node)
                            )?;
                        }

                        writeln!(self.writer, "}}")?;
                    }

                    if let Some((instructions, else_node)) = else_branch {
                        write!(self.writer, "else {{")?;

                        self.write_instructions(instructions)?;

                        if let Some(node) = *node
                            && let Some(else_node) = *else_node
                        {
                            writeln!(
                                self.writer,
                                "{} = {};",
                                mangle_local(node),
                                mangle_local(else_node)
                            )?;
                        } else {
                            self.write_intrinsic("unreachable", &[])?;
                        }

                        writeln!(self.writer, "}}")?;
                    }
                }
                ir::Instruction::Return { value } => {
                    writeln!(self.writer, "return {};", mangle_local(*value))?;
                }
                ir::Instruction::Loop { node, body, result } => {
                    writeln!(self.writer, "while (true) {{")?;
                    self.write_instructions(body)?;

                    write!(
                        self.writer,
                        "if ({:?} in {}) {{ {} = {}[{:?}][0]; break; }}",
                        mangle_variant(1),
                        mangle_local(*result),
                        mangle_local(*node),
                        mangle_local(*result),
                        mangle_variant(1)
                    )?;
                    writeln!(self.writer, "}}")?;
                }
                ir::Instruction::Trace { span } => {
                    let can_trace = match self.options.trace {
                        TraceOptions::None => false,
                        TraceOptions::All => true,
                        TraceOptions::Files(files) => files.contains(&span.path.as_str()),
                    };

                    if can_trace {
                        writeln!(self.writer, "env.trace({});", format_trace(span))?;
                    }
                }
                ir::Instruction::Value { node, value } => {
                    write!(self.writer, "{} = ", mangle_local(*node))?;
                    self.write_value(value)?;
                    writeln!(self.writer, ";")?;
                }
            }
        }

        Ok(())
    }

    fn write_conditions(&mut self, conditions: &'a [ir::Condition]) -> Result<(), CodegenError> {
        if conditions.is_empty() {
            write!(self.writer, "true")?;
        } else {
            write!(self.writer, "(")?;

            for (index, condition) in conditions.iter().enumerate() {
                if let Some(node) = condition.primary_node() {
                    self.update_mapping(node);
                }

                if index > 0 {
                    write!(self.writer, " && ")?;
                }

                match condition {
                    ir::Condition::Or(branches) => {
                        if branches.is_empty() {
                            write!(self.writer, "false")?;
                        } else {
                            write!(self.writer, "(")?;
                            for (index, branch) in branches.iter().enumerate() {
                                if index > 0 {
                                    write!(self.writer, " || ")?;
                                }
                                self.write_conditions(branch)?;
                            }
                            write!(self.writer, ")")?;
                        }
                    }
                    ir::Condition::EqualToNumber { input, value } => {
                        write!(self.writer, "{} === {}", mangle_local(*input), value)?;
                    }
                    ir::Condition::EqualToString { input, value } => {
                        write!(self.writer, "{} === {:?}", mangle_local(*input), value)?;
                    }
                    ir::Condition::EqualToVariant { input, variant } => {
                        write!(
                            self.writer,
                            "{:?} in {}",
                            mangle_variant(*variant),
                            mangle_local(*input)
                        )?;
                    }
                    ir::Condition::Initialize {
                        variable, value, ..
                    } => {
                        write!(self.writer, "!void ({} = ", mangle_local(*variable))?;
                        self.write_value(value)?;
                        write!(self.writer, ")")?;
                    }
                    ir::Condition::Mutate { input, variable } => {
                        write!(
                            self.writer,
                            "!void ({} = {})",
                            mangle_local(*variable),
                            mangle_local(*input)
                        )?;
                    }
                }
            }

            write!(self.writer, ")")?;
        }

        Ok(())
    }

    fn write_value(&mut self, value: &'a ir::Value) -> Result<(), CodegenError> {
        match value {
            ir::Value::Bound(node) => {
                return Err(anyhow::format_err!("bound {node:?} not resolved"));
            }
            ir::Value::Call { function, inputs } => {
                write!(self.writer, "{}(", mangle_local(*function))?;
                for input in inputs {
                    write!(self.writer, "{}, ", mangle_local(*input))?;
                }
                write!(self.writer, ")")?;
            }
            ir::Value::Constant(key) => {
                write!(self.writer, "{}()", mangle_function(key, self.definitions))?;
            }
            ir::Value::Function(function) => {
                self.write_function(None, function)?;
            }
            ir::Value::Field {
                input, field_name, ..
            } => {
                write!(self.writer, "{}[{:?}]", mangle_local(*input), field_name)?;
            }
            ir::Value::Tuple(elements) => {
                write!(self.writer, "[")?;
                for (index, element) in elements.iter().enumerate() {
                    if index > 0 {
                        write!(self.writer, ", ")?;
                    }

                    write!(self.writer, "{}", mangle_local(*element))?;
                }
                write!(self.writer, "]")?;
            }
            ir::Value::Marker => {
                write!(self.writer, "undefined")?;
            }
            ir::Value::MutableVariable(node) => {
                write!(self.writer, "{}", mangle_local(*node))?;
            }
            ir::Value::Number(number) => {
                write!(self.writer, "{number}")?;
            }
            ir::Value::Runtime { name, inputs } => {
                self.write_intrinsic(name, inputs)?;
            }
            ir::Value::String(string) => {
                write!(self.writer, "{string:?}")?;
            }
            ir::Value::Structure(fields) => {
                write!(self.writer, "{{")?;
                for (index, (field_name, field)) in fields.iter().enumerate() {
                    if index > 0 {
                        write!(self.writer, ", ")?;
                    }

                    write!(self.writer, "{:?}: {}", field_name, mangle_local(*field))?;
                }
                write!(self.writer, "}}")?;
            }
            ir::Value::TupleElement { input, index } => {
                write!(self.writer, "{}[{}]", mangle_local(*input), index)?;
            }
            ir::Value::Variable(node) => {
                write!(self.writer, "{}", mangle_local(*node))?;
            }
            ir::Value::Variant { index, elements } => {
                write!(self.writer, "{{ {:?}: [", mangle_variant(*index))?;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    write!(self.writer, "{}", mangle_local(*element))?;
                }
                write!(self.writer, "] }}")?;
            }
            ir::Value::VariantElement {
                input,
                variant,
                index,
            } => {
                write!(
                    self.writer,
                    "{}[{:?}][{}]",
                    mangle_local(*input),
                    mangle_variant(*variant),
                    index
                )?;
            }
        }

        Ok(())
    }

    fn write_intrinsic(&mut self, name: &str, inputs: &[Node]) -> Result<(), CodegenError> {
        match (name, inputs) {
            ("debug", [input]) => {
                write!(self.writer, "env.debug({})", mangle_local(*input))?;
            }
            ("string-count", [input]) => {
                write!(self.writer, "{}.length", mangle_local(*input))?;
            }
            ("string-concat", [left, right]) => {
                write!(
                    self.writer,
                    "{} + {}",
                    mangle_local(*left),
                    mangle_local(*right)
                )?;
            }
            ("external", [name, input]) => {
                write!(
                    self.writer,
                    "env[{}]({})",
                    mangle_local(*name),
                    mangle_local(*input)
                )?;
            }
            ("number-to-string", [input]) => {
                write!(self.writer, "{}.toString()", mangle_local(*input))?;
            }
            ("string-to-number", [input]) => {
                write!(self.writer, "parseFloat({})", mangle_local(*input))?;
            }
            ("add", [left, right]) => {
                write!(
                    self.writer,
                    "{} + {}",
                    mangle_local(*left),
                    mangle_local(*right)
                )?;
            }
            ("sub", [left, right]) => {
                write!(
                    self.writer,
                    "{} - {}",
                    mangle_local(*left),
                    mangle_local(*right)
                )?;
            }
            ("mul", [left, right]) => {
                write!(
                    self.writer,
                    "{} * {}",
                    mangle_local(*left),
                    mangle_local(*right)
                )?;
            }
            ("div", [left, right]) => {
                write!(
                    self.writer,
                    "{} / {}",
                    mangle_local(*left),
                    mangle_local(*right)
                )?;
            }
            ("rem", [left, right]) => {
                write!(
                    self.writer,
                    "{} % {}",
                    mangle_local(*left),
                    mangle_local(*right)
                )?;
            }
            ("pow", [left, right]) => {
                write!(
                    self.writer,
                    "{} ** {}",
                    mangle_local(*left),
                    mangle_local(*right)
                )?;
            }
            ("floor", [input]) => {
                write!(self.writer, "Math.floor({})", mangle_local(*input))?;
            }
            ("ceil", [input]) => {
                write!(self.writer, "Math.ceil({})", mangle_local(*input))?;
            }
            ("sqrt", [input]) => {
                write!(self.writer, "Math.sqrt({})", mangle_local(*input))?;
            }
            ("neg", [input]) => {
                write!(self.writer, "-{}", mangle_local(*input))?;
            }
            ("sin", [input]) => {
                write!(self.writer, "Math.sin({})", mangle_local(*input))?;
            }
            ("cos", [input]) => {
                write!(self.writer, "Math.cos({})", mangle_local(*input))?;
            }
            ("tan", [input]) => {
                write!(self.writer, "Math.tan({})", mangle_local(*input))?;
            }
            ("equal", [left, right]) => {
                write!(
                    self.writer,
                    "{} === {} ? 1 : 0",
                    mangle_local(*left),
                    mangle_local(*right)
                )?;
            }
            ("order", [left, right]) => {
                write!(
                    self.writer,
                    "({} < {}) ? -1 : (({} > {}) ? 1 : 0)",
                    mangle_local(*left),
                    mangle_local(*right),
                    mangle_local(*left),
                    mangle_local(*right)
                )?;
            }
            ("empty-list", []) => {
                write!(self.writer, "[]")?;
            }
            ("list-count", [input]) => {
                write!(self.writer, "{}.length", mangle_local(*input))?;
            }
            ("list-first", [input]) => {
                write!(self.writer, "{}[0]", mangle_local(*input))?;
            }
            ("list-last", [input]) => {
                write!(
                    self.writer,
                    "{}[{}.length - 1]",
                    mangle_local(*input),
                    mangle_local(*input)
                )?;
            }
            ("list-initial", [input]) => {
                write!(self.writer, "{}.slice(0, -1)", mangle_local(*input))?;
            }
            ("list-tail", [input]) => {
                write!(self.writer, "{}.slice(1)", mangle_local(*input))?;
            }
            ("list-nth", [input, index]) => {
                write!(
                    self.writer,
                    "{}[{}]",
                    mangle_local(*input),
                    mangle_local(*index)
                )?;
            }
            ("list-append", [list, element]) => {
                write!(
                    self.writer,
                    "[...{}, {}]",
                    mangle_local(*list),
                    mangle_local(*element)
                )?;
            }
            ("list-prepend", [list, element]) => {
                write!(
                    self.writer,
                    "[{}, ...{}]",
                    mangle_local(*element),
                    mangle_local(*list)
                )?;
            }
            ("list-insert-at", [list, index, element]) => {
                write!(
                    self.writer,
                    "[...{}.slice(0, {}), {}, ...{}.slice({})]",
                    mangle_local(*list),
                    mangle_local(*index),
                    mangle_local(*element),
                    mangle_local(*list),
                    mangle_local(*index)
                )?;
            }
            ("list-remove-at", [list, index]) => {
                write!(
                    self.writer,
                    "[...{}.slice(0, {}), ...{}.slice({} + 1)]",
                    mangle_local(*list),
                    mangle_local(*index),
                    mangle_local(*list),
                    mangle_local(*index)
                )?;
            }
            ("string-characters", [input]) => {
                write!(self.writer, "{}.split(\"\")", mangle_local(*input))?;
            }
            ("random-number", [min, max]) => {
                write!(
                    self.writer,
                    "Math.random() * ({} - {}) + {}",
                    mangle_local(*min),
                    mangle_local(*max),
                    mangle_local(*min)
                )?;
            }
            ("nan", []) => {
                write!(self.writer, "NaN")?;
            }
            ("is-nan", [input]) => {
                write!(self.writer, "isNaN({}) ? 1 : 0", mangle_local(*input))?;
            }
            ("hash-string", [input]) => {
                writeln!(self.writer, "(() => {{")?;
                writeln!(self.writer, "let hash = 0;")?;
                writeln!(
                    self.writer,
                    "for (const char of {}) hash = (hash << 5) - hash + char;",
                    mangle_local(*input)
                )?;
                writeln!(self.writer, "return hash >>> 0;")?;
                writeln!(self.writer, "}})()")?;
            }
            ("unreachable", []) => {
                write!(
                    self.writer,
                    "(() => {{ throw new Error(\"unreachable\"); }})()"
                )?;
            }
            _ => {
                return Err(anyhow::format_err!(
                    "invalid intrinsic {name:?}({inputs:?})"
                ));
            }
        }

        Ok(())
    }
}

fn mangle_node(node: Node) -> String {
    node.id()
        .to_string()
        .replace(|c: char| !c.is_ascii_alphanumeric(), "_")
}

fn mangle_function<'a>(
    key: &'a ir::DefinitionKey,
    indices: &mut Vec<&'a ir::DefinitionKey>,
) -> String {
    let index = indices.iter().position(|k| *k == key).unwrap_or_else(|| {
        let len = indices.len();
        indices.push(key);
        len
    });

    format!("func_{index}")
}

fn mangle_local(node: Node) -> String {
    format!("local_{}", mangle_node(node))
}

fn mangle_variant(index: usize) -> String {
    format!("variant_{index}")
}

fn format_trace(span: &Span) -> String {
    serde_json::json!({
        "path": span.path,
        "start": span.start,
        "end": span.end,
    })
    .to_string()
}
