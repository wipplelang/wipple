use crate::{
    codegen::{CodegenError, mir},
    db::{Db, Node},
    facts::{DebugInfo, Syntax},
    span::Span,
};
use std::fmt::{self, Write};

#[derive(Debug, Clone, Copy)]
pub struct Options<'a> {
    pub file_name: Option<&'a str>,
    pub source_root: &'a str,
    pub include_prelude: bool,
}

pub struct Backend<'a> {
    db: &'a Db,
    options: Options<'a>,
    module: String,
    line: usize,
    col: usize,
    mapping: Option<Mapping<'a>>,
    sourcemap: parcel_sourcemap::SourceMap,
}

#[derive(Debug)]
pub struct Output {
    pub module: String,
    pub source_map: String,
}

impl<'a> Backend<'a> {
    pub fn new(db: &'a Db, options: Options<'a>) -> Self {
        Backend {
            db,
            options,
            module: String::new(),
            line: 0,
            col: 0,
            mapping: None,
            sourcemap: parcel_sourcemap::SourceMap::new(""),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Mapping<'a> {
    span: &'a Span,
    debug_info: Option<&'a DebugInfo>,
}

impl fmt::Write for Backend<'_> {
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

        self.module.push_str(s);

        Ok(())
    }
}

impl super::Backend for Backend<'_> {
    type Output = Output;

    fn run(mut self, program: &mir::Program) -> Result<Self::Output, CodegenError> {
        // Write prelude

        if self.options.include_prelude {
            writeln!(self, "let env;")?;
            writeln!(self)?;
            writeln!(self, "export default function (e) {{")?;
            writeln!(self, "env = e;")?;

            if let Some(main) = program.main {
                writeln!(self, "{}();", mangle_function(main))?;
            }

            writeln!(self, "}}")?;
        }

        // Write program

        for (&index, function) in &program.functions {
            self.write_function(Some(index), function)?;
        }

        if !self.options.include_prelude
            && let Some(main) = program.main
        {
            writeln!(self, "{}();", mangle_function(main))?;
        }

        // Write source map

        for file in &program.source_files {
            if let Some(source_index) = self
                .sourcemap
                .get_sources()
                .iter()
                .position(|path| path == file.path)
            {
                self.sourcemap
                    .set_source_content(source_index, &file.source)
                    .unwrap();
            }
        }

        let mut vlq = Vec::new();
        self.sourcemap.write_vlq(&mut vlq).unwrap();
        let vlq = String::from_utf8(vlq).unwrap();

        let json = serde_json::json!({
            "version": 3,
            "sourceRoot": self.options.source_root,
            "sources": self.sourcemap.get_sources(),
            "sourcesContent": self.sourcemap.get_sources_content(),
            "names": self.sourcemap.get_names(),
            "mappings": vlq,
        });

        if let Some(file_name) = self.options.file_name {
            writeln!(self.module, "\n//# sourceMappingURL={file_name}.map").unwrap();
        }

        Ok(Output {
            module: self.module,
            source_map: json.to_string(),
        })
    }
}

impl Backend<'_> {
    fn update_mapping(&mut self, node: Node) {
        if let Some(Syntax(syntax)) = self.db.get(node) {
            self.mapping = Some(Mapping {
                span: syntax.get(self.db).span(self.db),
                debug_info: self.db.get::<DebugInfo>(node),
            });
        }
    }

    fn write_function(
        &mut self,
        index: Option<mir::FunctionIndex>,
        function: &mir::Function,
    ) -> Result<(), CodegenError> {
        write!(self, "function ")?;

        if let Some(index) = index {
            write!(self, "{}", mangle_function(index))?;
        }

        write!(self, "(")?;

        for &index in function.inputs.keys() {
            self.write_local(index)?;
            write!(self, ", ")?;
        }

        writeln!(self, ") {{")?;

        for &index in function.locals.keys() {
            write!(self, "let ")?;
            self.write_local(index)?;
            writeln!(self, ";")?;
        }

        self.write_statements(&function.body)?;

        writeln!(self, "}}")?;

        Ok(())
    }

    fn write_statements(&mut self, statements: &[mir::Statement]) -> Result<(), CodegenError> {
        for statement in statements {
            match statement {
                mir::Statement::If {
                    branches,
                    else_branch,
                } => {
                    for (index, (condition, statements)) in branches.iter().enumerate() {
                        if index > 0 {
                            write!(self, " else ")?;
                        }

                        write!(self, "if (")?;
                        self.write_condition(condition)?;
                        write!(self, ") {{")?;
                        self.write_statements(statements)?;
                        write!(self, "}}")?;
                    }

                    if let Some(statements) = else_branch {
                        write!(self, " else {{")?;
                        self.write_statements(statements)?;
                        write!(self, "}}")?;
                    }
                }
                mir::Statement::Return { value } => {
                    write!(self, "return ")?;
                    self.write_local(*value)?;
                    write!(self, ";")?;
                }
                mir::Statement::Loop { body } => {
                    writeln!(self, "while (true) {{")?;
                    self.write_statements(body)?;
                    write!(self, "}}")?;
                }
                mir::Statement::Break => {
                    write!(self, "break;")?;
                }
                mir::Statement::Assign { local, value } => {
                    self.write_local(*local)?;
                    write!(self, " = ")?;
                    self.write_expression(value.as_ref())?;
                    write!(self, ";")?;
                }
                mir::Statement::Trace { trace } => {
                    write!(self, "env.trace({trace});")?;
                }
            }

            writeln!(self)?;
        }

        Ok(())
    }

    fn write_condition(&mut self, condition: &mir::Condition) -> Result<(), CodegenError> {
        match condition {
            mir::Condition::True => {
                write!(self, "true")?;
            }
            mir::Condition::False => {
                write!(self, "false")?;
            }
            mir::Condition::And { left, right } => {
                write!(self, "(")?;
                self.write_condition(left)?;
                write!(self, " && ")?;
                self.write_condition(right)?;
                write!(self, ")")?;
            }
            mir::Condition::Or { left, right } => {
                write!(self, "(")?;
                self.write_condition(left)?;
                write!(self, " || ")?;
                self.write_condition(right)?;
                write!(self, ")")?;
            }
            mir::Condition::Intrinsic { intrinsic } => {
                self.write_intrinsic(intrinsic)?;
            }
            mir::Condition::Variant { value, variant } => {
                write!(self, "({:?} in (", mangle_variant(*variant))?;
                self.write_expression(value.as_ref())?;
                write!(self, "))")?;
            }
            mir::Condition::Initialize { local, value } => {
                write!(self, "!void (")?;
                self.write_local(*local)?;
                write!(self, " = ")?;
                self.write_expression(value.as_ref())?;
                write!(self, ")")?;
            }
            mir::Condition::Mutate { local, value } => {
                write!(self, "!void (")?;
                self.write_local(*local)?;
                write!(self, " = ")?;
                self.write_local(*value)?;
                write!(self, ")")?;
            }
        }

        Ok(())
    }

    fn write_expression(
        &mut self,
        expression: mir::SourceMapped<&mir::Expression>,
    ) -> Result<(), CodegenError> {
        if let Some(node) = expression.node {
            self.update_mapping(node);
        }

        match &expression.inner {
            mir::Expression::Function { index, bounds } => {
                write!(self, "{}(", mangle_function(*index))?;
                for bound in bounds {
                    write!(self, "() => ")?;
                    self.write_expression(bound.as_ref())?;
                    write!(self, ", ")?;
                }
                write!(self, ")")?;
            }
            mir::Expression::Bound { local } => {
                self.write_local(*local)?;
                write!(self, "()")?;
            }
            mir::Expression::Call { function, inputs } => {
                self.write_local(*function)?;
                write!(self, "(")?;
                for input in inputs {
                    self.write_local(*input)?;
                    write!(self, ", ")?;
                }
                write!(self, ")")?;
            }
            mir::Expression::Closure(function) => {
                self.write_function(None, function)?;
            }
            mir::Expression::Element { value, index } => {
                write!(self, "(")?;
                self.write_local(*value)?;
                write!(self, ")[{index}]")?;
            }
            mir::Expression::Tuple { elements } => {
                write!(self, "[")?;
                for element in elements {
                    self.write_local(*element)?;
                    write!(self, ", ")?;
                }
                write!(self, "]")?;
            }
            mir::Expression::Marker => {
                write!(self, "undefined")?;
            }
            mir::Expression::Local { local } | mir::Expression::MutableLocal { local } => {
                self.write_local(*local)?;
            }
            mir::Expression::Number { value } => {
                write!(self, "{value}")?;
            }
            mir::Expression::Intrinsic { intrinsic } => {
                self.write_intrinsic(intrinsic)?;
            }
            mir::Expression::String { value } => {
                write!(self, "{value:?}")?;
            }
            mir::Expression::Structure { fields } => {
                write!(self, "{{")?;
                for (index, value) in fields {
                    write!(self, "{index}: ")?;
                    self.write_local(*value)?;
                    write!(self, ", ")?;
                }
                write!(self, "}}")?;
            }
            mir::Expression::Variant { variant, elements } => {
                write!(self, "{{ {:?}: [", mangle_variant(*variant))?;
                for element in elements {
                    self.write_local(*element)?;
                    write!(self, ", ")?;
                }
                write!(self, "] }}")?;
            }
            mir::Expression::VariantElement {
                value,
                variant,
                index,
            } => {
                self.write_local(*value)?;
                write!(self, "[{:?}][{}]", mangle_variant(*variant), index)?;
            }
        }

        Ok(())
    }

    fn write_intrinsic(
        &mut self,
        intrinsic: &mir::Intrinsic<mir::SourceMapped<Box<mir::Expression>>>,
    ) -> Result<(), CodegenError> {
        match intrinsic {
            mir::Intrinsic::Debug { value } => {
                write!(self, "env.debug(")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::StringCount { value } => {
                self.write_expression(value.as_deref())?;
                write!(self, ".length")?;
            }
            mir::Intrinsic::StringConcat { left, right } => {
                write!(self, "(")?;
                self.write_expression(left.as_deref())?;
                write!(self, " + ")?;
                self.write_expression(right.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::External { name, value } => {
                write!(self, "env[")?;
                self.write_expression(name.as_deref())?;
                write!(self, "](")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::NumberToString { value } => {
                self.write_expression(value.as_deref())?;
                write!(self, ".toString()")?;
            }
            mir::Intrinsic::StringToNumber { value } => {
                write!(self, "parseFloat(")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Add { left, right } => {
                write!(self, "(")?;
                self.write_expression(left.as_deref())?;
                write!(self, " + ")?;
                self.write_expression(right.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Sub { left, right } => {
                write!(self, "(")?;
                self.write_expression(left.as_deref())?;
                write!(self, " - ")?;
                self.write_expression(right.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Mul { left, right } => {
                write!(self, "(")?;
                self.write_expression(left.as_deref())?;
                write!(self, " * ")?;
                self.write_expression(right.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Div { left, right } => {
                write!(self, "(")?;
                self.write_expression(left.as_deref())?;
                write!(self, " / ")?;
                self.write_expression(right.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Rem { left, right } => {
                write!(self, "(")?;
                self.write_expression(left.as_deref())?;
                write!(self, " % ")?;
                self.write_expression(right.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Pow { left, right } => {
                write!(self, "Math.pow(")?;
                self.write_expression(left.as_deref())?;
                write!(self, ", ")?;
                self.write_expression(right.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Floor { value } => {
                write!(self, "Math.floor(")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Ceil { value } => {
                write!(self, "Math.ceil(")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Sqrt { value } => {
                write!(self, "Math.sqrt(")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Neg { value } => {
                write!(self, "(-")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Sin { value } => {
                write!(self, "Math.sin(")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Cos { value } => {
                write!(self, "Math.cos(")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Tan { value } => {
                write!(self, "Math.tan(")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::NumberEqual {
                left,
                right,
                true_variant,
                false_variant,
            }
            | mir::Intrinsic::StringEqual {
                left,
                right,
                true_variant,
                false_variant,
            } => {
                write!(self, "((")?;
                self.write_expression(left.as_deref())?;
                write!(self, " === ")?;
                self.write_expression(right.as_deref())?;
                write!(
                    self,
                    ") ? {{ {:?}: [] }} : {{ {:?}: [] }})",
                    mangle_variant(*true_variant),
                    mangle_variant(*false_variant)
                )?;
            }
            mir::Intrinsic::Order {
                left,
                right,
                is_less_than_variant,
                is_equal_variant,
                is_greater_than_variant,
            } => {
                write!(self, "((")?;
                self.write_expression(left.as_deref())?;
                write!(self, " < ")?;
                self.write_expression(right.as_deref())?;
                write!(
                    self,
                    ") ? {{ {:?}: [] }} : (",
                    mangle_variant(*is_less_than_variant)
                )?;
                self.write_expression(left.as_deref())?;
                write!(self, " > ")?;
                self.write_expression(right.as_deref())?;
                write!(
                    self,
                    ") ? {{ {:?}: [] }} : {{ {:?}: [] }})",
                    mangle_variant(*is_greater_than_variant),
                    mangle_variant(*is_equal_variant)
                )?;
            }
            mir::Intrinsic::EmptyList => {
                write!(self, "[]")?;
            }
            mir::Intrinsic::ListCount { value } => {
                self.write_expression(value.as_deref())?;
                write!(self, ".length")?;
            }
            mir::Intrinsic::ListFirst { value } => {
                self.write_expression(value.as_deref())?;
                write!(self, "[0]")?;
            }
            mir::Intrinsic::ListLast { value } => {
                self.write_expression(value.as_deref())?;
                write!(self, ".at(-1)")?;
            }
            mir::Intrinsic::ListInitial { value } => {
                self.write_expression(value.as_deref())?;
                write!(self, ".slice(0, -1)")?;
            }
            mir::Intrinsic::ListTail { value } => {
                self.write_expression(value.as_deref())?;
                write!(self, ".slice(1)")?;
            }
            mir::Intrinsic::ListNth { value, index } => {
                self.write_expression(value.as_deref())?;
                write!(self, "[")?;
                self.write_expression(index.as_deref())?;
                write!(self, "]")?;
            }
            mir::Intrinsic::ListAppend { value, element } => {
                write!(self, "[...")?;
                self.write_expression(value.as_deref())?;
                write!(self, ", ")?;
                self.write_expression(element.as_deref())?;
                write!(self, "]")?;
            }
            mir::Intrinsic::ListPrepend { value, element } => {
                write!(self, "[")?;
                self.write_expression(element.as_deref())?;
                write!(self, ", ...")?;
                self.write_expression(value.as_deref())?;
                write!(self, "]")?;
            }
            mir::Intrinsic::ListInsertAt {
                value,
                index,
                element,
            } => {
                write!(self, "((list, index) => [...list.slice(0, index), ")?;
                self.write_expression(element.as_deref())?;
                write!(self, ", ...list.slice(index)])(")?;
                self.write_expression(value.as_deref())?;
                write!(self, ", ")?;
                self.write_expression(index.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::ListRemoveAt { value, index } => {
                write!(
                    self,
                    "((list, index) => [...list.slice(0, index), ...list.slice(index + 1)])("
                )?;
                self.write_expression(value.as_deref())?;
                write!(self, ", ")?;
                self.write_expression(index.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::StringCharacters { value } => {
                self.write_expression(value.as_deref())?;
                write!(self, ".split(\"\")")?;
            }
            mir::Intrinsic::RandomNumber { min, max } => {
                write!(self, "((min, max) => Math.random() * (max - min) + min)(")?;
                self.write_expression(min.as_deref())?;
                write!(self, ", ")?;
                self.write_expression(max.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Nan => {
                write!(self, "NaN")?;
            }
            mir::Intrinsic::IsNan {
                value,
                true_variant,
                false_variant,
            } => {
                write!(self, "isNaN(")?;
                self.write_expression(value.as_deref())?;
                write!(
                    self,
                    ") ? {{ {:?}: [] }} : {{ {:?}: [] }}",
                    mangle_variant(*true_variant),
                    mangle_variant(*false_variant)
                )?;
            }
            mir::Intrinsic::HashString { value } => {
                writeln!(self, "((s) => {{")?;
                writeln!(self, "let hash = 0;")?;
                writeln!(self, "for (const char of s) {{")?;
                writeln!(self, "hash = (hash << 5) - hash + char;")?;
                writeln!(self, "}}")?;
                writeln!(self, "return hash >>> 0;")?;
                writeln!(self, "}})(")?;
                self.write_expression(value.as_deref())?;
                write!(self, ")")?;
            }
            mir::Intrinsic::Unreachable => {
                writeln!(self, "(() => {{")?;
                writeln!(self, "throw new Error(\"unreachable\");")?;
                writeln!(self, "}})()")?;
            }
        }

        Ok(())
    }

    fn write_local(&mut self, local: mir::LocalIndex) -> Result<(), CodegenError> {
        write!(self, "{}", mangle_local(local))?;
        Ok(())
    }
}

fn mangle_function(index: mir::FunctionIndex) -> String {
    format!("func{}", index.0)
}

fn mangle_local(index: mir::LocalIndex) -> String {
    format!("local{}", index.0)
}

fn mangle_variant(index: usize) -> String {
    format!("variant{index}")
}
