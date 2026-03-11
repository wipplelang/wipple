use crate::{
    codegen::{ir, mangle::Mangle},
    database::Span,
};
use parcel_sourcemap::SourceMap;
use std::fmt::{self, Write as _};

#[derive(Debug, Clone, Copy)]
pub struct Options<'a> {
    pub sourcemap: bool,
    pub trace: &'a [&'a str],
}

pub fn write_to_string(program: &ir::Program, options: Options<'_>) -> String {
    let mut script = String::new();
    Backend::new(&mut script, options).write(program).unwrap();
    script
}

pub struct Backend<'a> {
    writer: Writer<'a>,
    options: Options<'a>,
}

struct Writer<'a> {
    inner: &'a mut (dyn fmt::Write + 'a),
    line: usize,
    column: usize,
    sourcemap: Option<SourceMap>,
}

impl<'a> Backend<'a> {
    pub fn new(w: &'a mut (dyn fmt::Write + 'a), options: Options<'a>) -> Self {
        Backend {
            writer: Writer {
                inner: w,
                sourcemap: options.sourcemap.then(|| SourceMap::new("")),
                line: 0,
                column: 0,
            },
            options,
        }
    }

    pub fn write(&mut self, program: &ir::Program) -> fmt::Result {
        writeln!(self.writer, "let runtime;")?;
        writeln!(self.writer, "export default (r) => {{")?;
        writeln!(self.writer, "runtime = r;")?;

        for expression in &program.files {
            self.write_expression(expression)?;
        }

        writeln!(self.writer, "}};")?;

        for (key, body) in &program.definitions {
            self.write_definition(key, body)?;
        }

        if let Some(sourcemap) = &mut self.writer.sourcemap {
            for file in &program.files {
                let Some(span) = file.span.as_ref() else {
                    continue;
                };

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

    pub fn write_expression(&mut self, expression: &ir::SpannedExpression) -> fmt::Result {
        let should_map = matches!(
            expression.inner,
            ir::Expression::Trace | ir::Expression::NoOp | ir::Expression::Sequence(_)
        );

        if !should_map
            && let Some(span) = &expression.span
            && let Some(sourcemap) = &mut self.writer.sourcemap
        {
            let source = sourcemap
                .get_sources()
                .iter()
                .position(|p| p == &span.path)
                .map_or_else(|| sourcemap.add_source(&span.path), |index| index as u32);

            let name = expression.identifier.as_deref().map(|identifier| {
                sourcemap
                    .get_name_index(identifier)
                    .unwrap_or_else(|| sourcemap.add_name(identifier))
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

        // write!(self.writer, "/* {:?} */", expression.representation)?;

        match &expression.inner {
            ir::Expression::And(expressions) => {
                write!(self.writer, "(true")?;

                for expression in expressions {
                    write!(self.writer, " && ")?;
                    self.write_expression(expression)?;
                }

                write!(self.writer, ")")?;
            }
            ir::Expression::AssignTo(value, variable) => {
                write!(self.writer, "((")?;
                self.write_mangled(variable)?;
                write!(self.writer, " = ")?;
                self.write_expression(value)?;
                write!(self.writer, ") || true)")?;
            }
            ir::Expression::AssignToMutable(value, variable) => {
                write!(self.writer, "((")?;
                self.write_mangled(variable)?;
                write!(self.writer, " = {{ value: ")?;
                self.write_expression(value)?;
                write!(self.writer, " }}) || true)")?;
            }
            ir::Expression::Bound(_) => panic!("bounds must be resolved"),
            ir::Expression::Call(function, inputs) => {
                self.write_expression(function)?;
                write!(self.writer, "(")?;

                for input in inputs {
                    self.write_expression(input)?;
                    write!(self.writer, ", ")?;
                }

                write!(self.writer, ")")?;
            }
            ir::Expression::Concat(expressions) => {
                write!(self.writer, "(\"\"")?;

                for expression in expressions {
                    write!(self.writer, " + ")?;
                    self.write_expression(expression)?;
                }

                write!(self.writer, ")")?;
            }
            ir::Expression::Constant(key) => {
                self.write_mangled(key)?;
                write!(self.writer, "()")?;
            }
            ir::Expression::Declare(variable) => {
                write!(self.writer, "let ")?;
                self.write_mangled(variable)?;
            }
            ir::Expression::EqualToNumber(value, expected) => {
                write!(self.writer, "(")?;
                self.write_expression(value)?;
                write!(self.writer, " === {})", expected)?;
            }
            ir::Expression::EqualToString(value, expected) => {
                write!(self.writer, "(")?;
                self.write_expression(value)?;
                write!(self.writer, " === {})", serde_json::json!(expected))?;
            }
            ir::Expression::EqualToVariant(value, expected) => {
                write!(self.writer, "(")?;
                self.write_expression(value)?;
                write!(self.writer, "[runtime.variant] === {})", expected)?;
            }
            ir::Expression::Field(value, field) => {
                self.write_expression(value)?;
                write!(self.writer, "[{}]", serde_json::json!(field))?;
            }
            ir::Expression::Function(inputs, statements, _captures) => {
                write!(self.writer, "((")?;
                for input in inputs {
                    self.write_mangled(input)?;
                    write!(self.writer, ", ")?;
                }
                writeln!(self.writer, ") => {{")?;
                self.write_statements(statements)?;
                write!(self.writer, "}})")?;
            }
            ir::Expression::If(arms, last) => {
                for (pattern, value) in arms {
                    write!(self.writer, "if (")?;
                    self.write_expression(pattern)?;
                    write!(self.writer, ") {{")?;
                    if let Some(value) = value {
                        write!(self.writer, "return ")?;
                        self.write_expression(value)?;
                        write!(self.writer, ";")?;
                    }
                    write!(self.writer, "}}")?;
                }

                write!(self.writer, "else {{ ")?;
                if let Some(last) = last {
                    write!(self.writer, "return ")?;
                    self.write_expression(last)?;
                } else {
                    write!(self.writer, "throw new Error(\"unreachable\")")?;
                }
                write!(self.writer, "; }}")?;
            }
            ir::Expression::Index(value, index) => {
                self.write_expression(value)?;
                write!(self.writer, "[{}]", index)?;
            }
            ir::Expression::List(elements) => {
                write!(self.writer, "[")?;
                for element in elements {
                    self.write_expression(element)?;
                    write!(self.writer, ", ")?;
                }
                write!(self.writer, "]")?;
            }
            ir::Expression::Marker => {
                write!(self.writer, "null")?;
            }
            ir::Expression::Mutable(variable) => {
                self.write_mangled(variable)?;
                write!(self.writer, ".value")?;
            }
            ir::Expression::Mutate(variable, value) => {
                write!(self.writer, "((")?;
                self.write_mangled(variable)?;
                write!(self.writer, ".value = ")?;
                self.write_mangled(value)?;
                write!(self.writer, ") || true)")?;
            }
            ir::Expression::Number(number) => {
                write!(self.writer, "{}", number)?;
            }
            ir::Expression::NoOp => panic!("NoOp should be filtered out"),
            ir::Expression::Or(patterns) => {
                write!(self.writer, "(false")?;

                for pattern in patterns {
                    write!(self.writer, " || (")?;
                    self.write_expression(pattern)?;
                    write!(self.writer, ")")?;
                }

                write!(self.writer, ")")?;
            }
            ir::Expression::Return(value) => {
                write!(self.writer, "return ")?;
                self.write_expression(value)?;
            }
            ir::Expression::Runtime(name, inputs) => {
                write!(
                    self.writer,
                    "runtime.{}(",
                    convert_case::ccase!(kebab -> camel, name)
                )?;

                for input in inputs {
                    self.write_expression(input)?;
                    write!(self.writer, ", ")?;
                }

                write!(self.writer, ")")?;
            }
            ir::Expression::Scope(statements) => {
                write!(self.writer, "(() => {{")?;
                self.write_statements(statements)?;
                write!(self.writer, "}})()")?;
            }
            ir::Expression::Sequence(statements) => {
                self.write_statements(statements)?;
            }
            ir::Expression::String(string) => {
                write!(self.writer, "{}", serde_json::json!(string))?;
            }
            ir::Expression::Structure(fields) => {
                write!(self.writer, "{{")?;
                for (name, value) in fields {
                    write!(self.writer, "{}: ", serde_json::json!(name))?;
                    self.write_expression(value)?;
                    write!(self.writer, ", ")?;
                }
                write!(self.writer, "}}")?;
            }
            ir::Expression::Trace => {
                if let Some(span) = &expression.span
                    && self.can_trace(span)
                {
                    write!(
                        self.writer,
                        "runtime.trace({})",
                        serde_json::json!(format!("{}", serde_json::json!(span.start)))
                    )?;
                }
            }
            ir::Expression::Variable(node) => {
                self.write_mangled(node)?;
            }
            ir::Expression::Variant(index, elements) => {
                write!(self.writer, "runtime.variant({}, [", index)?;
                for element in elements {
                    self.write_expression(element)?;
                    write!(self.writer, ", ")?;
                }
                write!(self.writer, "])")?;
            }
        }

        Ok(())
    }

    fn write_statements<'e>(
        &mut self,
        statements: impl IntoIterator<Item = &'e ir::SpannedExpression>,
    ) -> fmt::Result {
        for statement in statements {
            if let ir::Expression::NoOp = statement.inner {
                continue;
            }

            if let ir::Expression::Trace = statement.inner
                && statement
                    .span
                    .as_ref()
                    .is_none_or(|span| !self.can_trace(span))
            {
                continue;
            }

            self.write_expression(statement)?;
            writeln!(self.writer, ";")?;
        }

        Ok(())
    }

    pub fn write_definition(
        &mut self,
        key: &ir::DefinitionKey,
        body: &ir::SpannedExpression,
    ) -> fmt::Result {
        write!(self.writer, "function ")?;
        self.write_mangled(key)?;
        writeln!(self.writer, "() {{")?;
        write!(self.writer, "return ")?;
        self.write_expression(body)?;
        writeln!(self.writer, ";")?;
        writeln!(self.writer, "}}")?;

        Ok(())
    }

    fn write_mangled(&mut self, m: &impl Mangle) -> fmt::Result {
        write!(self.writer, "{}", m.mangle())
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
