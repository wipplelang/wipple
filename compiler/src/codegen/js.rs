use crate::{
    codegen::{Options, ir},
    database::{NodeRef, Span},
};
use parcel_sourcemap::SourceMap;
use std::{
    collections::HashSet,
    fmt::{self, Write as _},
};

pub struct JsBackend<'a> {
    options: &'a Options<'a>,
    writer: &'a mut (dyn fmt::Write + 'a),
    line: usize,
    column: usize,
    sourcemap: Option<SourceMap>,
}

impl<'a> JsBackend<'a> {
    pub fn new(options: &'a Options<'a>, w: &'a mut (dyn fmt::Write + 'a)) -> Self {
        JsBackend {
            options,
            writer: w,
            sourcemap: options.sourcemap.then(|| SourceMap::new("")),
            line: 0,
            column: 0,
        }
    }

    pub fn write_program(&mut self, program: &ir::Program) -> fmt::Result {
        if self.options.module {
            writeln!(self, "let __wipple_env, __wipple_proxy;")?;
            writeln!(self, "async function __wipple_main(env, proxy) {{")?;
            writeln!(self, "__wipple_env = env;")?;
            writeln!(self, "__wipple_proxy = proxy;")?;
        } else {
            writeln!(self, "async function __wipple_main() {{")?;
        }

        for expression in &program.files {
            self.write_expression(expression)?;
        }

        writeln!(self, "}};")?;

        for (node, body) in &program.definitions {
            self.write_definition(node, body)?;
        }

        write!(self, "{}", self.options.core)?;
        self.write_intrinsics(&program.intrinsics)?;

        if self.options.module {
            writeln!(self, "export default __wipple_main;")?;
        } else {
            writeln!(self, "__wipple_main();")?;
        }

        if let Some(sourcemap) = &mut self.sourcemap {
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
                self,
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
            && let Some(sourcemap) = &mut self.sourcemap
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
                self.line as u32,
                self.column as u32,
                Some(parcel_sourcemap::OriginalLocation {
                    original_line: span.start.line as u32 - 1,
                    original_column: span.start.column as u32 - 1,
                    source,
                    name,
                }),
            );
        }

        match &expression.inner {
            ir::Expression::And(expressions) => {
                write!(self, "(true")?;

                for expression in expressions {
                    write!(self, " && ")?;
                    self.write_expression(expression)?;
                }

                write!(self, ")")?;
            }
            ir::Expression::AssignTo(value, variable) => {
                write!(self, "((")?;
                self.write_node(variable)?;
                write!(self, " = ")?;
                self.write_expression(value)?;
                write!(self, ") || true)")?;
            }
            ir::Expression::Bound(bound) => {
                write!(self, "await __wipple_bounds.")?;
                self.write_node(bound)?;
                write!(self, "()")?;
            }
            ir::Expression::Call(function, inputs) => {
                write!(self, "await (")?;
                self.write_expression(function)?;
                write!(self, ")(")?;

                for input in inputs {
                    self.write_expression(input)?;
                    write!(self, ", ")?;
                }

                write!(self, ")")?;
            }
            ir::Expression::Concat(expressions) => {
                write!(self, "(\"\"")?;

                for expression in expressions {
                    write!(self, " + ")?;
                    self.write_expression(expression)?;
                }

                write!(self, ")")?;
            }
            ir::Expression::If(arms, last) => {
                for (pattern, value) in arms {
                    write!(self, "if (")?;
                    self.write_expression(pattern)?;
                    write!(self, ") {{")?;
                    if let Some(value) = value {
                        write!(self, "return ")?;
                        self.write_expression(value)?;
                        write!(self, ";")?;
                    }
                    write!(self, "}}")?;
                }

                write!(self, "else {{ ")?;
                if let Some(last) = last {
                    write!(self, "return ")?;
                    self.write_expression(last)?;
                } else {
                    write!(self, "throw new Error(\"unreachable\")")?;
                }
                write!(self, "; }}")?;
            }
            ir::Expression::Declare(variable) => {
                write!(self, "var ")?;
                self.write_node(variable)?;
            }
            ir::Expression::EqualToNumber(value, expected) => {
                write!(self, "(")?;
                self.write_expression(value)?;
                write!(self, " === {})", expected)?;
            }
            ir::Expression::EqualToString(value, expected) => {
                write!(self, "(")?;
                self.write_expression(value)?;
                write!(self, " === {})", serde_json::json!(expected))?;
            }
            ir::Expression::EqualToVariant(value, expected) => {
                write!(self, "(")?;
                self.write_expression(value)?;
                write!(self, "[__wipple_variant] === {})", expected)?;
            }
            ir::Expression::Field(value, field) => {
                self.write_expression(value)?;
                write!(self, "[{}]", serde_json::json!(field))?;
            }
            ir::Expression::Function(inputs, output) => {
                write!(self, "(async (")?;
                for input in inputs {
                    self.write_node(input)?;
                    write!(self, ", ")?;
                }
                writeln!(self, ") => {{")?;
                self.write_expression(output)?;
                write!(self, "}})")?;
            }
            ir::Expression::Identifier(node) => {
                self.write_node(node)?;
            }
            ir::Expression::Index(value, index) => {
                self.write_expression(value)?;
                write!(self, "[{}]", index)?;
            }
            ir::Expression::List(elements) => {
                write!(self, "[")?;
                for element in elements {
                    self.write_expression(element)?;
                    write!(self, ", ")?;
                }
                write!(self, "]")?;
            }
            ir::Expression::Marker => {
                write!(self, "null")?;
            }
            ir::Expression::Number(number) => {
                write!(self, "{}", number)?;
            }
            ir::Expression::NoOp => panic!("NoOp should be filtered out"),
            ir::Expression::Or(patterns) => {
                write!(self, "(false")?;

                for pattern in patterns {
                    write!(self, " || (")?;
                    self.write_expression(pattern)?;
                    write!(self, ")")?;
                }

                write!(self, ")")?;
            }
            ir::Expression::Return(value) => {
                write!(self, "return")?;

                if let Some(value) = value {
                    write!(self, " ")?;
                    self.write_expression(value)?;
                }
            }
            ir::Expression::Runtime(name, inputs) => {
                write!(self, "await __wipple_runtime_{}(", name.replace('-', "_"))?;

                for input in inputs {
                    self.write_expression(input)?;
                    write!(self, ", ")?;
                }

                write!(self, ")")?;
            }
            ir::Expression::Sequence(statements) => {
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
                    writeln!(self, ";")?;
                }
            }
            ir::Expression::String(string) => {
                write!(self, "{}", serde_json::json!(string))?;
            }
            ir::Expression::Structure(fields) => {
                write!(self, "{{")?;
                for (name, value) in fields {
                    write!(self, "{}: ", serde_json::json!(name))?;
                    self.write_expression(value)?;
                    write!(self, ", ")?;
                }
                write!(self, "}}")?;
            }
            ir::Expression::Trace => {
                if let Some(span) = &expression.span
                    && self.can_trace(span)
                {
                    write!(
                        self,
                        "__wipple_env.trace({})",
                        serde_json::json!(span.start)
                    )?;
                }
            }
            ir::Expression::Variant(index, elements) => {
                write!(self, "__wipple_variant({}, [", index)?;
                for element in elements {
                    self.write_expression(element)?;
                    write!(self, ", ")?;
                }
                write!(self, "])")?;
            }
        }

        Ok(())
    }

    pub fn write_definition(
        &mut self,
        node: &NodeRef,
        body: &ir::SpannedExpression,
    ) -> fmt::Result {
        write!(self, "async function ")?;
        self.write_node(node)?;
        writeln!(self, "(__wipple_bounds) {{")?;
        write!(self, "return ")?;
        self.write_expression(body)?;
        writeln!(self, ";")?;
        writeln!(self, "}}")?;

        Ok(())
    }

    fn write_node(&mut self, node: &NodeRef) -> fmt::Result {
        write!(self, "_{}", node.id())
    }

    fn write_intrinsics<'s>(
        &mut self,
        reachable: impl IntoIterator<Item = &'s String>,
    ) -> fmt::Result {
        let reachable = reachable
            .into_iter()
            .map(|s| s.replace('-', "_"))
            .collect::<HashSet<_>>();

        let prefix = "const __wipple_runtime_";

        let mut indices = self.options.runtime.match_indices(prefix).peekable();
        while let Some(start) = indices.next() {
            let end = indices.peek();

            let function = match end {
                Some(end) => &self.options.runtime[start.0..end.0],
                None => &self.options.runtime[start.0..],
            };

            let function = function.strip_prefix(prefix).unwrap();

            if reachable.iter().any(|f| function.starts_with(f)) {
                write!(self, "{}{}", prefix, function)?;
            }
        }

        Ok(())
    }

    fn can_trace(&self, span: &Span) -> bool {
        self.options.trace.contains(&span.path.as_str())
    }
}

impl fmt::Write for JsBackend<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }

        self.writer.write_str(s)
    }
}
