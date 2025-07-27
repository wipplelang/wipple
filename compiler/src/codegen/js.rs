use crate::{
    codegen::{Condition, Executable, Expression, Statement, Type},
    util::WithInfo,
};
use serde::Deserialize;
use std::fmt::{self, Write};

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default)]
pub struct Options {
    pub mode: Mode,
    pub debug: bool,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Mode {
    #[default]
    Module,
    Iife(String),
}

impl Executable {
    pub fn to_js(&self, options: Options) -> String {
        let mut output = String::new();
        self.write_js(&mut output, options).unwrap();
        output
    }

    pub fn write_js(&self, w: &mut dyn Write, options: Options) -> fmt::Result {
        match &options.mode {
            Mode::Module => writeln!(w, "export default async function(runtime) {{")?,
            Mode::Iife(_) => writeln!(w, "(async (runtime) => {{")?,
        }

        for (r#trait, instances) in &self.instances {
            writeln!(w, "const {trait} = [")?;
            for instance in instances {
                write!(w, "[{}, {{", instance.id)?;
                for (parameter, ty) in &instance.substitutions {
                    write!(w, "{parameter}: ")?;
                    self.write_type(w, ty)?;
                    write!(w, ", ")?;
                }
                writeln!(w, "}}],")?;
            }
            writeln!(w, "];")?;
        }

        for (id, (path, statements)) in &self.items {
            if options.debug {
                write!(
                    w,
                    "/**! {} @ {:?} ({}) */ ",
                    statements.info.path, statements.info.span, path
                )?;
            }

            writeln!(w, "async function {id}(types) {{")?;
            for statement in &statements.item {
                self.write_statement(w, statement, &options)?;
            }
            writeln!(w, "}}")?;
        }

        writeln!(w, "return (await {}({{}}))();", self.entrypoint)?;

        match options.mode {
            Mode::Module => writeln!(w, "}};")?,
            Mode::Iife(arg) => writeln!(w, "}})({arg});")?,
        }

        Ok(())
    }

    fn write_statement(
        &self,
        w: &mut dyn Write,
        statement: &Statement,
        options: &Options,
    ) -> fmt::Result {
        match statement {
            Statement::If {
                conditions,
                statements,
                is_else,
            } => {
                if *is_else {
                    write!(w, "else ")?;
                }

                if !conditions.is_empty() {
                    write!(w, "if (")?;

                    for (index, condition) in conditions.iter().enumerate() {
                        if index > 0 {
                            write!(w, " && ")?;
                        }

                        self.write_condition(w, condition, options)?;
                    }

                    writeln!(w, ") {{")?;
                }

                for statement in statements {
                    self.write_statement(w, statement, options)?;
                }

                if !conditions.is_empty() {
                    writeln!(w, "}}")?;
                }
            }
            Statement::Expression(expression) => {
                self.write_expression(w, expression.as_ref(), options)?;
                writeln!(w, ";")?;
            }
            Statement::Return(expression) => {
                write!(w, "return ")?;
                self.write_expression(w, expression.as_ref(), options)?;
                writeln!(w, ";")?;
            }
        }

        Ok(())
    }

    fn write_expression(
        &self,
        w: &mut dyn Write,
        expression: WithInfo<&Expression>,
        options: &Options,
    ) -> fmt::Result {
        if options.debug {
            write!(
                w,
                "/*! {} @ {:?} */ ",
                expression.info.path, expression.info.span
            )?;
        }

        match expression.item {
            Expression::Variable(id) => {
                write!(w, "{id}")?;
            }
            Expression::Constant(id, substitutions) => {
                write!(w, "await runtime.constant({id}, types, {{")?;
                for (parameter, ty) in substitutions {
                    write!(w, "{parameter}: ")?;
                    self.write_type(w, ty)?;
                    write!(w, ", ")?;
                }
                write!(w, "}})")?;
            }
            Expression::Trait(id, substitutions) => {
                write!(w, "await runtime.trait({id}, types, {{")?;
                for (parameter, ty) in substitutions {
                    write!(w, "{parameter}: ")?;
                    self.write_type(w, ty)?;
                    write!(w, ", ")?;
                }
                write!(w, "}})")?;
            }
            Expression::Call(function, inputs) => {
                write!(w, "await (")?;
                self.write_expression(w, function.as_ref().as_ref(), options)?;
                write!(w, ")(")?;
                for input in inputs {
                    self.write_expression(w, input.as_ref(), options)?;
                    write!(w, ", ")?;
                }
                write!(w, ")")?;
            }
            Expression::Marker => {
                write!(w, "null")?;
            }
            Expression::List(items) => {
                write!(w, "[")?;
                for item in items {
                    self.write_expression(w, item.as_ref(), options)?;
                    write!(w, ", ")?;
                }
                write!(w, "]")?;
            }
            Expression::Variant(index, items) => {
                write!(w, "runtime.variant({index}, [")?;
                for item in items {
                    self.write_expression(w, item.as_ref(), options)?;
                    write!(w, ", ")?;
                }
                write!(w, "])")?;
            }
            Expression::Intrinsic(name, inputs) => {
                write!(w, "await runtime[{name:?}](")?;
                for input in inputs {
                    self.write_expression(w, input.as_ref(), options)?;
                    write!(w, ", ")?;
                }
                write!(w, ")")?;
            }
            Expression::Text(value) => {
                write!(w, "{value:?}")?;
            }
            Expression::Number(value) => {
                write!(w, "{value}")?;
            }
            Expression::Format(segments, trailing) => {
                write!(w, "(")?;
                for (index, (text, input)) in segments.iter().enumerate() {
                    if index > 0 {
                        write!(w, " + ")?;
                    }

                    write!(w, "{text:?} + ")?;
                    self.write_expression(w, input.as_ref(), options)?;
                }
                write!(w, " + {trailing:?})")?;
            }
            Expression::Function(inputs, vars, statements) => {
                write!(w, "(async (")?;
                for input in inputs {
                    write!(w, "{input},")?;
                }
                writeln!(w, ") => {{")?;
                for var in vars {
                    writeln!(w, "let {var};")?;
                }
                for statement in statements {
                    self.write_statement(w, statement, options)?;
                }
                write!(w, "}})")?;
            }
        }

        Ok(())
    }

    fn write_condition(
        &self,
        w: &mut dyn Write,
        condition: &Condition,
        options: &Options,
    ) -> fmt::Result {
        match condition {
            Condition::Assign(id, value) => {
                write!(w, "(({id} = ")?;
                self.write_expression(w, value.as_ref(), options)?;
                write!(w, ") || true)")?; // ensure assignments never fail
            }
            Condition::Element(id, parent, index) => {
                write!(w, "(({id} = {parent}[{index}]) || true)")?;
            }
            Condition::IsVariant(id, variant) => {
                write!(w, "({id}[runtime.variant] === {variant})")?;
            }
            Condition::IsNumber(id, value) => {
                write!(w, "{id} === {value}")?;
            }
            Condition::IsText(id, value) => {
                write!(w, "{id} === {value:?}")?;
            }
            Condition::Or(left, right) => {
                write!(w, "(")?;
                for (index, condition) in left.iter().enumerate() {
                    if index > 0 {
                        write!(w, " && ")?;
                    }

                    self.write_condition(w, condition, options)?;
                }
                write!(w, ") || (")?;
                for (index, condition) in right.iter().enumerate() {
                    if index > 0 {
                        write!(w, " && ")?;
                    }
                    self.write_condition(w, condition, options)?;
                }
                write!(w, ")")?;
            }
        }
        Ok(())
    }

    fn write_type(&self, w: &mut dyn Write, ty: &Type) -> fmt::Result {
        write!(w, "{}", serde_json::to_string(ty).unwrap())
    }
}
