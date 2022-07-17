use super::{Expression, Program, Statement, Terminator};
use crate::{ir::SectionIndex, IrComputationId, VariableId};
use std::io::Write;

impl Program {
    pub fn to_writer_pretty(&self, w: &mut impl Write) -> std::io::Result<()> {
        let mut indentation = String::new();

        macro_rules! indent {
            () => {
                indentation += "\t";
            };
        }

        macro_rules! dedent {
            () => {
                indentation.pop().expect("cannot dedent any further");
            };
        }

        macro_rules! write_indented {
            ($($t:tt)*) => {{
                write!(w, "{}", indentation)?;
                writeln!(w, $($t)*)?
            }};
        }

        macro_rules! write_section {
            ($index:ident, $section:ident) => {{
                write_indented!("{}:", mangle_section($index));
                indent!();

                for statement in &$section.statements {
                    match statement {
                        Statement::Compute(id, expr) => {
                            let expr = match expr {
                                Expression::Marker => String::from("marker"),
                                Expression::Constant(id) => mangle_constant(*id),
                                Expression::Function(id) => mangle_function(*id),
                                Expression::Variable(id) => mangle_variable(*id),
                                Expression::FunctionInput => mangle_function_input(),
                                Expression::Number(number) => format!("number {}", number),
                                Expression::Text(text) => format!("text \"{}\"", text),
                                Expression::Call(func, input) => format!(
                                    "call {} {}",
                                    mangle_computation(*func),
                                    mangle_computation(*input)
                                ),
                                Expression::External(abi, identifier, inputs) => format!(
                                    "external {:?} {:?} {}",
                                    abi,
                                    identifier,
                                    inputs
                                        .iter()
                                        .map(|id| mangle_computation(*id))
                                        .collect::<Vec<_>>()
                                        .join(" ")
                                ),
                            };

                            write_indented!("{} = {}", mangle_computation(*id), expr);
                        }
                        Statement::Initialize(var, computation) => {
                            write_indented!(
                                "{} = {}",
                                mangle_variable(*var),
                                mangle_computation(*computation)
                            );
                        }
                    }
                }

                if let Some(terminator) = &$section.terminator {
                    match terminator {
                        Terminator::If(condition, then_branch, else_branch) => {
                            write_indented!(
                                "if {} then {} else {}",
                                mangle_computation(*condition),
                                mangle_section(*then_branch),
                                mangle_section(*else_branch)
                            )
                        }
                        Terminator::Return(id) => {
                            write_indented!("return {}", mangle_computation(*id));
                        }
                        Terminator::Unreachable => write_indented!("unreachable"),
                        Terminator::Goto(section) => {
                            write_indented!("goto {}", mangle_section(*section));
                        }
                    }
                }

                dedent!();
            }};
        }

        for (id, constant) in self.constants.iter().enumerate() {
            write_indented!("{}:", mangle_constant(id));

            indent!();

            for (index, section) in constant.sections.enumerate() {
                write_section!(index, section);
            }

            dedent!();
        }

        for (id, function) in self.functions.iter().enumerate() {
            write_indented!(
                "{}{}:",
                mangle_function(id),
                (!function.captures.is_empty())
                    .then(|| {
                        format!(
                            " (captures {})",
                            function
                                .captures
                                .iter()
                                .map(|var| mangle_variable(*var))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    })
                    .unwrap_or_default()
            );

            indent!();

            for (index, section) in function.sections.enumerate() {
                write_section!(index, section);
            }

            dedent!();
        }

        write_indented!("entrypoint:");

        indent!();

        for (index, section) in self.entrypoint.enumerate() {
            write_section!(index, section);
        }

        dedent!();

        Ok(())
    }
}

fn mangle_constant(id: usize) -> String {
    format!("C{}", id)
}

fn mangle_function(id: usize) -> String {
    format!("F{}", id)
}

fn mangle_variable(id: VariableId) -> String {
    format!("@{}", id.0)
}

fn mangle_function_input() -> String {
    String::from("I")
}

fn mangle_section(index: SectionIndex) -> String {
    format!("#{}", index.0)
}

fn mangle_computation(id: IrComputationId) -> String {
    format!("${}", id.0)
}
