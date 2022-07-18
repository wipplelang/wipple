use super::{Expression, Program, Statement, Terminator};
use crate::{ir::SectionIndex, IrComputationId, VariableId};

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
                write!(f, "{}", indentation)?;
                writeln!(f, $($t)*)?
            }};
        }

        macro_rules! write_section {
            ($index:ident, $section:ident) => {{
                write_indented!("{}:", mangle_section($index));
                indent!();

                for statement in &$section.statements {
                    write_indented!("{}", statement)
                }

                if let Some(terminator) = &$section.terminator {
                    write_indented!("{}", terminator);
                } else {
                    write_indented!("<no terminator>");
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
                "{}{}{}:",
                mangle_function(id),
                (!function.locals.is_empty())
                    .then(|| {
                        format!(
                            " (locals {})",
                            function
                                .locals
                                .iter()
                                .map(|var| mangle_variable(*var))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    })
                    .unwrap_or_default(),
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

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::Compute(id, expr) => {
                let expr = match expr {
                    Expression::Marker => String::from("marker"),
                    Expression::Constant(id) => mangle_constant(*id),
                    Expression::Function(id) => mangle_function(*id),
                    Expression::Variable(id) => mangle_variable(*id),
                    Expression::FunctionInput => mangle_function_input(),
                    Expression::Number(number) => format!("number {}", number),
                    Expression::Text(text) => format!("text {:?}", text),
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
                    Expression::Tuple(values) => format!(
                        "tuple {}",
                        values
                            .iter()
                            .map(|id| mangle_computation(*id))
                            .collect::<Vec<_>>()
                            .join(" ")
                    ),
                    Expression::Variant(discriminant, values) => format!(
                        "variant {} {}",
                        discriminant,
                        values
                            .iter()
                            .map(|id| mangle_computation(*id))
                            .collect::<Vec<_>>()
                            .join(" ")
                    ),
                    Expression::TupleElement(id, index) => {
                        format!("tuple-element {} {}", mangle_computation(*id), index)
                    }
                    Expression::VariantElement(id, index) => {
                        format!("variant-element {} {}", mangle_computation(*id), index)
                    }
                    Expression::Discriminant(id) => {
                        format!("discriminant {}", mangle_computation(*id))
                    }
                    Expression::CompareDiscriminants(id, index) => {
                        format!(
                            "compare-discriminants {} {}",
                            mangle_computation(*id),
                            index
                        )
                    }
                };

                write!(f, "{} = {}", mangle_computation(*id), expr)
            }
            Statement::Initialize(var, computation) => {
                write!(
                    f,
                    "{} = {}",
                    mangle_variable(*var),
                    mangle_computation(*computation)
                )
            }
        }
    }
}

impl<I: std::fmt::Display> std::fmt::Display for Terminator<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Terminator::If(condition, then_branch, else_branch) => write!(
                f,
                "if {} then {} else {}",
                mangle_computation(*condition),
                then_branch,
                else_branch
            ),
            Terminator::Return(id) => write!(f, "return {}", mangle_computation(*id)),
            Terminator::Unreachable => write!(f, "unreachable"),
            Terminator::Goto(section) => write!(f, "goto {}", section),
        }
    }
}

impl std::fmt::Display for SectionIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", mangle_section(*self))
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
