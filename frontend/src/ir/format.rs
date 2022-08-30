use super::{ExpressionKind, Program, SectionIndex, Statement, Terminator, Type};
use crate::{ComputationId, MonomorphizedConstantId, VariableId};
use std::collections::BTreeMap;

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut i = String::new();

        #[derive(Default)]
        struct Info<'a> {
            computations: BTreeMap<ComputationId, &'a Type>,
        }

        let mut info = Info::default();

        macro_rules! indent {
            ($i:ident) => {
                $i += "\t";
            };
        }

        macro_rules! dedent {
            ($i:ident) => {
                $i.pop().expect("cannot dedent any further");
            };
        }

        macro_rules! write_indented {
            ($f:ident, $i:ident, $($t:tt)*) => {{
                write!($f, "{}", $i)?;
                write!($f, $($t)*)?
            }};
        }

        macro_rules! writeln_indented {
            ($f:ident, $i:ident, $($t:tt)*) => {{
                write!($f, "{}", $i)?;
                writeln!($f, $($t)*)?
            }};
        }

        macro_rules! write_section {
            ($f:ident, $i:ident, $index:ident, $section:ident, $info:expr) => {{
                writeln_indented!($f, $i, "{}:", mangle_section($index));
                indent!($i);

                for statement in &$section.statements {
                    write_statement($f, &$i, statement, $info)?;
                }

                if let Some(terminator) = &$section.terminator {
                    writeln_indented!($f, $i, "{}", terminator);
                } else {
                    writeln_indented!($f, $i, "<no terminator>");
                }

                dedent!($i);
            }};
        }

        fn write_statement<'a>(
            f: &mut std::fmt::Formatter,
            i: &str,
            statement: &'a Statement,
            info: &mut Info<'a>,
        ) -> std::fmt::Result {
            match statement {
                Statement::Compute(id, expr) => {
                    info.computations.insert(*id, &expr.ty);

                    write_indented!(f, i, "{}: {} = ", mangle_computation(*id), expr.ty);

                    match &expr.kind {
                        ExpressionKind::Marker => writeln!(f, "marker")?,
                        ExpressionKind::Constant(id) => writeln!(f, "{}", mangle_constant(*id))?,
                        ExpressionKind::Function(function) => {
                            writeln!(f, "function")?;
                            let mut i = i.to_string();
                            indent!(i);

                            for (index, section) in function.sections.enumerate() {
                                write_section!(f, i, index, section, info);
                            }

                            dedent!(i);
                        }
                        ExpressionKind::Variable(id) => writeln!(f, "{}", mangle_variable(*id))?,
                        ExpressionKind::FunctionInput => {
                            writeln!(f, "{}", mangle_function_input())?
                        }
                        ExpressionKind::Number(number) => writeln!(f, "number {}", number)?,
                        ExpressionKind::Text(text) => writeln!(f, "text {:?}", text)?,
                        ExpressionKind::Call(func, input) => writeln!(
                            f,
                            "call {} {}",
                            mangle_computation(*func),
                            mangle_computation(*input)
                        )?,
                        ExpressionKind::External(lib, identifier, inputs) => writeln!(
                            f,
                            "external {:?} {:?} {}",
                            lib,
                            identifier,
                            inputs
                                .iter()
                                .map(|id| mangle_computation(*id))
                                .collect::<Vec<_>>()
                                .join(" ")
                        )?,
                        ExpressionKind::Tuple(values) => writeln!(
                            f,
                            "tuple {}",
                            values
                                .iter()
                                .map(|id| mangle_computation(*id))
                                .collect::<Vec<_>>()
                                .join(" ")
                        )?,
                        ExpressionKind::Structure(values) => writeln!(
                            f,
                            "structure {}",
                            values
                                .iter()
                                .map(|id| mangle_computation(*id))
                                .collect::<Vec<_>>()
                                .join(" ")
                        )?,
                        ExpressionKind::Variant(discriminant, values) => writeln!(
                            f,
                            "variant {} {}",
                            discriminant,
                            values
                                .iter()
                                .map(|id| mangle_computation(*id))
                                .collect::<Vec<_>>()
                                .join(" ")
                        )?,
                        ExpressionKind::TupleElement(id, index) => {
                            writeln!(f, "tuple-element {} {}", mangle_computation(*id), index)?
                        }
                        ExpressionKind::StructureElement(id, _, index) => {
                            writeln!(f, "structure-element {} {}", mangle_computation(*id), index)?
                        }
                        ExpressionKind::VariantElement(id, _, index) => {
                            writeln!(f, "variant-element {} {}", mangle_computation(*id), index)?
                        }
                        ExpressionKind::Discriminant(id) => {
                            writeln!(f, "discriminant {}", mangle_computation(*id))?
                        }
                        ExpressionKind::CompareDiscriminants(id, index) => writeln!(
                            f,
                            "compare-discriminants {} {}",
                            mangle_computation(*id),
                            index
                        )?,
                    };
                }
                Statement::Initialize(var, computation) => {
                    let ty = info.computations.get(computation).unwrap();

                    writeln_indented!(
                        f,
                        i,
                        "{}: {} = {}",
                        mangle_variable(*var),
                        ty,
                        mangle_computation(*computation)
                    )
                }
                Statement::Drop(vars) => {
                    writeln_indented!(
                        f,
                        i,
                        "drop {}",
                        vars.iter()
                            .map(|var| mangle_variable(*var))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }

            Ok(())
        }

        for (id, constant) in self.constants.iter() {
            writeln_indented!(f, i, "{}:", mangle_constant(*id));

            indent!(i);

            for (index, section) in constant.sections.enumerate() {
                write_section!(f, i, index, section, &mut info);
            }

            dedent!(i);
        }

        writeln_indented!(f, i, "entrypoint:");

        indent!(i);

        for (index, section) in self.entrypoint.enumerate() {
            write_section!(f, i, index, section, &mut info);
        }

        dedent!(i);

        Ok(())
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

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "number"),
            Type::Integer => write!(f, "integer"),
            Type::Text => write!(f, "text"),
            Type::Discriminant => write!(f, "discriminant"),
            Type::Boolean => write!(f, "boolean"),
            Type::Function(input, output) => write!(f, "(function {} {})", input, output),
            Type::Tuple(tys) => {
                write!(f, "(")?;

                for ty in tys {
                    write!(f, "{}, ", ty)?;
                }

                write!(f, ")")?;

                Ok(())
            }
            Type::List(ty) => write!(f, "(list {})", ty),
            Type::Mutable(ty) => write!(f, "(mutable {})", ty),
            Type::Marker => write!(f, "marker"),
            Type::Structure(id, _) => write!(f, "(structure {})", id.0),
            Type::Enumeration(id, _) => write!(f, "(enumeration {})", id.0),
            Type::Recursive(id) => write!(f, "(recursive {})", id.0),
            Type::Unreachable => write!(f, "unreachable"),
        }
    }
}

impl std::fmt::Display for SectionIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", mangle_section(*self))
    }
}

fn mangle_constant(id: MonomorphizedConstantId) -> String {
    format!("C{}", id.0)
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

fn mangle_computation(id: ComputationId) -> String {
    format!("${}", id.0)
}
