use super::*;

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (label, (kind, vars, blocks)) in self.labels.iter().enumerate() {
            writeln!(f, "{} #{} ({} vars):", kind, label, vars)?;

            for (index, bb) in blocks.iter().enumerate() {
                writeln!(f, "  bb{}:", index)?;

                for statement in &bb.statements {
                    writeln!(f, "    {}", statement)?;
                }

                writeln!(f, "    {}", bb.terminator)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for LabelKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LabelKind::Entrypoint => write!(f, "entrypoint"),
            LabelKind::Constant(_) => write!(f, "constant"),
            LabelKind::Function(_, _) => write!(f, "function"),
            LabelKind::Closure(_, _, _) => write!(f, "closure"),
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::Copy => write!(f, "copy"),
            Statement::Drop => write!(f, "drop"),
            Statement::PushFrame => write!(f, "push frame"),
            Statement::PopFrame => write!(f, "pop frame"),
            Statement::Initialize(var) => write!(f, "initialize ${}", var),
            Statement::Free(var) => write!(f, "free ${}", var),
            Statement::Unpack(captures) => write!(f, "unpack {}", captures),
            Statement::Expression(_, expr) => write!(f, "{}", expr),
        }
    }
}

impl std::fmt::Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Terminator::Return => write!(f, "return"),
            Terminator::If(variant, then_label, else_label) => {
                write!(
                    f,
                    "if {} bb{} bb{}",
                    variant.into_inner(),
                    then_label,
                    else_label
                )
            }
            Terminator::Jump(label) => write!(f, "jump bb{}", label),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Marker => write!(f, "marker"),
            Expression::Text(text) => write!(f, "text {:?}", text),
            Expression::Number(number) => write!(f, "number {}", number),
            Expression::Integer(integer) => write!(f, "integer {}", integer),
            Expression::Natural(natural) => write!(f, "natural {}", natural),
            Expression::Byte(byte) => write!(f, "byte {}", byte),
            Expression::Signed(signed) => write!(f, "signed {}", signed),
            Expression::Unsigned(unsigned) => write!(f, "unsigned {}", unsigned),
            Expression::Float(float) => write!(f, "float {}", float),
            Expression::Double(double) => write!(f, "double {}", double),
            Expression::Variable(variable) => write!(f, "variable ${}", variable),
            Expression::Constant(label) => write!(f, "constant #{}", label),
            Expression::Function(label) => write!(f, "function #{}", label),
            Expression::Closure(captures, label) => write!(f, "closure {} #{}", captures, label),
            Expression::Call => write!(f, "call"),
            Expression::External(abi, identifier, count) => {
                write!(f, "external {:?} {:?} {}", abi, identifier, count)
            }
            Expression::Runtime(func, count) => {
                write!(f, "runtime {:?} {}", func.to_string(), count)
            }
            Expression::Tuple(count) => write!(f, "tuple {}", count),
            Expression::Structure(count) => write!(f, "structure {}", count),
            Expression::Variant(variant, count) => {
                write!(f, "variant {} {}", variant.into_inner(), count)
            }
            Expression::TupleElement(index) => write!(f, "tuple element {}", index),
            Expression::StructureElement(index) => {
                write!(f, "structure element {}", index.into_inner())
            }
            Expression::VariantElement(variant, index) => {
                write!(f, "variant element {} {}", variant.into_inner(), index)
            }
            Expression::Reference => write!(f, "reference"),
            Expression::Dereference => write!(f, "dereference"),
        }
    }
}

impl std::fmt::Display for CaptureList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "({})",
            self.0
                .iter()
                .map(|(captured, var)| format!("${} => ${}", captured, var))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
