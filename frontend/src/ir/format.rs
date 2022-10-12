use super::*;

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "entrypoint: {}", self.entrypoint)?;

        for (label, (span, statements)) in &self.labels {
            write!(f, "\n{}", label)?;

            if let Some(span) = span {
                write!(f, " ({:?})", span)?;
            }

            writeln!(f, ":")?;

            for statement in statements {
                writeln!(f, "  {}", statement)?;
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::Comment(comment) => write!(f, ";; {}", comment),
            Statement::Copy => write!(f, "copy"),
            Statement::Drop => write!(f, "drop"),
            Statement::Begin => write!(f, "begin"),
            Statement::End => write!(f, "end"),
            Statement::Value(value) => write!(f, "{}", value),
            Statement::Call => write!(f, "call"),
            Statement::TailCall => write!(f, "tail call"),
            Statement::External(abi, identifier, count) => {
                write!(f, "external {:?} {:?} {}", abi, identifier, count)
            }
            Statement::Tuple(count) => write!(f, "tuple {}", count),
            Statement::Structure(count) => write!(f, "structure {}", count),
            Statement::Variant(variant, count) => write!(f, "variant {} {}", variant, count),
            Statement::TupleElement(index) => write!(f, "tuple element {}", index),
            Statement::StructureElement(index) => write!(f, "structure element {}", index),
            Statement::VariantElement(variant, index) => {
                write!(f, "variant element {} {}", variant, index)
            }
            Statement::Initialize(initialize) => write!(f, "initialize ${}", initialize.0),
            Statement::If(variant, label) => write!(f, "if {} {}", variant, label),
            Statement::Jump(label) => write!(f, "jump {}", label),
            Statement::Unreachable => write!(f, "unreachable"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Marker => write!(f, "marker"),
            Value::Text(text) => write!(f, "text {:?}", text),
            Value::Number(number) => write!(f, "number {}", number),
            Value::Integer(integer) => write!(f, "integer {}", integer),
            Value::Natural(natural) => write!(f, "natural {}", natural),
            Value::Byte(byte) => write!(f, "byte {}", byte),
            Value::Signed(signed) => write!(f, "signed {}", signed),
            Value::Unsigned(unsigned) => write!(f, "unsigned {}", unsigned),
            Value::Float(float) => write!(f, "float {}", float),
            Value::Double(double) => write!(f, "double {}", double),
            Value::Variable(variable) => write!(f, "variable ${}", variable.0),
            Value::Constant(label) => write!(f, "constant {}", label),
            Value::Function(label) => write!(f, "function {}", label),
            Value::Closure(label) => write!(f, "closure {}", label),
        }
    }
}
