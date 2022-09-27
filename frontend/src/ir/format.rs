use super::{Program, Statement};

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut i = String::new();

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

        macro_rules! writeln_indented {
            ($f:ident, $i:ident) => {{
                writeln!($f, "{}", $i)?;
            }};
            ($f:ident, $i:ident, $($t:tt)*) => {{
                write!($f, "{}", $i)?;
                writeln!($f, $($t)*)?
            }};
        }

        writeln_indented!(f, i, "entrypoint = {}", self.entrypoint.0);
        writeln_indented!(f, i);

        for (label, statements) in &self.labels {
            writeln_indented!(f, i, "#{}:", label.0);

            indent!(i);

            for statement in statements {
                match statement {
                    Statement::Copy => writeln_indented!(f, i, "copy"),
                    Statement::Drop => writeln_indented!(f, i, "drop"),
                    Statement::Initialize(var) => writeln_indented!(f, i, "initialize {}", var.0),
                    Statement::Goto(label, tail) => {
                        if *tail {
                            writeln_indented!(f, i, "tail goto #{}", label.0);
                        } else {
                            writeln_indented!(f, i, "goto #{}", label.0);
                        }
                    }
                    Statement::Sub(label, tail) => {
                        if *tail {
                            writeln_indented!(f, i, "tail sub #{}", label.0);
                        } else {
                            writeln_indented!(f, i, "sub #{}", label.0);
                        }
                    }
                    Statement::If(then_label, else_label) => {
                        writeln_indented!(f, i, "if #{} #{}", then_label.0, else_label.0)
                    }
                    Statement::Marker => writeln_indented!(f, i, "marker"),
                    Statement::Closure(id) => {
                        writeln_indented!(f, i, "closure {}", id.0);
                    }
                    Statement::Variable(id) => {
                        writeln_indented!(f, i, "variable {}", id.0)
                    }
                    Statement::Constant(id) => {
                        writeln_indented!(f, i, "constant {}", id.0)
                    }
                    Statement::Number(number) => {
                        writeln_indented!(f, i, "number {}", number)
                    }
                    Statement::Integer(integer) => {
                        writeln_indented!(f, i, "integer {}", integer);
                    }
                    Statement::Natural(natural) => {
                        writeln_indented!(f, i, "natural {}", natural);
                    }
                    Statement::Byte(byte) => writeln_indented!(f, i, "byte {}", byte),
                    Statement::Signed(signed) => {
                        writeln_indented!(f, i, "signed {}", signed)
                    }
                    Statement::Unsigned(unsigned) => {
                        writeln_indented!(f, i, "unsigned {}", unsigned);
                    }
                    Statement::Float(float) => writeln_indented!(f, i, "float {}", float),
                    Statement::Double(double) => {
                        writeln_indented!(f, i, "double {}", double)
                    }
                    Statement::Text(text) => writeln_indented!(f, i, "text {:?}", text),
                    Statement::Call(tail) => {
                        if *tail {
                            writeln_indented!(f, i, "tail call");
                        } else {
                            writeln_indented!(f, i, "call");
                        }
                    }
                    Statement::External(lib, identifier, count) => {
                        writeln_indented!(f, i, "external {:?} {:?} {}", lib, identifier, count);
                    }
                    Statement::Tuple(count) => writeln_indented!(f, i, "tuple {}", count),
                    Statement::Structure(count) => {
                        writeln_indented!(f, i, "structure {}", count);
                    }
                    Statement::Variant(discriminant, count) => {
                        writeln_indented!(f, i, "variant {} {}", discriminant, count);
                    }
                    Statement::TupleElement(index) => {
                        writeln_indented!(f, i, "tuple element {}", index);
                    }
                    Statement::StructureElement(index) => {
                        writeln_indented!(f, i, "structure element {}", index);
                    }
                    Statement::VariantElement(discriminant, index) => {
                        writeln_indented!(f, i, "variant element {} {}", discriminant, index);
                    }
                    Statement::CompareDiscriminants(index) => {
                        writeln_indented!(f, i, "compare discriminants {}", index);
                    }
                }
            }

            dedent!(i);
        }

        Ok(())
    }
}
