use crate::{codegen::ir, database::NodeRef};
use std::fmt::Write as _;

pub trait Mangle {
    fn mangle(&self) -> String;
}

impl Mangle for NodeRef {
    fn mangle(&self) -> String {
        format!("_{}", self.id())
    }
}

impl Mangle for ir::DefinitionKey {
    fn mangle(&self) -> String {
        let mut s = self.node.mangle();

        for (parameter, ty) in &self.substitutions {
            write!(s, "_{}_{}", parameter.id(), ty.mangle_nominal()).unwrap();
        }

        // Bounds do not need to be part of the mangled ID because instances are
        // globally unique

        s
    }
}

impl ir::Type {
    pub fn mangle_nominal(&self) -> String {
        self.mangle_inner(true).unwrap()
    }

    pub fn mangle_structural(&self) -> Option<String> {
        self.mangle_inner(false)
    }

    fn mangle_inner(&self, nominal: bool) -> Option<String> {
        match self {
            ir::Type::Named(node, parameters, flags) => {
                if !nominal && flags.intrinsic {
                    None
                } else {
                    let mut s = format!("type{}", node.mangle());

                    for parameter in parameters {
                        write!(s, "_{}", parameter.mangle_nominal()).unwrap();
                    }

                    Some(s)
                }
            }
            ir::Type::Tuple(elements) => {
                let mut s = String::from("tuple");

                for element in elements {
                    write!(s, "_{}", element.mangle_nominal()).unwrap();
                }

                Some(s)
            }
            ir::Type::Function(inputs, output) => {
                let mut s = String::from("function");

                for input in inputs {
                    write!(s, "_{}", input.mangle_nominal()).unwrap();
                }

                write!(s, "_{}", output.mangle_nominal()).unwrap();

                Some(s)
            }
            ir::Type::Parameter(node) => Some(format!("parameter{}", node.mangle())),
        }
    }
}
