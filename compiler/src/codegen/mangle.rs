use crate::{
    codegen::ir::{DefinitionKey, Type},
    database::NodeRef,
};
use std::fmt::Write as _;

pub trait Mangle {
    fn mangle(&self) -> String;
}

impl Mangle for NodeRef {
    fn mangle(&self) -> String {
        format!("_{}", self.id())
    }
}

impl Mangle for DefinitionKey {
    fn mangle(&self) -> String {
        let mut s = self.node.mangle();

        for (parameter, representation) in &self.substitutions {
            write!(s, "_{}_{}", parameter.id(), representation.mangle()).unwrap();
        }

        // Bounds do not need to be part of the mangled ID because instances are
        // globally unique

        s
    }
}

impl Mangle for Type {
    fn mangle(&self) -> String {
        match self {
            Type::Named(node, parameters) => {
                let mut s = format!("type{}", node.mangle());

                for parameter in parameters {
                    write!(s, "_{}", parameter.mangle()).unwrap();
                }

                s
            }
            Type::Tuple(elements) => {
                let mut s = String::from("tuple");

                for element in elements {
                    write!(s, "_{}", element.mangle()).unwrap();
                }

                s
            }
            Type::Function(inputs, output) => {
                let mut s = String::from("function");

                for input in inputs {
                    write!(s, "_{}", input.mangle()).unwrap();
                }

                write!(s, "_{}", output.mangle()).unwrap();

                s
            }
            Type::Parameter(node) => format!("parameter{}", node.mangle()),
        }
    }
}
