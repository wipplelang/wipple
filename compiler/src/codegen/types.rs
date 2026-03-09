use crate::{
    codegen::ir,
    database::{Db, NodeRef},
    typecheck::{ConstructedTypeTag, Type},
};
use std::collections::BTreeMap;

impl Type {
    pub fn key(self, db: &Db) -> Option<ir::Type> {
        db.key_for_type(self, &Default::default())
    }
}

impl Db {
    pub fn key_for_type(
        &self,
        ty: Type,
        substitutions: &BTreeMap<NodeRef, ir::Type>,
    ) -> Option<ir::Type> {
        let ty = match ty {
            Type::Node(node) => self.get_type(&node)?,
            Type::Constructed(ty) => ty,
        };

        match ty.tag {
            ConstructedTypeTag::Named(definition) => {
                let parameters = ty
                    .children
                    .into_iter()
                    .map(|ty| self.key_for_type(ty, substitutions))
                    .collect::<Option<_>>()?;

                Some(ir::Type::Named(definition, parameters))
            }
            ConstructedTypeTag::Function => {
                let (output, inputs) = ty.children.split_first()?;

                let output = self.key_for_type(output.clone(), substitutions)?;

                let inputs = inputs
                    .iter()
                    .map(|input| self.key_for_type(input.clone(), substitutions))
                    .collect::<Option<_>>()?;

                Some(ir::Type::Function(inputs, Box::new(output)))
            }
            ConstructedTypeTag::Tuple => {
                let elements = ty
                    .children
                    .into_iter()
                    .map(|child| self.key_for_type(child, substitutions))
                    .collect::<Option<_>>()?;

                Some(ir::Type::Tuple(elements))
            }
            ConstructedTypeTag::Block => {
                let [output] = ty.children.as_slice() else {
                    return None;
                };

                Some(ir::Type::Function(
                    Vec::new(),
                    Box::new(self.key_for_type(output.clone(), substitutions)?),
                ))
            }
            ConstructedTypeTag::Parameter(parameter) => Some(
                substitutions
                    .get(&parameter)
                    .cloned()
                    .unwrap_or(ir::Type::Parameter(parameter)),
            ),
        }
    }
}

impl ir::Type {
    pub fn traverse_mut(&mut self, f: &mut dyn FnMut(&mut Self)) {
        f(self);

        match self {
            ir::Type::Named(_, parameters) => {
                for parameter in parameters {
                    parameter.traverse_mut(f);
                }
            }
            ir::Type::Tuple(elements) => {
                for element in elements {
                    element.traverse_mut(f);
                }
            }
            ir::Type::Function(inputs, output) => {
                for input in inputs {
                    input.traverse_mut(f);
                }

                output.traverse_mut(f);
            }
            ir::Type::Parameter(_) => {}
        }
    }
}
