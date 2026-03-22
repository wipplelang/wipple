use crate::{
    codegen::ir,
    database::{Db, NodeRef},
    nodes::{EnumerationVariants, StructureFields},
    typecheck::{ConstructedTypeTag, Type},
    visit::{Defined, Definition, TypeDefinition},
};
use std::collections::BTreeMap;

impl Db {
    pub fn ir_type(&self, ty: impl Into<Type>) -> Option<ir::Type> {
        self.ir_type_inner(ty.into(), &BTreeMap::new())
    }

    fn ir_type_inner(
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
                    .map(|ty| self.ir_type_inner(ty, substitutions))
                    .collect::<Option<_>>()?;

                let Defined(Definition::Type(TypeDefinition { attributes, .. })) =
                    self.get(&definition)?
                else {
                    return None;
                };

                Some(ir::Type::Named {
                    definition,
                    parameters,
                    intrinsic: attributes.intrinsic,
                    representation: attributes.representation,
                    abi: attributes.abi,
                })
            }
            ConstructedTypeTag::Function => {
                let (output, inputs) = ty.children.split_first()?;

                let output = self.ir_type_inner(output.clone(), substitutions)?;

                let inputs = inputs
                    .iter()
                    .map(|input| self.ir_type_inner(input.clone(), substitutions))
                    .collect::<Option<_>>()?;

                Some(ir::Type::Function(inputs, Box::new(output)))
            }
            ConstructedTypeTag::Tuple => {
                let elements = ty
                    .children
                    .into_iter()
                    .map(|child| self.ir_type_inner(child, substitutions))
                    .collect::<Option<_>>()?;

                Some(ir::Type::Tuple(elements))
            }
            ConstructedTypeTag::Block => {
                let [output] = ty.children.as_slice() else {
                    return None;
                };

                Some(ir::Type::Function(
                    Vec::new(),
                    Box::new(self.ir_type_inner(output.clone(), substitutions)?),
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
            ir::Type::Named { parameters, .. } => {
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

    pub fn substitute(&mut self, substitutions: &BTreeMap<NodeRef, ir::Type>) {
        self.traverse_mut(&mut |ty| {
            if let ir::Type::Parameter(parameter) = ty
                && let Some(substitution) = substitutions.get(parameter)
            {
                *ty = substitution.clone();
            }
        });
    }
}

impl Db {
    pub fn ir_named_type_representation(
        &self,
        definition: &NodeRef,
        parameters: &[ir::Type],
    ) -> Option<ir::TypeRepresentation> {
        let Defined(Definition::Type(TypeDefinition {
            attributes,
            parameters: type_parameters,
            ..
        })) = self.get(definition)?
        else {
            return None;
        };

        let substitutions = type_parameters
            .into_iter()
            .zip(parameters.iter().cloned())
            .collect::<BTreeMap<_, _>>();

        Some(if attributes.intrinsic {
            ir::TypeRepresentation::Intrinsic
        } else if let Some(StructureFields(fields)) = self.get(definition) {
            ir::TypeRepresentation::Structure(
                fields
                    .into_iter()
                    .map(|field| {
                        let mut ty = self.ir_type(field)?;
                        ty.substitute(&substitutions);
                        Some(ty)
                    })
                    .collect::<Option<_>>()?,
            )
        } else if let Some(EnumerationVariants(variants)) = self.get(definition) {
            ir::TypeRepresentation::Enumeration(
                variants
                    .into_iter()
                    .map(|(_, elements)| {
                        elements
                            .into_iter()
                            .map(|element| {
                                let mut ty = self.ir_type(element)?;
                                ty.substitute(&substitutions);
                                Some(ty)
                            })
                            .collect()
                    })
                    .collect::<Option<_>>()?,
            )
        } else {
            ir::TypeRepresentation::Marker
        })
    }
}
