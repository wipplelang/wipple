use crate::{
    codegen::ir,
    db::{Db, Node},
    typecheck::{
        groups::update_type,
        ty::{Ty, TyTag},
    },
    visit::{
        definitions::{Defined, TypeDefinition},
        exhaustiveness::{EnumerationVariants, StructureFields},
    },
};
use std::collections::BTreeMap;

pub fn ir_type(db: &Db, ty: Ty) -> Option<ir::Type> {
    ir_type_inner(db, ty, &BTreeMap::new())
}

fn ir_type_inner(db: &Db, ty: Ty, substitutions: &BTreeMap<Node, ir::Type>) -> Option<ir::Type> {
    let Ty::Constructed(ty) = update_type(db, &ty) else {
        return None;
    };

    match ty.tag {
        TyTag::Named(definition) => {
            let parameters = ty
                .children
                .into_iter()
                .map(|ty| ir_type_inner(db, ty, substitutions))
                .collect::<Option<_>>()?;

            let TypeDefinition { attributes, .. } = db
                .get(definition)
                .and_then(|Defined(definition)| definition.downcast_ref())?;

            Some(ir::Type::Named {
                definition,
                parameters,
                intrinsic: attributes.intrinsic,
                representation: attributes.representation.clone(),
                abi: attributes.abi.clone(),
            })
        }
        TyTag::Function => {
            let (output, inputs) = ty.children.split_first()?;

            let output = ir_type_inner(db, output.clone(), substitutions)?;

            let inputs = inputs
                .iter()
                .map(|input| ir_type_inner(db, input.clone(), substitutions))
                .collect::<Option<_>>()?;

            Some(ir::Type::Function(inputs, Box::new(output)))
        }
        TyTag::Tuple => {
            let elements = ty
                .children
                .into_iter()
                .map(|child| ir_type_inner(db, child, substitutions))
                .collect::<Option<_>>()?;

            Some(ir::Type::Tuple(elements))
        }
        TyTag::Block => {
            let [output] = ty.children.as_slice() else {
                return None;
            };

            Some(ir::Type::Function(
                Vec::new(),
                Box::new(ir_type_inner(db, output.clone(), substitutions)?),
            ))
        }
        TyTag::Parameter(parameter) => Some(
            substitutions
                .get(&parameter)
                .cloned()
                .unwrap_or(ir::Type::Parameter(parameter)),
        ),
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

    pub fn substitute(&mut self, substitutions: &BTreeMap<Node, ir::Type>) {
        self.traverse_mut(&mut |ty| {
            if let ir::Type::Parameter(parameter) = ty
                && let Some(substitution) = substitutions.get(parameter)
            {
                *ty = substitution.clone();
            }
        });
    }
}

pub fn ir_named_type_representation(
    db: &Db,
    definition: Node,
    parameters: &[ir::Type],
) -> Option<ir::TypeRepresentation> {
    let TypeDefinition {
        attributes,
        parameters: type_parameters,
        ..
    } = db
        .get(definition)
        .and_then(|Defined(definition)| definition.downcast_ref())?;

    let substitutions = type_parameters
        .iter()
        .copied()
        .zip(parameters.iter().cloned())
        .collect::<BTreeMap<_, _>>();

    Some(if attributes.intrinsic {
        ir::TypeRepresentation::Intrinsic
    } else if let Some(StructureFields { fields, .. }) = db.get(definition) {
        ir::TypeRepresentation::Structure(
            fields
                .iter()
                .map(|field| {
                    let mut ty = ir_type(db, Ty::Node(*field))?;
                    ty.substitute(&substitutions);
                    Some(ty)
                })
                .collect::<Option<_>>()?,
        )
    } else if let Some(EnumerationVariants(variants)) = db.get(definition) {
        ir::TypeRepresentation::Enumeration(
            variants
                .iter()
                .map(|(_, elements)| {
                    elements
                        .iter()
                        .map(|element| {
                            let mut ty = ir_type(db, Ty::Node(*element))?;
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
