use crate::{
    codegen::{CodegenError, hir},
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

pub fn type_of(db: &Db, node: Node) -> Result<hir::Type, CodegenError> {
    type_of_inner(db, node, &BTreeMap::new())
        .ok_or_else(|| anyhow::format_err!("missing type of {node:?}"))
}

fn type_of_inner(
    db: &Db,
    node: Node,
    substitutions: &BTreeMap<Node, hir::Type>,
) -> Option<hir::Type> {
    let Ty::Constructed(ty) = update_type(db, &Ty::Node(node)) else {
        return None;
    };

    match ty.tag {
        TyTag::Named(definition) => {
            let parameters = ty
                .children
                .into_iter()
                .map(|ty| type_of_inner(db, ty, substitutions))
                .collect::<Option<_>>()?;

            Some(hir::Type::Named {
                definition,
                parameters,
            })
        }
        TyTag::Function => {
            let (output, inputs) = ty.children.split_first()?;

            let output = type_of_inner(db, *output, substitutions)?;

            let inputs = inputs
                .iter()
                .map(|input| type_of_inner(db, *input, substitutions))
                .collect::<Option<_>>()?;

            Some(hir::Type::Function(inputs, Box::new(output)))
        }
        TyTag::Tuple => {
            let elements = ty
                .children
                .into_iter()
                .map(|child| type_of_inner(db, child, substitutions))
                .collect::<Option<_>>()?;

            Some(hir::Type::Tuple(elements))
        }
        TyTag::Block => {
            let [output] = ty.children.as_slice() else {
                return None;
            };

            Some(hir::Type::Function(
                Vec::new(),
                Box::new(type_of_inner(db, *output, substitutions)?),
            ))
        }
        TyTag::Parameter(parameter) => Some(
            substitutions
                .get(&parameter)
                .cloned()
                .unwrap_or(hir::Type::Parameter(parameter)),
        ),
    }
}

impl hir::Type {
    pub fn traverse_mut(
        &mut self,
        f: &mut dyn FnMut(&mut Self) -> Result<(), CodegenError>,
    ) -> Result<(), CodegenError> {
        f(self)?;

        match self {
            hir::Type::Named { parameters, .. } => {
                for parameter in parameters {
                    parameter.traverse_mut(f)?;
                }
            }
            hir::Type::Tuple(elements) => {
                for element in elements {
                    element.traverse_mut(f)?;
                }
            }
            hir::Type::Function(inputs, output) => {
                for input in inputs {
                    input.traverse_mut(f)?;
                }

                output.traverse_mut(f)?;
            }
            hir::Type::Parameter(_) => {}
        }

        Ok(())
    }

    fn substitute(&mut self, substitutions: &BTreeMap<Node, &hir::Type>) {
        self.traverse_mut(&mut |ty| {
            if let hir::Type::Parameter(parameter) = ty
                && let Some(&substitution) = substitutions.get(parameter)
            {
                *ty = substitution.clone();
            }

            Ok(())
        })
        .unwrap();
    }
}

pub fn representation(
    db: &Db,
    definition: Node,
    parameters: &[hir::Type],
) -> Result<(Vec<Node>, hir::TypeRepresentation), CodegenError> {
    let TypeDefinition {
        parameters: type_parameters,
        attributes,
        ..
    } = db
        .get(definition)
        .and_then(|Defined(definition)| definition.downcast_ref())
        .ok_or_else(|| anyhow::format_err!("not a type definition"))?;

    let substitutions = type_parameters
        .iter()
        .copied()
        .zip(parameters)
        .collect::<BTreeMap<_, _>>();

    let representation = if let Some(intrinsic) = &attributes.intrinsic {
        let representation = match intrinsic.as_ref().map(|attribute| attribute.value.as_str()) {
            Some("number") => hir::IntrinsicRepresentation::Number,
            Some("string") => hir::IntrinsicRepresentation::String,
            _ => hir::IntrinsicRepresentation::Opaque,
        };

        hir::TypeRepresentation::Intrinsic(representation)
    } else if let Some(StructureFields { fields, .. }) = db.get(definition) {
        hir::TypeRepresentation::Structure(
            fields
                .iter()
                .map(|field| {
                    let mut ty = type_of(db, *field)?;
                    ty.substitute(&substitutions);
                    Ok(ty)
                })
                .collect::<Result<Vec<_>, CodegenError>>()?,
        )
    } else if let Some(EnumerationVariants(variants)) = db.get(definition) {
        hir::TypeRepresentation::Enumeration(
            variants
                .iter()
                .map(|(_, elements)| {
                    elements
                        .iter()
                        .map(|element| {
                            let mut ty = type_of(db, *element)?;
                            ty.substitute(&substitutions);
                            Ok(ty)
                        })
                        .collect()
                })
                .collect::<Result<Vec<_>, CodegenError>>()?,
        )
    } else {
        hir::TypeRepresentation::Marker
    };

    Ok((type_parameters.clone(), representation))
}
