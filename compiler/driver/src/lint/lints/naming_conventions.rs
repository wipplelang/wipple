use crate::{
    lint::{
        lints::{AddLint, Rule},
        Lint,
    },
    visit::{pattern_visitor, traverse_expression},
};
use serde::{Deserialize, Serialize};
use std::ops::ControlFlow;
use wipple_linker::Driver;
use wipple_typecheck::Pattern;
use wipple_util::WithInfo;
use Case::*;

/// A lint that triggers when a name doesn't follow the correct conventions
/// (separated by dashes, variables lowercase, types/traits capitalized).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NamingConventionsLint {
    /// The type of value the name represents (variable, type, etc.)
    pub convention: NamingConvention,

    /// The name modified to follow the correct convention.
    pub suggested_name: String,
}

/// The type of value a name represents.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum NamingConvention {
    Variable,
    Type,
    Trait,
    Constant,
    TypeParameter,
}

pub(super) struct NamingConventionsRule;

enum Case {
    Lowercase,
    Uppercase,
}

fn convert_case(s: &str, case: Case) -> Option<String> {
    let converted = convert_case::Converter::new()
        .set_boundaries(&[
            convert_case::Boundary::Acronym,
            convert_case::Boundary::Hyphen,
            convert_case::Boundary::Underscore,
            convert_case::Boundary::LowerUpper,
        ])
        .set_delim('-')
        .set_pattern(match case {
            Lowercase => convert_case::Pattern::Lowercase,
            Uppercase => convert_case::Pattern::Capital,
        })
        .convert(s);

    (s != converted).then_some(converted)
}

impl<D: Driver> Rule<D> for NamingConventionsRule {
    fn lint_item(
        &self,
        path: &wipple_lower::Path,
        item: &wipple_linker::UnlinkedItem<D>,
        add_lint: AddLint<'_, D>,
    ) {
        if is_constructor(path) {
            return;
        }

        traverse_expression(
            item.expression.as_ref(),
            pattern_visitor(|pattern, _| {
                if let Pattern::Variable(name, _) = &pattern.item {
                    if let Some(name) = convert_case(name, Lowercase) {
                        add_lint(
                            pattern.replace(Lint::NamingConventions(NamingConventionsLint {
                                convention: NamingConvention::Variable,
                                suggested_name: name,
                            })),
                        );
                    }
                }

                ControlFlow::Continue(())
            }),
        )
    }

    fn lint_type_declaration(
        &self,
        path: &wipple_lower::Path,
        declaration: wipple_util::WithInfo<<D>::Info, &wipple_typecheck::TypeDeclaration<D>>,
        add_lint: AddLint<'_, D>,
    ) {
        if is_constructor(path) || ignores_naming_conventions(&declaration.item.attributes) {
            return;
        }

        let name = match path.0.last().and_then(|path| path.name()) {
            Some(name) => name,
            None => return,
        };

        if let Some(name) = convert_case(name, Uppercase) {
            add_lint(
                declaration.replace(Lint::NamingConventions(NamingConventionsLint {
                    convention: NamingConvention::Type,
                    suggested_name: name,
                })),
            );
        }
    }

    fn lint_trait_declaration(
        &self,
        path: &wipple_lower::Path,
        declaration: wipple_util::WithInfo<<D>::Info, &wipple_typecheck::TraitDeclaration<D>>,
        add_lint: AddLint<'_, D>,
    ) {
        if is_constructor(path) || ignores_naming_conventions(&declaration.item.attributes) {
            return;
        }

        let name = match path.0.last().and_then(|path| path.name()) {
            Some(name) => name,
            None => return,
        };

        if let Some(name) = convert_case(name, Uppercase) {
            add_lint(
                declaration.replace(Lint::NamingConventions(NamingConventionsLint {
                    convention: NamingConvention::Trait,
                    suggested_name: name,
                })),
            );
        }
    }

    fn lint_constant_declaration(
        &self,
        path: &wipple_lower::Path,
        declaration: wipple_util::WithInfo<<D>::Info, &wipple_typecheck::ConstantDeclaration<D>>,
        add_lint: AddLint<'_, D>,
    ) {
        if is_constructor(path) || ignores_naming_conventions(&declaration.item.attributes) {
            return;
        }

        let name = match path.0.last().and_then(|path| path.name()) {
            Some(name) => name,
            None => return,
        };

        // Variants are capitalized
        let case = if path.0.len() > 1
            && matches!(
                path.0[path.0.len() - 2],
                wipple_lower::PathComponent::Type(_)
            ) {
            Uppercase
        } else {
            Lowercase
        };

        if let Some(name) = convert_case(name, case) {
            add_lint(
                declaration.replace(Lint::NamingConventions(NamingConventionsLint {
                    convention: NamingConvention::Constant,
                    suggested_name: name,
                })),
            );
        }
    }

    fn lint_type_parameter_declaration(
        &self,
        path: &wipple_lower::Path,
        declaration: wipple_util::WithInfo<
            <D>::Info,
            &wipple_typecheck::TypeParameterDeclaration<D>,
        >,
        add_lint: AddLint<'_, D>,
    ) {
        if is_constructor(path) {
            return;
        }

        let name = match path.0.last().and_then(|path| path.name()) {
            Some(name) => name,
            None => return,
        };

        if let Some(name) = convert_case(name, Uppercase) {
            add_lint(
                declaration.replace(Lint::NamingConventions(NamingConventionsLint {
                    convention: NamingConvention::TypeParameter,
                    suggested_name: name,
                })),
            );
        }
    }
}

// Ignore constructors in favor of the actual declaration
fn is_constructor(path: &wipple_lower::Path) -> bool {
    path.0
        .iter()
        .any(|component| matches!(component, wipple_lower::PathComponent::Constructor(_)))
}

fn ignores_naming_conventions<D: Driver>(
    attributes: &[WithInfo<D::Info, wipple_typecheck::Attribute<D>>],
) -> bool {
    attributes.iter().any(|attribute| {
        if let wipple_typecheck::Attribute::Name(name) = &attribute.item {
            if name.item == "ignore-naming-conventions" {
                return true;
            }
        }

        false
    })
}
