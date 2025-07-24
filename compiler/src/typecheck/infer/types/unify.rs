//! Unification.

use crate::{
    typecheck::{
        Driver,
        infer::{
            Expression,
            errors::QueuedError,
            types::{Instance, Type, TypeKind, TypeVariable, context::TypeContext},
        },
    },
    util::WithInfo,
};
use std::collections::btree_map;

impl TypeContext {
    pub fn variable(&mut self) -> TypeVariable {
        self.variable_with_default(None)
    }

    pub fn variable_with_default(&mut self, default: impl Into<Option<Type>>) -> TypeVariable {
        let counter = self.next_variable;
        self.next_variable += 1;

        if let Some(default) = default.into() {
            self.defaults.insert(counter, default);
        }

        TypeVariable { counter }
    }
}

impl TypeVariable {
    pub fn with_substitution_mut<T>(
        &self,
        context: &mut TypeContext,
        f: impl FnOnce(btree_map::Entry<'_, u32, Type>) -> T,
    ) -> T {
        f(context.substitutions.entry(self.counter))
    }

    pub fn default(&self, context: &mut TypeContext) -> Option<Type> {
        context.defaults.get(&self.counter).cloned()
    }
}

#[derive(Clone, Copy, Default)]
#[non_exhaustive]
pub struct UnifyOptions {
    pub require_equal_type_parameters: bool,
}

#[must_use]
pub fn unify_with_options(
    driver: &dyn Driver,
    r#type: &Type,
    expected_type: &Type,
    context: &mut TypeContext,
    options: UnifyOptions,
) -> bool {
    fn unify_variable(variable: &TypeVariable, r#type: &Type, context: &mut TypeContext) -> bool {
        if r#type.contains_variable(variable) {
            return false;
        }

        variable.with_substitution_mut(context, |substitution| match substitution {
            btree_map::Entry::Vacant(entry) => {
                entry.insert(r#type.clone());
            }
            btree_map::Entry::Occupied(_) => panic!("variable already has substitution"),
        });

        if let TypeKind::Variable(other) = &r#type.kind {
            if let Some(default) = variable.default(context) {
                context.defaults.insert(other.counter, default);
            }
        }

        true
    }

    fn unify_inner(
        driver: &dyn Driver,
        r#type: &Type,
        expected_type: &Type,
        context: &mut TypeContext,
        options: UnifyOptions,
        ignore_type_parameters: bool,
    ) -> bool {
        let r#type = r#type.apply_in_context(context);
        let expected_type = expected_type.apply_in_context(context);

        match (&r#type.kind, &expected_type.kind) {
            // Opaque types don't unify with anything
            (TypeKind::Opaque(_), _) | (_, TypeKind::Opaque(_)) => true,

            // Type variables unify with anything and are substituted with the
            // other type in `apply_in_context`
            (TypeKind::Variable(variable), _) => {
                unify_variable(variable, &expected_type, context);
                true
            }
            (_, TypeKind::Variable(variable)) => {
                unify_variable(variable, &r#type, context);
                true
            }

            // Unify the right side of an equal type, and the left side if it's
            // a type variable (ie. an instantiated type parameter)
            (TypeKind::Equal { left, right }, _) => {
                let left_context = if let TypeKind::Variable(_) = left.kind {
                    &mut *context
                } else {
                    &mut context.clone()
                };

                unify_inner(driver, left, &expected_type, left_context, options, true)
                    & unify_inner(driver, right, &expected_type, context, options, true)
            }
            (_, TypeKind::Equal { left, right }) => {
                let left_context = if let TypeKind::Variable(_) = left.kind {
                    &mut *context
                } else {
                    &mut context.clone()
                };

                unify_inner(driver, &r#type, left, left_context, options, true)
                    & unify_inner(driver, &r#type, right, context, options, true)
            }

            // Type parameters are equal to themselves, but otherwise must be
            // instantiated
            (TypeKind::Parameter(parameter), TypeKind::Parameter(expected_parameter)) => {
                ignore_type_parameters
                    || driver.paths_are_equal(parameter, expected_parameter)
                    || !options.require_equal_type_parameters
            }
            (_, TypeKind::Parameter(_)) | (TypeKind::Parameter(_), _) => ignore_type_parameters,

            // Unify declared types, functions, tuples, and blocks structurally
            (
                TypeKind::Declared { path, parameters },
                TypeKind::Declared {
                    path: expected_path,
                    parameters: expected_parameters,
                },
            ) => {
                if !driver.paths_are_equal(path, expected_path) {
                    return false;
                }

                let mut unified = true;
                for (r#type, expected_type) in parameters.iter().zip(expected_parameters) {
                    unified &= unify_inner(
                        driver,
                        r#type,
                        expected_type,
                        context,
                        options,
                        ignore_type_parameters,
                    );
                }

                unified
            }
            (
                TypeKind::Function { inputs, output },
                TypeKind::Function {
                    inputs: expected_inputs,
                    output: expected_output,
                },
            ) => {
                if inputs.len() != expected_inputs.len() {
                    return false;
                }

                let mut unified = true;
                for (r#type, expected_type) in inputs.iter().zip(expected_inputs) {
                    unified &= unify_inner(
                        driver,
                        r#type,
                        expected_type,
                        context,
                        options,
                        ignore_type_parameters,
                    );
                }

                unified
                    & unify_inner(
                        driver,
                        output,
                        expected_output,
                        context,
                        options,
                        ignore_type_parameters,
                    )
            }
            (TypeKind::Tuple(elements), TypeKind::Tuple(expected_elements)) => {
                if elements.len() != expected_elements.len() {
                    return false;
                }

                let mut unified = true;
                for (r#type, expected_type) in elements.iter().zip(expected_elements) {
                    unified &= unify_inner(
                        driver,
                        r#type,
                        expected_type,
                        context,
                        options,
                        ignore_type_parameters,
                    );
                }

                unified
            }
            (TypeKind::Block(r#type), TypeKind::Block(expected_type)) => unify_inner(
                driver,
                r#type,
                expected_type,
                context,
                options,
                ignore_type_parameters,
            ),

            // Intrinsic types unify with other intrinsic types (they're
            // supposed to be wrapped in another type)
            (TypeKind::Intrinsic, TypeKind::Intrinsic) => true,

            // Unknown types unify with everything
            (TypeKind::Unknown, _) | (_, TypeKind::Unknown) => true,

            // Any other combination of types is an error
            _ => false,
        }
    }

    unify_inner(driver, r#type, expected_type, context, options, false)
}

#[must_use]
pub fn unify(
    driver: &dyn Driver,
    r#type: &Type,
    expected_type: &Type,
    context: &mut TypeContext,
) -> bool {
    unify_with_options(
        driver,
        r#type,
        expected_type,
        context,
        UnifyOptions::default(),
    )
}

#[must_use]
pub fn unify_parameters_with_options(
    driver: &dyn Driver,
    parameters: &[Type],
    expected_parameters: &[Type],
    context: &mut TypeContext,
    options: UnifyOptions,
) -> bool {
    let mut unified = true;
    for (r#type, expected_type) in parameters.iter().zip(expected_parameters) {
        unified &= unify_with_options(driver, r#type, expected_type, context, options);
    }

    unified
}

#[must_use]
pub fn unify_instance_with_options(
    driver: &dyn Driver,
    instance: WithInfo<&Instance>,
    expected_instance: WithInfo<&Instance>,
    context: &mut TypeContext,
    options: UnifyOptions,
) -> bool {
    driver.paths_are_equal(&instance.item.r#trait, &expected_instance.item.r#trait)
        && unify_parameters_with_options(
            driver,
            &instance.item.parameters,
            &expected_instance.item.parameters,
            context,
            options,
        )
}

#[must_use]
pub fn unify_instance(
    driver: &dyn Driver,
    actual_instance: WithInfo<&Instance>,
    expected_instance: WithInfo<&Instance>,
    context: &mut TypeContext,
) -> bool {
    unify_instance_with_options(
        driver,
        actual_instance,
        expected_instance,
        context,
        UnifyOptions::default(),
    )
}

pub fn try_unify_expression(
    driver: &dyn Driver,
    expression: WithInfo<&mut Expression>,
    expected_type: &Type,
    context: &mut TypeContext,
    error_queue: &mut Vec<WithInfo<QueuedError>>,
) -> bool {
    let unified = unify(driver, &expression.item.r#type, expected_type, context);
    if !unified {
        error_queue.push(WithInfo {
            info: expression.info.clone(),
            item: QueuedError::Mismatch {
                actual: expression.item.r#type.clone(),
                expected: expected_type.clone(),
            },
        });
    }

    unified
}

pub fn try_unify(
    driver: &dyn Driver,
    r#type: WithInfo<&Type>,
    expected_type: &Type,
    context: &mut TypeContext,
    error_queue: &mut Vec<WithInfo<QueuedError>>,
) {
    if !unify(driver, r#type.item, expected_type, context) {
        error_queue.push(WithInfo {
            info: r#type.info.clone(),
            item: QueuedError::Mismatch {
                actual: r#type.item.clone(),
                expected: expected_type.clone(),
            },
        });
    }
}

pub fn substitute_defaults(
    driver: &dyn Driver,
    r#type: &mut Type,
    context: &mut TypeContext,
) -> bool {
    r#type.apply_in_context_mut(context);

    match &mut r#type.kind {
        TypeKind::Variable(variable) => variable
            .default(context)
            .is_some_and(|default| unify(driver, r#type, &default, context)),
        TypeKind::Declared { parameters, .. } => {
            for r#type in parameters {
                if substitute_defaults(driver, r#type, context) {
                    return true;
                }
            }

            false
        }
        TypeKind::Function { inputs, output } => {
            for r#type in inputs {
                if substitute_defaults(driver, r#type, context) {
                    return true;
                }
            }

            substitute_defaults(driver, output, context)
        }
        TypeKind::Tuple(elements) => {
            for r#type in elements {
                if substitute_defaults(driver, r#type, context) {
                    return true;
                }
            }

            false
        }
        TypeKind::Block(r#type) => substitute_defaults(driver, r#type, context),
        TypeKind::Message { segments, .. } => {
            for segment in segments {
                if substitute_defaults(driver, &mut segment.value, context) {
                    return true;
                }
            }

            false
        }
        TypeKind::Equal { left, right } => {
            substitute_defaults(driver, left, context)
                || substitute_defaults(driver, right, context)
        }
        TypeKind::Opaque(_) | TypeKind::Parameter(_) | TypeKind::Unknown | TypeKind::Intrinsic => {
            false
        }
    }
}
