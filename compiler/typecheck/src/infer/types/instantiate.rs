//! The instantiation context.

use crate::{
    infer::{
        errors::QueuedError,
        r#type::infer_type,
        types::{context::TypeContext, Instance, Type, TypeKind},
        ResolveContext,
    },
    Driver,
};
use wipple_util::WithInfo;

pub struct InstantiationContext<'a, D: Driver> {
    pub type_context: &'a mut TypeContext<D>,
    pub types: Vec<(D::Path, Type<D>)>,
    pub info: D::Info,
    pub error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    pub errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
}

#[derive(Clone, Copy, Default)]
#[non_exhaustive]
pub struct InstantiationOptions {
    pub instantiate_inferred_parameters_as_opaque: bool,
}

impl<'a, D: Driver> InstantiationContext<'a, D> {
    pub fn from_parameters(
        driver: &D,
        parameters: impl IntoIterator<Item = D::Path>,
        type_context: &'a mut TypeContext<D>,
        info: D::Info,
        error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
        errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    ) -> Self {
        InstantiationContext::from_parameters_with_options(
            driver,
            parameters,
            type_context,
            info,
            error_queue,
            errors,
            Default::default(),
        )
    }

    pub fn from_parameters_with_options(
        driver: &D,
        parameters: impl IntoIterator<Item = D::Path>,
        type_context: &'a mut TypeContext<D>,
        info: D::Info,
        error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
        errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
        options: InstantiationOptions,
    ) -> Self {
        let mut context = InstantiationContext {
            type_context,
            types: Vec::new(),
            info: info.clone(),
            error_queue,
            errors,
        };

        for path in parameters {
            let parameter_declaration = driver.get_type_parameter_declaration(&path);

            let default = parameter_declaration
                .as_ref()
                .item
                .default
                .as_ref()
                .map(|r#type| {
                    infer_type(
                        driver,
                        r#type.as_ref(),
                        context.type_context,
                        context.error_queue,
                        context.errors,
                    )
                    .instantiate(driver, &mut context)
                });

            let variable = context.type_context.variable_with_default(default);

            let kind = if parameter_declaration.item.infer.is_some()
                && options.instantiate_inferred_parameters_as_opaque
            {
                TypeKind::Opaque(variable)
            } else {
                TypeKind::Variable(variable)
            };

            context.types.push((path, Type::new(kind, info.clone())));
        }

        context
    }

    pub fn type_for_parameter(&mut self, driver: &D, parameter: &D::Path) -> Type<D> {
        self.types
            .iter()
            .find_map(|(instantiation_path, r#type)| {
                driver
                    .paths_are_equal(parameter, instantiation_path)
                    .then_some(r#type)
            })
            .cloned()
            .unwrap_or_else(|| {
                self.errors.push(WithInfo {
                    info: self.info.clone(),
                    item: crate::Diagnostic::UndeclaredTypeParameter(parameter.clone()),
                });

                Type::new(TypeKind::Unknown, self.info.clone())
            })
    }

    pub fn into_types_for_parameters(self) -> Vec<Type<D>> {
        self.types.into_iter().map(|(_, r#type)| r#type).collect()
    }
}

impl<D: Driver> Type<D> {
    #[must_use]
    pub fn instantiate(
        &self,
        driver: &D,
        instantiation_context: &mut InstantiationContext<'_, D>,
    ) -> Self {
        let mut r#type = self.clone();
        r#type.instantiate_mut(driver, instantiation_context);
        r#type
    }

    pub fn instantiate_mut(
        &mut self,
        driver: &D,
        instantiation_context: &mut InstantiationContext<'_, D>,
    ) {
        self.apply_in_context_mut(instantiation_context.type_context);

        match &mut self.kind {
            TypeKind::Variable(_) => {}
            TypeKind::Opaque(_) => {}
            TypeKind::Parameter(path) => {
                *self = instantiation_context.type_for_parameter(driver, path);
            }
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.instantiate_mut(driver, instantiation_context);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.instantiate_mut(driver, instantiation_context);
                }

                output.instantiate_mut(driver, instantiation_context);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.instantiate_mut(driver, instantiation_context);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.instantiate_mut(driver, instantiation_context);
            }
            TypeKind::Message { segments, .. } => {
                for segment in segments {
                    segment.value.instantiate_mut(driver, instantiation_context);
                }
            }
            TypeKind::Equal { left, right } => {
                left.instantiate_mut(driver, instantiation_context);
                right.instantiate_mut(driver, instantiation_context);
            }
            TypeKind::Unknown | TypeKind::Intrinsic => {}
        }
    }

    #[must_use]
    pub fn instantiate_opaque_in_context(&self, context: &mut TypeContext<D>) -> Self {
        let mut r#type = self.clone();
        r#type.instantiate_opaque_in_context_mut(context);
        r#type
    }

    pub fn instantiate_opaque_in_context_mut(&mut self, context: &mut TypeContext<D>) {
        self.apply_in_context_mut(context);

        match &mut self.kind {
            TypeKind::Variable(_) => {}
            TypeKind::Opaque(variable) => {
                self.kind = TypeKind::Variable(*variable);
            }
            TypeKind::Parameter(_) => {}
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.instantiate_opaque_in_context_mut(context);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.instantiate_opaque_in_context_mut(context);
                }

                output.instantiate_opaque_in_context_mut(context);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.instantiate_opaque_in_context_mut(context);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.instantiate_opaque_in_context_mut(context);
            }
            TypeKind::Message { segments, .. } => {
                for segment in segments {
                    segment.value.instantiate_opaque_in_context_mut(context);
                }
            }
            TypeKind::Equal { left, right } => {
                left.instantiate_opaque_in_context_mut(context);
                right.instantiate_opaque_in_context_mut(context);
            }
            TypeKind::Unknown | TypeKind::Intrinsic => {}
        }
    }
}

impl<D: Driver> Instance<D> {
    #[must_use]
    pub fn instantiate(
        &self,
        driver: &D,
        instantiation_context: &mut InstantiationContext<'_, D>,
    ) -> Self {
        let mut instance = self.clone();
        instance.instantiate_mut(driver, instantiation_context);
        instance
    }

    pub fn instantiate_mut(
        &mut self,
        driver: &D,
        instantiation_context: &mut InstantiationContext<'_, D>,
    ) {
        for parameter in &mut self.parameters {
            parameter.instantiate_mut(driver, instantiation_context);
        }
    }

    #[must_use]
    pub fn instantiate_opaque(&self, context: &mut TypeContext<D>) -> Self {
        let mut instance = self.clone();
        instance.instantiate_opaque_mut(context);
        instance
    }

    pub fn instantiate_opaque_mut(&mut self, context: &mut TypeContext<D>) {
        for parameter in &mut self.parameters {
            parameter.instantiate_opaque_in_context_mut(context);
        }
    }

    pub fn set_source_info(&mut self, context: &mut ResolveContext<'_, D>, info: &D::Info) {
        for parameter in &mut self.parameters {
            parameter.set_source_info(context, info);
        }
    }
}