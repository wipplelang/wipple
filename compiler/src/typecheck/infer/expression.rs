use crate::{
    typecheck::{
        Driver,
        infer::{
            Arm, Expression, ExpressionKind, FinalizeContext, FormatSegment, InferContext,
            ResolveContext, StructureFieldValue,
            errors::{QueuedError, try_report_custom_unused_error},
            item::resolve_item,
            pattern::infer_pattern,
            r#trait::resolve_trait,
            r#type::{finalize_instance, finalize_type, infer_type},
            types::{
                Instance, Type, TypeKind,
                context::TrackedExpressionId,
                instantiate::InstantiationContext,
                unify::{substitute_defaults, try_unify, try_unify_expression},
            },
        },
        utils::{
            instantiated_language_constant, instantiated_language_trait,
            instantiated_language_type, resolve_trait_parameters_from_type,
            try_instantiated_language_type,
        },
    },
    util::WithInfo,
};
use std::collections::HashMap;

pub fn infer_expression(
    expression: WithInfo<crate::typecheck::UntypedExpression>,
    parent_id: TrackedExpressionId,
    context: &mut InferContext<'_>,
) -> WithInfo<Expression> {
    context.with_tracked_expression(Some(parent_id), |context, expression_id| {
        let info = expression.info.clone();

        let mut expression = expression.map(|expression| match expression {
            crate::typecheck::UntypedExpression::Unknown => Expression {
                r#type: Type::new(TypeKind::Unknown, info.clone()),
                kind: ExpressionKind::Unknown(None),
            },
            crate::typecheck::UntypedExpression::Annotate { value, r#type } => {
                let mut value = infer_expression(value.unboxed(), expression_id, context);

                let r#type = infer_type(
                    context.driver,
                    r#type.as_ref(),
                    context.type_context,
                    context.error_queue,
                    context.errors,
                );

                try_unify_expression(
                    context.driver,
                    value.as_mut(),
                    &r#type,
                    context.type_context,
                    context.error_queue,
                );

                value.item
            }
            crate::typecheck::UntypedExpression::Variable(name, variable) => {
                let mut r#type = context
                    .variables
                    .get(&variable)
                    .cloned()
                    .unwrap_or_else(|| Type::new(TypeKind::Unknown, info.clone()));

                r#type.info = info.clone();

                Expression {
                    r#type,
                    kind: ExpressionKind::Variable(name, variable),
                }
            }
            crate::typecheck::UntypedExpression::Constant(path) => {
                let constant_declaration = context.driver.get_constant_declaration(&path);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    constant_declaration.item.parameters.clone(),
                    context.type_context,
                    info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let r#type = infer_type(
                    context.driver,
                    constant_declaration.item.r#type.as_ref(),
                    instantiation_context.type_context,
                    instantiation_context.error_queue,
                    instantiation_context.errors,
                )
                .instantiate(context.driver, &mut instantiation_context);

                Expression {
                    r#type,
                    kind: ExpressionKind::UnresolvedConstant(path),
                }
            }
            crate::typecheck::UntypedExpression::Trait(path) => {
                let trait_declaration = context.driver.get_trait_declaration(&path);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    trait_declaration.item.parameters.clone(),
                    context.type_context,
                    info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let r#type = match trait_declaration.item.r#type.as_ref() {
                    Some(r#type) => infer_type(
                        context.driver,
                        r#type.as_ref(),
                        instantiation_context.type_context,
                        instantiation_context.error_queue,
                        instantiation_context.errors,
                    )
                    .instantiate(context.driver, &mut instantiation_context),
                    None => {
                        context.errors.push(WithInfo {
                            info: info.clone(),
                            item: crate::typecheck::Diagnostic::TraitHasNoValue(path.clone()),
                        });

                        Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            info.clone(),
                        )
                    }
                };

                Expression {
                    r#type,
                    kind: ExpressionKind::UnresolvedTrait(path),
                }
            }
            crate::typecheck::UntypedExpression::Number(number) => {
                let r#type = instantiated_language_type(
                    "number",
                    info.clone(),
                    context.driver,
                    context.type_context,
                    context.error_queue,
                    context.errors,
                )
                .unwrap_or_else(|| {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                    )
                });

                Expression {
                    r#type,
                    kind: ExpressionKind::Number(number),
                }
            }
            crate::typecheck::UntypedExpression::Text(text) => {
                let r#type = instantiated_language_type(
                    "text",
                    info.clone(),
                    context.driver,
                    context.type_context,
                    context.error_queue,
                    context.errors,
                )
                .unwrap_or_else(|| {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                    )
                });

                Expression {
                    r#type,
                    kind: ExpressionKind::Text(text),
                }
            }
            crate::typecheck::UntypedExpression::Block {
                statements,
                top_level,
                captures,
            } => {
                let statements = statements
                    .into_iter()
                    .map(|expression| infer_expression(expression, expression_id, context))
                    .collect::<Vec<_>>();

                let r#type = statements.last().map_or_else(
                    || {
                        // We have an empty top level, whose type is `Unknown`
                        Type::new(TypeKind::Unknown, info.clone())
                    },
                    |expression| expression.item.r#type.clone(),
                );

                Expression {
                    r#type: Type::new(TypeKind::Block(Box::new(r#type)), info.clone()),
                    kind: ExpressionKind::Block {
                        statements,
                        top_level,
                        captures,
                    },
                }
            }
            crate::typecheck::UntypedExpression::Do(block) => {
                let output_type = Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                );

                let block_type =
                    Type::new(TypeKind::Block(Box::new(output_type.clone())), info.clone());

                let mut block = infer_expression(block.unboxed(), expression_id, context);

                try_unify_expression(
                    context.driver,
                    block.as_mut(),
                    &block_type,
                    context.type_context,
                    context.error_queue,
                );

                Expression {
                    r#type: output_type,
                    kind: ExpressionKind::Do(block.boxed()),
                }
            }
            crate::typecheck::UntypedExpression::Function {
                mut inputs,
                body,
                captures,
            } => {
                let input_types = inputs
                    .iter()
                    .map(|_| {
                        Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            info.clone(),
                        )
                    })
                    .collect::<Vec<_>>();

                for (pattern, input_type) in inputs.iter_mut().zip(&input_types) {
                    let input_type = pattern.replace(input_type);
                    infer_pattern(pattern.as_mut(), input_type, context);
                }

                let body = infer_expression(body.unboxed(), expression_id, context);

                Expression {
                    r#type: Type::new(
                        TypeKind::Function {
                            inputs: input_types,
                            output: Box::new(body.item.r#type.clone()),
                        },
                        info.clone(),
                    ),
                    kind: ExpressionKind::Function {
                        inputs,
                        body: body.boxed(),
                        captures,
                    },
                }
            }
            crate::typecheck::UntypedExpression::Call { function, inputs } => {
                let function = infer_expression(function.unboxed(), expression_id, context);

                let inputs = inputs
                    .into_iter()
                    .map(|input| infer_expression(input, expression_id, context))
                    .collect::<Vec<_>>();

                let r#type = match &function.item.r#type.kind {
                    TypeKind::Function { output, .. } => output.as_ref().clone(),
                    _ => Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                    ),
                };

                Expression {
                    r#type,
                    kind: ExpressionKind::Call {
                        function: function.boxed(),
                        inputs,
                    },
                }
            }
            crate::typecheck::UntypedExpression::When { input, arms } => {
                let input = infer_expression(input.unboxed(), expression_id, context);

                let r#type = Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                );

                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        arm.map(|mut arm| {
                            infer_pattern(
                                arm.pattern.as_mut(),
                                input.as_ref().map(|input| &input.r#type),
                                context,
                            );

                            let mut arm = Arm {
                                pattern: arm.pattern,
                                body: infer_expression(arm.body, expression_id, context),
                            };

                            try_unify_expression(
                                context.driver,
                                arm.body.as_mut(),
                                &r#type,
                                context.type_context,
                                context.error_queue,
                            );

                            arm
                        })
                    })
                    .collect();

                Expression {
                    r#type,
                    kind: ExpressionKind::When {
                        input: input.boxed(),
                        arms,
                    },
                }
            }
            crate::typecheck::UntypedExpression::Intrinsic { name, inputs } => Expression {
                r#type: Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                ),
                kind: ExpressionKind::Intrinsic {
                    name,
                    inputs: inputs
                        .into_iter()
                        .map(|expression| infer_expression(expression, expression_id, context))
                        .collect(),
                },
            },
            crate::typecheck::UntypedExpression::Initialize { mut pattern, value } => {
                let value = infer_expression(value.unboxed(), expression_id, context);

                infer_pattern(
                    pattern.as_mut(),
                    value.as_ref().map(|value| &value.r#type),
                    context,
                );

                let r#type = instantiated_language_type(
                    "none",
                    info.clone(),
                    context.driver,
                    context.type_context,
                    context.error_queue,
                    context.errors,
                )
                .unwrap_or_else(|| {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                    )
                });

                Expression {
                    r#type,
                    kind: ExpressionKind::Initialize {
                        pattern,
                        value: value.boxed(),
                    },
                }
            }
            crate::typecheck::UntypedExpression::Mutate { name, path, value } => {
                let mut value = infer_expression(value.unboxed(), expression_id, context);

                let r#type = context
                    .variables
                    .get(&path.item)
                    .cloned()
                    .unwrap_or_else(|| Type::new(TypeKind::Unknown, info.clone()));

                try_unify_expression(
                    context.driver,
                    value.as_mut(),
                    &r#type,
                    context.type_context,
                    context.error_queue,
                );

                let r#type = instantiated_language_type(
                    "none",
                    info.clone(),
                    context.driver,
                    context.type_context,
                    context.error_queue,
                    context.errors,
                )
                .unwrap_or_else(|| {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                    )
                });

                Expression {
                    r#type,
                    kind: ExpressionKind::Mutate {
                        name,
                        path,
                        value: value.boxed(),
                    },
                }
            }
            crate::typecheck::UntypedExpression::Marker(path) => {
                let type_declaration = context.driver.get_type_declaration(&path);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    type_declaration.item.parameters.clone(),
                    context.type_context,
                    info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let r#type = Type::new(
                    TypeKind::Declared {
                        path: path.clone(),
                        parameters: type_declaration
                            .item
                            .parameters
                            .into_iter()
                            .map(|path| {
                                instantiation_context.type_for_parameter(context.driver, &path)
                            })
                            .collect(),
                    },
                    info.clone(),
                );

                Expression {
                    r#type,
                    kind: ExpressionKind::Marker(path),
                }
            }
            crate::typecheck::UntypedExpression::Structure(fields) => Expression {
                r#type: Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                ),
                kind: ExpressionKind::UnresolvedStructure(
                    fields
                        .into_iter()
                        .map(|field| {
                            field.map(|field| StructureFieldValue {
                                name: field.name,
                                value: infer_expression(field.value, expression_id, context),
                            })
                        })
                        .collect(),
                ),
            },
            crate::typecheck::UntypedExpression::Variant { variant, values } => {
                let enumeration = context.driver.get_enumeration_for_variant(&variant.item);
                let type_declaration = context.driver.get_type_declaration(&enumeration);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    type_declaration.item.parameters.clone(),
                    context.type_context,
                    info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let values = match type_declaration.item.representation.item {
                    crate::typecheck::TypeRepresentation::Enumeration(declared_variants) => {
                        if let Some(declared_variant) = declared_variants.get(&variant.item) {
                            values
                                .into_iter()
                                .zip(&declared_variant.item.value_types)
                                .map(|(value, declared_type)| {
                                    let declared_type = infer_type(
                                        context.driver,
                                        declared_type.as_ref(),
                                        instantiation_context.type_context,
                                        instantiation_context.error_queue,
                                        instantiation_context.errors,
                                    )
                                    .instantiate(context.driver, &mut instantiation_context);

                                    (value, declared_type)
                                })
                                .collect()
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(),
                };

                let parameters = type_declaration
                    .item
                    .parameters
                    .into_iter()
                    .map(|path| instantiation_context.type_for_parameter(context.driver, &path))
                    .collect::<Vec<_>>();

                let r#type = Type::new(
                    TypeKind::Declared {
                        path: enumeration,
                        parameters,
                    },
                    info.clone(),
                );

                let values = values
                    .into_iter()
                    .map(|(value, declared_type)| {
                        let mut value = infer_expression(value, expression_id, context);

                        try_unify_expression(
                            context.driver,
                            value.as_mut(),
                            &declared_type,
                            context.type_context,
                            context.error_queue,
                        );

                        value
                    })
                    .collect::<Vec<_>>();

                Expression {
                    r#type,
                    kind: ExpressionKind::Variant { variant, values },
                }
            }
            crate::typecheck::UntypedExpression::Wrapper {
                r#type: path,
                value,
            } => {
                let type_declaration = context.driver.get_type_declaration(&path);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    type_declaration.item.parameters.clone(),
                    context.type_context,
                    info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let r#type = Type::new(
                    TypeKind::Declared {
                        path,
                        parameters: type_declaration
                            .item
                            .parameters
                            .into_iter()
                            .map(|path| {
                                instantiation_context.type_for_parameter(context.driver, &path)
                            })
                            .collect(),
                    },
                    info.clone(),
                );

                let value = match type_declaration.item.representation.item {
                    crate::typecheck::TypeRepresentation::Wrapper(declared_type) => {
                        let declared_type = infer_type(
                            context.driver,
                            declared_type.as_ref(),
                            instantiation_context.type_context,
                            instantiation_context.error_queue,
                            instantiation_context.errors,
                        )
                        .instantiate(context.driver, &mut instantiation_context);

                        let mut value = infer_expression(value.unboxed(), expression_id, context);

                        try_unify_expression(
                            context.driver,
                            value.as_mut(),
                            &declared_type,
                            context.type_context,
                            context.error_queue,
                        );

                        value
                    }
                    _ => WithInfo {
                        info: info.clone(),
                        item: Expression {
                            r#type: Type::new(
                                TypeKind::Variable(context.type_context.variable()),
                                info.clone(),
                            ),
                            kind: ExpressionKind::Unknown(None),
                        },
                    },
                };

                Expression {
                    r#type,
                    kind: ExpressionKind::Wrapper(value.boxed()),
                }
            }
            crate::typecheck::UntypedExpression::Tuple(elements) => {
                let elements = elements
                    .into_iter()
                    .map(|expression| infer_expression(expression, expression_id, context))
                    .collect::<Vec<_>>();

                Expression {
                    r#type: Type::new(
                        TypeKind::Tuple(
                            elements
                                .iter()
                                .map(|element| element.item.r#type.clone())
                                .collect(),
                        ),
                        info.clone(),
                    ),
                    kind: ExpressionKind::Tuple(elements),
                }
            }
            crate::typecheck::UntypedExpression::Collection(elements) => {
                let element_type = Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                );

                let initial_collection = instantiated_language_constant(
                    "initial-collection",
                    &info,
                    Some(expression_id),
                    context,
                );

                let collection_type = initial_collection.item.r#type.clone();

                elements
                    .into_iter()
                    .fold(initial_collection, |current, expression| {
                        let mut expression = infer_expression(expression, expression_id, context);

                        try_unify_expression(
                            context.driver,
                            expression.as_mut(),
                            &element_type,
                            context.type_context,
                            context.error_queue,
                        );

                        expression.item.r#type = element_type.clone();

                        let build_collection_trait = instantiated_language_trait(
                            "build-collection",
                            &info,
                            Some(expression_id),
                            context,
                        );

                        WithInfo {
                            info: expression.info.clone(),
                            item: Expression {
                                r#type: collection_type.clone(),
                                kind: ExpressionKind::Call {
                                    function: WithInfo {
                                        info: current.info.clone(),
                                        item: Box::new(Expression {
                                            r#type: Type::new(
                                                TypeKind::Function {
                                                    inputs: vec![collection_type.clone()],
                                                    output: Box::new(collection_type.clone()),
                                                },
                                                current.info.clone(),
                                            ),
                                            kind: ExpressionKind::Call {
                                                function: build_collection_trait.boxed(),
                                                inputs: vec![expression],
                                            },
                                        }),
                                    },
                                    inputs: vec![current],
                                },
                            },
                        }
                    })
                    .item
            }
            crate::typecheck::UntypedExpression::Format { segments, trailing } => {
                let text_type = instantiated_language_type(
                    "text",
                    info.clone(),
                    context.driver,
                    context.type_context,
                    context.error_queue,
                    context.errors,
                )
                .unwrap_or_else(|| {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                    )
                });

                let segments = segments
                    .into_iter()
                    .map(|segment| {
                        let value = infer_expression(segment.value, expression_id, context);

                        let describe_trait = instantiated_language_trait(
                            "describe",
                            &info,
                            Some(expression_id),
                            context,
                        );

                        FormatSegment {
                            text: segment.text,
                            value: WithInfo {
                                info: value.info.clone(),
                                item: Expression {
                                    r#type: Type::new(
                                        TypeKind::Variable(context.type_context.variable()),
                                        value.info.clone(),
                                    ),
                                    kind: ExpressionKind::Call {
                                        function: describe_trait.boxed(),
                                        inputs: vec![value],
                                    },
                                },
                            },
                        }
                    })
                    .collect();

                Expression {
                    r#type: text_type,
                    kind: ExpressionKind::Format { segments, trailing },
                }
            }
        });

        expression.item.r#type.info = info;

        expression
    })
}

pub fn resolve_expression(
    mut expression: WithInfo<Expression>,
    context: &mut ResolveContext<'_>,
) -> WithInfo<Expression> {
    let kind = match expression.item.kind {
        ExpressionKind::Unknown(path) => ExpressionKind::Unknown(path),
        ExpressionKind::Variable(ref name, ref variable) => {
            let mut variable_type = context
                .variables
                .get(variable)
                .cloned()
                .unwrap_or_else(|| Type::new(TypeKind::Unknown, expression.info.clone()));

            variable_type.apply_in_context_mut(context.type_context);
            variable_type.info = expression.info.clone();

            let name = name.clone();
            let variable = variable.clone();

            try_unify_expression(
                context.driver,
                expression.as_mut(),
                &variable_type,
                context.type_context,
                context.error_queue,
            );

            ExpressionKind::Variable(name, variable)
        }
        ExpressionKind::UnresolvedConstant(ref path) => {
            let path = path.clone();

            match resolve_item(&path, expression.as_mut(), true, context) {
                Ok(Some((parameters, bounds))) => ExpressionKind::ResolvedConstant {
                    path,
                    parameters,
                    bounds,
                },
                Ok(None) => ExpressionKind::UnresolvedConstant(path), // try again with more type information
                Err(error) => {
                    context.error_queue.push(error);
                    ExpressionKind::Unknown(Some(Box::new(ExpressionKind::UnresolvedConstant(
                        path,
                    ))))
                }
            }
        }
        ExpressionKind::UnresolvedTrait(ref path) => {
            let path = path.clone();

            match resolve_trait_parameters_from_type(&path, expression.as_mut(), context) {
                Some(parameters) => {
                    let query = expression.replace(Instance {
                        r#trait: path,
                        parameters,
                    });

                    match resolve_trait(query.as_ref(), context) {
                        Ok(instance) => ExpressionKind::ResolvedTrait {
                            trait_path: query.item.r#trait,
                            parameters: query.item.parameters,
                            instance,
                        },
                        Err(error) => {
                            context.error_queue.push(error);
                            ExpressionKind::Unknown(Some(Box::new(
                                ExpressionKind::UnresolvedTrait(query.item.r#trait),
                            )))
                        }
                    }
                }
                None => {
                    context.errors.push(WithInfo {
                        info: expression.info.clone(),
                        item: crate::typecheck::Diagnostic::TraitHasNoValue(path.clone()),
                    });

                    ExpressionKind::Unknown(Some(Box::new(ExpressionKind::UnresolvedTrait(path))))
                }
            }
        }
        ExpressionKind::ResolvedConstant {
            path,
            parameters,
            bounds,
        } => ExpressionKind::ResolvedConstant {
            path,
            parameters,
            bounds,
        },
        ExpressionKind::ResolvedTrait {
            trait_path,
            parameters,
            instance,
        } => ExpressionKind::ResolvedTrait {
            trait_path,
            parameters,
            instance,
        },
        ExpressionKind::Number(number) => ExpressionKind::Number(number),
        ExpressionKind::Text(text) => ExpressionKind::Text(text),
        ExpressionKind::Block {
            statements,
            top_level,
            captures,
        } => ExpressionKind::Block {
            statements: statements
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
            top_level,
            captures,
        },
        ExpressionKind::Do(block) => {
            ExpressionKind::Do(resolve_expression(block.unboxed(), context).boxed())
        }
        ExpressionKind::Function {
            mut inputs,
            body,
            captures,
        } => {
            if let TypeKind::Function {
                inputs: input_types,
                ..
            } = &expression.item.r#type.kind
            {
                for (pattern, input_type) in inputs.iter_mut().zip(input_types) {
                    let mut infer_context = InferContext::from_resolve_context(context);

                    let input_type = pattern.replace(input_type);

                    infer_pattern(pattern.as_mut(), input_type, &mut infer_context);
                }
            }

            ExpressionKind::Function {
                inputs,
                body: resolve_expression(body.unboxed(), context).boxed(),
                captures,
            }
        }
        ExpressionKind::Call {
            mut function,
            mut inputs,
        } => {
            function
                .item
                .r#type
                .apply_in_context_mut(context.type_context);

            for input in &mut inputs {
                input.item.r#type.apply_in_context_mut(context.type_context);
            }

            // If we encounter a unit after a number, treat the unit as a
            // function
            let is_number_with_unit = (|| {
                if inputs.len() != 1 {
                    return false;
                }

                let input = inputs.first().unwrap();

                let number_type = match try_instantiated_language_type(
                    "number",
                    expression.info.clone(),
                    context.driver,
                    context.type_context,
                    context.error_queue,
                    context.errors,
                ) {
                    Some(number_type) => number_type,
                    None => return false,
                };

                let number_type_path = match number_type.kind {
                    TypeKind::Declared { path, .. } => path,
                    _ => return false,
                };

                // Return true if the expression in function position is a
                // number...
                if matches!(
                    &function.item.r#type.kind,
                    TypeKind::Declared { path, .. }
                        if context.driver.paths_are_equal(path, &number_type_path),
                ) {
                    return true;
                }

                // ...or if the expression in input position is a function that
                // accepts a number
                if let TypeKind::Function { inputs, .. } = &input.item.r#type.kind {
                    if inputs.len() != 1 {
                        return false;
                    }

                    return matches!(
                        &inputs.first().unwrap().kind,
                        TypeKind::Declared { path, .. }
                            if context.driver.paths_are_equal(path, &number_type_path),
                    ) && !matches!(&function.item.r#type.kind, TypeKind::Function { .. });
                }

                false
            })();

            if is_number_with_unit {
                let input = inputs.pop().unwrap();

                let mut unit = resolve_expression(input, context);
                let number = resolve_expression(function.unboxed(), context);

                let unit_type = Type::new(
                    TypeKind::Function {
                        inputs: vec![number.item.r#type.clone()],
                        output: Box::new(expression.item.r#type.clone()),
                    },
                    expression.info.clone(),
                );

                if try_unify_expression(
                    context.driver,
                    unit.as_mut(),
                    &unit_type,
                    context.type_context,
                    context.error_queue,
                ) {
                    ExpressionKind::Call {
                        function: unit.boxed(),
                        inputs: vec![number],
                    }
                } else {
                    // Prevent duplicate errors caused by recreating the call
                    // expression
                    ExpressionKind::Unknown(Some(Box::new(ExpressionKind::Call {
                        function: number.boxed(),
                        inputs,
                    })))
                }
            } else if let TypeKind::Function {
                inputs: input_types,
                output: output_type,
            } = &function.item.r#type.kind
            {
                match inputs.len().cmp(&input_types.len()) {
                    std::cmp::Ordering::Equal => {
                        for (input, input_type) in inputs.iter_mut().zip(input_types) {
                            try_unify_expression(
                                context.driver,
                                input.as_mut(),
                                input_type,
                                context.type_context,
                                context.error_queue,
                            );
                        }

                        try_unify(
                            context.driver,
                            WithInfo {
                                info: expression.info.clone(),
                                item: &expression.item.r#type,
                            },
                            output_type,
                            context.type_context,
                            context.error_queue,
                        );

                        let inputs = inputs
                            .into_iter()
                            .map(|input| resolve_expression(input, context))
                            .collect::<Vec<_>>();

                        let function = resolve_expression(function.unboxed(), context);

                        ExpressionKind::Call {
                            function: function.boxed(),
                            inputs,
                        }
                    }
                    std::cmp::Ordering::Less => {
                        context.error_queue.push(WithInfo {
                            info: expression.info.clone(),
                            item: QueuedError::MissingInputs(input_types[inputs.len()..].to_vec()),
                        });

                        ExpressionKind::Unknown(Some(Box::new(ExpressionKind::Call {
                            function,
                            inputs,
                        })))
                    }
                    std::cmp::Ordering::Greater => {
                        for _ in &inputs[input_types.len()..] {
                            context.error_queue.push(WithInfo {
                                info: expression.info.clone(),
                                item: QueuedError::ExtraInput,
                            });
                        }

                        ExpressionKind::Unknown(Some(Box::new(ExpressionKind::Call {
                            function,
                            inputs,
                        })))
                    }
                }
            } else {
                let inputs = inputs
                    .into_iter()
                    .map(|input| resolve_expression(input, context))
                    .collect::<Vec<_>>();

                let mut function = resolve_expression(function.unboxed(), context);

                let function_type = Type::new(
                    TypeKind::Function {
                        inputs: inputs
                            .iter()
                            .map(|input| input.item.r#type.clone())
                            .collect(),
                        output: Box::new(expression.item.r#type.clone()),
                    },
                    function.info.clone(),
                );

                try_unify_expression(
                    context.driver,
                    function.as_mut(),
                    &function_type,
                    context.type_context,
                    context.error_queue,
                );

                ExpressionKind::Call {
                    function: function.boxed(),
                    inputs,
                }
            }
        }
        ExpressionKind::When { input, arms } => {
            let input = resolve_expression(input.unboxed(), context).boxed();

            let arms = arms
                .into_iter()
                .map(|arm| {
                    arm.map(|mut arm| {
                        infer_pattern(
                            arm.pattern.as_mut(),
                            input.as_ref().map(|input| &input.r#type),
                            &mut InferContext::from_resolve_context(context),
                        );

                        Arm {
                            pattern: arm.pattern,
                            body: resolve_expression(arm.body, context),
                        }
                    })
                })
                .collect::<Vec<_>>();

            ExpressionKind::When { input, arms }
        }
        ExpressionKind::Intrinsic { name, inputs } => ExpressionKind::Intrinsic {
            name,
            inputs: inputs
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        },
        ExpressionKind::Initialize { mut pattern, value } => {
            let value = resolve_expression(value.unboxed(), context).boxed();

            infer_pattern(
                pattern.as_mut(),
                value.as_ref().map(|value| &value.r#type),
                &mut InferContext::from_resolve_context(context),
            );

            ExpressionKind::Initialize { pattern, value }
        }
        ExpressionKind::Mutate { name, path, value } => {
            let value = resolve_expression(value.unboxed(), context).boxed();

            infer_pattern(
                path.as_ref()
                    .map(|path| crate::typecheck::Pattern::Variable(name.clone(), path.clone()))
                    .as_mut(),
                value.as_ref().map(|value| &value.r#type),
                &mut InferContext::from_resolve_context(context),
            );

            ExpressionKind::Mutate { name, path, value }
        }
        ExpressionKind::Marker(r#type) => ExpressionKind::Marker(r#type),
        ExpressionKind::UnresolvedStructure(fields) => {
            expression
                .item
                .r#type
                .apply_in_context_mut(context.type_context);

            match &expression.item.r#type.kind {
                TypeKind::Variable(_) | TypeKind::Opaque(_) => ExpressionKind::UnresolvedStructure(
                    fields
                        .into_iter()
                        .map(|field_value| {
                            field_value.map(|field_value| StructureFieldValue {
                                name: field_value.name,
                                value: resolve_expression(field_value.value, context),
                            })
                        })
                        .collect(),
                ),
                TypeKind::Declared { path, .. } => {
                    let path = path.clone();

                    let type_declaration = context.driver.get_type_declaration(&path);

                    if let crate::typecheck::TypeRepresentation::Structure(mut field_types) =
                        type_declaration.item.representation.item
                    {
                        let mut instantiation_context = InstantiationContext::from_parameters(
                            context.driver,
                            type_declaration.item.parameters.clone(),
                            context.type_context,
                            expression.info.clone(),
                            context.error_queue,
                            context.errors,
                        );

                        let fields = fields
                            .into_iter()
                            .filter_map(|field_value| {
                                let declared_type = match field_types.remove(&field_value.item.name)
                                {
                                    Some(field) => infer_type(
                                        context.driver,
                                        field.item.r#type.as_ref(),
                                        instantiation_context.type_context,
                                        instantiation_context.error_queue,
                                        instantiation_context.errors,
                                    )
                                    .instantiate(context.driver, &mut instantiation_context),
                                    None => {
                                        instantiation_context.error_queue.push(WithInfo {
                                            info: field_value.info.clone(),
                                            item: QueuedError::ExtraField,
                                        });

                                        return None;
                                    }
                                };

                                Some((field_value, declared_type))
                            })
                            .collect::<Vec<_>>();

                        let missing_fields = field_types.into_keys().collect::<Vec<_>>();
                        if !missing_fields.is_empty() {
                            instantiation_context.error_queue.push(WithInfo {
                                info: expression.info.clone(),
                                item: QueuedError::MissingFields(missing_fields),
                            });
                        }

                        let instantiated_declared_type = Type::new(
                            TypeKind::Declared {
                                path: path.clone(),
                                parameters: instantiation_context.into_types_for_parameters(),
                            },
                            expression.info.clone(),
                        );

                        try_unify(
                            context.driver,
                            WithInfo {
                                info: expression.info.clone(),
                                item: &instantiated_declared_type,
                            },
                            &expression.item.r#type,
                            context.type_context,
                            context.error_queue,
                        );

                        let fields = fields
                            .into_iter()
                            .map(|(field_value, declared_type)| {
                                field_value.map(|field_value| {
                                    let mut value = resolve_expression(field_value.value, context);

                                    try_unify_expression(
                                        context.driver,
                                        value.as_mut(),
                                        &declared_type,
                                        context.type_context,
                                        context.error_queue,
                                    );

                                    StructureFieldValue {
                                        name: field_value.name,
                                        value,
                                    }
                                })
                            })
                            .collect();

                        ExpressionKind::ResolvedStructure {
                            structure: path,
                            fields,
                        }
                    } else {
                        context.error_queue.push(WithInfo {
                            info: expression.info.clone(),
                            item: QueuedError::NotAStructure(expression.item.r#type.clone()),
                        });

                        ExpressionKind::Unknown(None)
                    }
                }
                _ => {
                    context.error_queue.push(WithInfo {
                        info: expression.info.clone(),
                        item: QueuedError::NotAStructure(expression.item.r#type.clone()),
                    });

                    ExpressionKind::Unknown(None)
                }
            }
        }
        ExpressionKind::ResolvedStructure { structure, fields } => {
            ExpressionKind::ResolvedStructure {
                structure,
                fields: fields
                    .into_iter()
                    .map(|field| {
                        field.map(|field| StructureFieldValue {
                            name: field.name,
                            value: resolve_expression(field.value, context),
                        })
                    })
                    .collect(),
            }
        }
        ExpressionKind::Variant { variant, values } => ExpressionKind::Variant {
            variant,
            values: values
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        },
        ExpressionKind::Wrapper(value) => {
            ExpressionKind::Wrapper(resolve_expression(value.unboxed(), context).boxed())
        }
        ExpressionKind::Tuple(elements) => ExpressionKind::Tuple(
            elements
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        ),
        ExpressionKind::Format { segments, trailing } => ExpressionKind::Format {
            segments: segments
                .into_iter()
                .map(|segment| FormatSegment {
                    text: segment.text,
                    value: resolve_expression(segment.value, context),
                })
                .collect(),
            trailing,
        },
    };

    WithInfo {
        info: expression.info,
        item: Expression {
            r#type: expression.item.r#type,
            kind,
        },
    }
}

pub fn substitute_defaults_in_expression(
    driver: &dyn Driver,
    expression: WithInfo<&mut Expression>,
    context: &mut ResolveContext<'_>,
) -> bool {
    let substituted_subexpression = match &mut expression.item.kind {
        ExpressionKind::Block { statements, .. } => statements.iter_mut().any(|statement| {
            substitute_defaults_in_expression(driver, statement.as_mut(), context)
        }),
        ExpressionKind::Do(block) => {
            substitute_defaults_in_expression(driver, block.as_deref_mut(), context)
        }
        ExpressionKind::Function { body, .. } => {
            substitute_defaults_in_expression(driver, body.as_deref_mut(), context)
        }
        ExpressionKind::Call { function, inputs } => {
            inputs
                .iter_mut()
                .any(|input| substitute_defaults_in_expression(driver, input.as_mut(), context))
                || substitute_defaults_in_expression(driver, function.as_deref_mut(), context)
        }
        ExpressionKind::When { input, arms } => {
            substitute_defaults_in_expression(driver, input.as_deref_mut(), context)
                || arms.iter_mut().any(|arm| {
                    substitute_defaults_in_expression(driver, arm.item.body.as_mut(), context)
                })
        }
        ExpressionKind::Intrinsic { inputs, .. } => inputs
            .iter_mut()
            .any(|input| substitute_defaults_in_expression(driver, input.as_mut(), context)),
        ExpressionKind::Initialize { value, .. } => {
            substitute_defaults_in_expression(driver, value.as_deref_mut(), context)
        }
        ExpressionKind::Mutate { value, .. } => {
            substitute_defaults_in_expression(driver, value.as_deref_mut(), context)
        }
        ExpressionKind::UnresolvedStructure(fields) => fields.iter_mut().any(|field| {
            substitute_defaults_in_expression(driver, field.item.value.as_mut(), context)
        }),
        ExpressionKind::ResolvedStructure { fields, .. } => fields.iter_mut().any(|field| {
            substitute_defaults_in_expression(driver, field.item.value.as_mut(), context)
        }),
        ExpressionKind::Variant { values, .. } => values
            .iter_mut()
            .any(|value| substitute_defaults_in_expression(driver, value.as_mut(), context)),
        ExpressionKind::Wrapper(value) => {
            substitute_defaults_in_expression(driver, value.as_deref_mut(), context)
        }
        ExpressionKind::Tuple(elements) => elements
            .iter_mut()
            .any(|element| substitute_defaults_in_expression(driver, element.as_mut(), context)),
        ExpressionKind::Format { segments, .. } => segments.iter_mut().any(|segment| {
            substitute_defaults_in_expression(driver, segment.value.as_mut(), context)
        }),
        ExpressionKind::ResolvedConstant { parameters, .. } => parameters
            .iter_mut()
            .any(|r#type| substitute_defaults(driver, r#type, context.type_context)),
        ExpressionKind::Unknown(_)
        | ExpressionKind::Variable(_, _)
        | ExpressionKind::UnresolvedConstant(_)
        | ExpressionKind::UnresolvedTrait(_)
        | ExpressionKind::ResolvedTrait { .. }
        | ExpressionKind::Marker(_)
        | ExpressionKind::Number(_)
        | ExpressionKind::Text(_) => false,
    };

    substituted_subexpression
        || substitute_defaults(driver, &mut expression.item.r#type, context.type_context)
}

pub fn finalize_expression(
    mut expression: WithInfo<Expression>,
    mut report_errors: bool,
    context: &mut FinalizeContext<'_>,
) -> WithInfo<crate::typecheck::TypedExpression> {
    let kind = match expression.item.kind {
        ExpressionKind::Unknown(original) => {
            report_errors = false;
            crate::typecheck::TypedExpressionKind::Unknown(original.map(|original| {
                let expression = WithInfo {
                    info: expression.info.clone(),
                    item: Expression {
                        r#type: expression.item.r#type.clone(),
                        kind: *original,
                    },
                };

                Box::new(finalize_expression(expression, false, context).item.kind)
            }))
        }
        ExpressionKind::Variable(name, variable) => {
            crate::typecheck::TypedExpressionKind::Variable(name, variable)
        }
        ExpressionKind::UnresolvedConstant(ref path) => {
            let path = path.clone();

            if !report_errors {
                crate::typecheck::TypedExpressionKind::Unknown(None)
            } else if let Some((error_queue, errors)) = context
                .error_queue
                .as_deref_mut()
                .zip(context.errors.as_deref_mut())
            {
                let mut resolve_context = ResolveContext {
                    driver: context.driver,
                    type_context: context.type_context,
                    error_queue,
                    errors,
                    variables: &mut Default::default(),
                    recursion_stack: &mut Default::default(),
                    bound_instances: context.bound_instances.clone(),
                };

                match resolve_item(&path, expression.as_mut(), false, &mut resolve_context) {
                    Ok(Some((parameters, bounds))) => {
                        finalize_type(expression.item.r#type.clone(), report_errors, context);

                        crate::typecheck::TypedExpressionKind::Constant {
                            path: path.clone(),
                            parameters: parameters
                                .into_iter()
                                .map(|r#type| finalize_type(r#type, report_errors, context).item)
                                .collect(),
                            bounds: bounds
                                .into_iter()
                                .map(|bound| {
                                    bound.map(|bound| {
                                        bound.map_err(|instance| {
                                            finalize_instance(instance, context)
                                        })
                                    })
                                })
                                .collect(),
                        }
                    }
                    Ok(None) => crate::typecheck::TypedExpressionKind::Unknown(None),
                    Err(error) => {
                        error_queue.push(error);
                        crate::typecheck::TypedExpressionKind::Unknown(None)
                    }
                }
            } else {
                crate::typecheck::TypedExpressionKind::Unknown(None)
            }
        }
        ExpressionKind::UnresolvedTrait(_) => {
            let error = WithInfo {
                info: expression.info.clone(),
                item: crate::typecheck::Diagnostic::UnknownType(
                    finalize_type(expression.item.r#type.clone(), report_errors, context).item,
                ),
            };

            if report_errors {
                if let Some(errors) = context.errors.as_deref_mut() {
                    errors.push(error);
                }
            }

            crate::typecheck::TypedExpressionKind::Unknown(None)
        }
        ExpressionKind::ResolvedConstant {
            path,
            parameters,
            bounds,
        } => crate::typecheck::TypedExpressionKind::Constant {
            path,
            parameters: parameters
                .into_iter()
                .map(|r#type| finalize_type(r#type, report_errors, context).item)
                .collect(),
            bounds: bounds
                .into_iter()
                .map(|bound| {
                    bound
                        .map(|bound| bound.map_err(|instance| finalize_instance(instance, context)))
                })
                .collect(),
        },
        ExpressionKind::ResolvedTrait {
            trait_path,
            parameters,
            instance,
        } => crate::typecheck::TypedExpressionKind::Trait {
            path: trait_path,
            parameters: parameters
                .into_iter()
                .map(|r#type| finalize_type(r#type, report_errors, context).item)
                .collect(),
            instance: instance
                .map(|instance| instance.map_err(|bound| finalize_instance(bound, context))),
        },
        ExpressionKind::Number(number) => crate::typecheck::TypedExpressionKind::Number(number),
        ExpressionKind::Text(text) => crate::typecheck::TypedExpressionKind::Text(text),
        ExpressionKind::Block {
            statements,
            top_level,
            captures,
        } => {
            let statement_count = statements.len();

            crate::typecheck::TypedExpressionKind::Block {
                statements: statements
                    .into_iter()
                    .enumerate()
                    .map(|(index, statement)| {
                        let is_last_statement = index + 1 == statement_count;

                        let statement_type = statement.item.r#type.clone();
                        let statement = finalize_expression(statement, report_errors, context);

                        // Report errors for unused values in statement position...
                        if (top_level || !is_last_statement)
                            && !matches!(statement.item.r#type, crate::typecheck::Type::Unknown)
                        {
                            let reported_custom_error = try_report_custom_unused_error(
                                context.driver,
                                &statement.info,
                                &statement_type,
                                context.type_context,
                                context.errors.as_deref_mut(),
                            );

                            // ...as well as uncalled functions
                            if !reported_custom_error {
                                if let crate::typecheck::Type::Function { inputs, .. } =
                                    &statement.item.r#type
                                {
                                    if let crate::typecheck::TypedExpressionKind::Constant {
                                        ..
                                    }
                                    | crate::typecheck::TypedExpressionKind::Trait { .. }
                                    | crate::typecheck::TypedExpressionKind::Variable {
                                        ..
                                    } = &statement.item.kind
                                    {
                                        if report_errors {
                                            if let Some(errors) = context.errors.as_deref_mut() {
                                                errors.push(WithInfo {
                                                    info: statement.info.clone(),
                                                    item:
                                                        crate::typecheck::Diagnostic::MissingInputs(
                                                            inputs.clone(),
                                                        ),
                                                });
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        statement
                    })
                    .collect(),
                captures,
            }
        }
        ExpressionKind::Do(block) => crate::typecheck::TypedExpressionKind::Do(
            finalize_expression(block.unboxed(), report_errors, context).boxed(),
        ),
        ExpressionKind::Function {
            inputs,
            body,
            captures,
        } => crate::typecheck::TypedExpressionKind::Function {
            inputs,
            body: finalize_expression(body.unboxed(), report_errors, context).boxed(),
            captures,
        },
        ExpressionKind::Call { function, inputs } => {
            let inputs = inputs
                .into_iter()
                .map(|input| finalize_expression(input, report_errors, context))
                .collect::<Vec<_>>();

            let function = finalize_expression(function.unboxed(), report_errors, context);

            crate::typecheck::TypedExpressionKind::Call {
                function: function.boxed(),
                inputs,
            }
        }
        ExpressionKind::When { input, arms } => crate::typecheck::TypedExpressionKind::When {
            input: finalize_expression(input.unboxed(), report_errors, context).boxed(),
            arms: arms
                .into_iter()
                .map(|arm| {
                    arm.map(|arm| crate::typecheck::TypedArm {
                        pattern: arm.pattern,
                        body: finalize_expression(arm.body, report_errors, context),
                    })
                })
                .collect(),
        },
        ExpressionKind::Intrinsic { name, inputs } => {
            crate::typecheck::TypedExpressionKind::Intrinsic {
                name,
                inputs: inputs
                    .into_iter()
                    .map(|expression| finalize_expression(expression, report_errors, context))
                    .collect(),
            }
        }
        ExpressionKind::Initialize { pattern, value } => {
            crate::typecheck::TypedExpressionKind::Initialize {
                pattern,
                value: finalize_expression(value.unboxed(), report_errors, context).boxed(),
            }
        }
        ExpressionKind::Mutate { name, path, value } => {
            crate::typecheck::TypedExpressionKind::Mutate {
                name,
                path,
                value: finalize_expression(value.unboxed(), report_errors, context).boxed(),
            }
        }
        ExpressionKind::Marker(r#type) => crate::typecheck::TypedExpressionKind::Marker(r#type),
        ExpressionKind::UnresolvedStructure(_) => {
            let error = WithInfo {
                info: expression.info.clone(),
                item: crate::typecheck::Diagnostic::UnknownType(
                    finalize_type(expression.item.r#type.clone(), report_errors, context).item,
                ),
            };

            if let Some(errors) = context.errors.as_deref_mut() {
                errors.push(error);
            }

            crate::typecheck::TypedExpressionKind::Unknown(None)
        }
        ExpressionKind::ResolvedStructure { structure, fields } => {
            let field_indices = match context
                .driver
                .get_type_declaration(&structure)
                .item
                .representation
                .item
            {
                crate::typecheck::TypeRepresentation::Structure(fields) => fields
                    .into_iter()
                    .map(|(name, field)| (name, field.item.index))
                    .collect::<HashMap<_, _>>(),
                _ => HashMap::new(),
            };

            crate::typecheck::TypedExpressionKind::Structure {
                structure,
                fields: fields
                    .into_iter()
                    .map(|field_value| {
                        field_value.map(|field_value| crate::typecheck::TypedStructureFieldValue {
                            name: field_value.name.clone(),
                            index: field_indices.get(&field_value.name).copied(),
                            value: finalize_expression(field_value.value, report_errors, context),
                        })
                    })
                    .collect(),
            }
        }
        ExpressionKind::Wrapper(value) => crate::typecheck::TypedExpressionKind::Wrapper(
            finalize_expression(value.unboxed(), report_errors, context).boxed(),
        ),
        ExpressionKind::Variant { variant, values } => {
            crate::typecheck::TypedExpressionKind::Variant {
                variant,
                values: values
                    .into_iter()
                    .map(|expression| finalize_expression(expression, report_errors, context))
                    .collect(),
            }
        }
        ExpressionKind::Tuple(elements) => crate::typecheck::TypedExpressionKind::Tuple(
            elements
                .into_iter()
                .map(|expression| finalize_expression(expression, report_errors, context))
                .collect(),
        ),
        ExpressionKind::Format { segments, trailing } => {
            crate::typecheck::TypedExpressionKind::Format {
                segments: segments
                    .into_iter()
                    .map(|segment| crate::typecheck::TypedFormatSegment {
                        text: segment.text.clone(),
                        value: finalize_expression(segment.value, report_errors, context),
                    })
                    .collect(),
                trailing,
            }
        }
    };

    let r#type = finalize_type(expression.item.r#type.clone(), report_errors, context).item;

    WithInfo {
        info: expression.info,
        item: crate::typecheck::TypedExpression { r#type, kind },
    }
}
