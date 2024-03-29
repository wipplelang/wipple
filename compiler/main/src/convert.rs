pub mod interface {
    pub fn convert_type_declaration(
        type_declaration: wipple_util::WithInfo<
            crate::Info,
            wipple_typecheck::TypeDeclaration<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<crate::Info, wipple_lower::TypeDeclaration<crate::Driver>> {
        type_declaration.map(|type_declaration| wipple_lower::TypeDeclaration {
            parameters: type_declaration.parameters,
            representation: convert_type_representation(type_declaration.representation),
        })
    }

    pub fn convert_trait_declaration(
        trait_declaration: wipple_util::WithInfo<
            crate::Info,
            wipple_typecheck::TraitDeclaration<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<crate::Info, wipple_lower::TraitDeclaration<crate::Driver>> {
        trait_declaration.map(|trait_declaration| wipple_lower::TraitDeclaration {
            parameters: trait_declaration.parameters,
            r#type: convert_type(trait_declaration.r#type),
        })
    }

    pub fn convert_type_parameter_declaration(
        type_parameter_declaration: wipple_util::WithInfo<
            crate::Info,
            wipple_typecheck::TypeParameterDeclaration<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<crate::Info, wipple_lower::TypeParameterDeclaration<crate::Driver>>
    {
        type_parameter_declaration.map(|type_parameter_declaration| {
            wipple_lower::TypeParameterDeclaration {
                infer: type_parameter_declaration.infer,
                default: type_parameter_declaration.default.map(convert_type),
            }
        })
    }

    pub fn convert_constant_declaration(
        constant_declaration: wipple_util::WithInfo<
            crate::Info,
            wipple_typecheck::ConstantDeclaration<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<crate::Info, wipple_lower::ConstantDeclaration<crate::Driver>> {
        constant_declaration.map(|constant_declaration| wipple_lower::ConstantDeclaration {
            parameters: constant_declaration.parameters,
            bounds: constant_declaration
                .bounds
                .into_iter()
                .map(convert_instance)
                .collect(),
            r#type: convert_type(constant_declaration.r#type),
        })
    }

    pub fn convert_instance_declaration(
        instance_declaration: wipple_util::WithInfo<
            crate::Info,
            wipple_typecheck::InstanceDeclaration<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<crate::Info, wipple_lower::InstanceDeclaration<crate::Driver>> {
        instance_declaration.map(|instance_declaration| wipple_lower::InstanceDeclaration {
            parameters: instance_declaration.parameters,
            bounds: instance_declaration
                .bounds
                .into_iter()
                .map(convert_instance)
                .collect(),
            instance: convert_instance(instance_declaration.instance),
        })
    }

    pub fn convert_type(
        r#type: wipple_util::WithInfo<crate::Info, wipple_typecheck::Type<crate::Driver>>,
    ) -> wipple_util::WithInfo<crate::Info, wipple_lower::Type<crate::Driver>> {
        let type_info = r#type.info.clone();

        r#type.map(|r#type| match r#type {
            wipple_typecheck::Type::Unknown(_) => wipple_lower::Type::Error,
            wipple_typecheck::Type::Parameter(parameter) => {
                wipple_lower::Type::Parameter(parameter)
            }
            wipple_typecheck::Type::Declared { path, parameters } => wipple_lower::Type::Declared {
                path: wipple_util::WithInfo {
                    info: type_info.clone(),
                    item: path,
                },
                parameters: parameters.into_iter().map(convert_type).collect(),
            },
            wipple_typecheck::Type::Function { inputs, output } => wipple_lower::Type::Function {
                inputs: inputs.into_iter().map(convert_type).collect(),
                output: convert_type(output.unboxed()).boxed(),
            },
            wipple_typecheck::Type::Tuple(elements) => {
                wipple_lower::Type::Tuple(elements.into_iter().map(convert_type).collect())
            }
            wipple_typecheck::Type::Block(r#type) => {
                wipple_lower::Type::Block(convert_type(r#type.unboxed()).boxed())
            }
            wipple_typecheck::Type::Intrinsic => wipple_lower::Type::Intrinsic,
        })
    }

    pub fn convert_instance(
        instance: wipple_util::WithInfo<crate::Info, wipple_typecheck::Instance<crate::Driver>>,
    ) -> wipple_util::WithInfo<crate::Info, wipple_lower::Instance<crate::Driver>> {
        let instance_info = instance.info.clone();

        instance.map(|instance| wipple_lower::Instance {
            r#trait: wipple_util::WithInfo {
                info: instance_info.clone(),
                item: instance.r#trait,
            },
            parameters: instance.parameters.into_iter().map(convert_type).collect(),
        })
    }

    pub fn convert_type_representation(
        type_representation: wipple_util::WithInfo<
            crate::Info,
            wipple_typecheck::TypeRepresentation<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<crate::Info, wipple_lower::TypeRepresentation<crate::Driver>> {
        type_representation.map(|type_representation| match type_representation {
            wipple_typecheck::TypeRepresentation::Marker => {
                wipple_lower::TypeRepresentation::Marker
            }
            wipple_typecheck::TypeRepresentation::Structure(fields) => {
                wipple_lower::TypeRepresentation::Structure(
                    fields
                        .into_iter()
                        .map(|(name, field)| wipple_util::WithInfo {
                            info: field.info.clone(),
                            item: wipple_lower::Field {
                                name: wipple_util::WithInfo {
                                    info: field.info,
                                    item: name,
                                },
                                r#type: convert_type(field.item.r#type),
                            },
                        })
                        .collect(),
                )
            }
            wipple_typecheck::TypeRepresentation::Enumeration(variants) => {
                wipple_lower::TypeRepresentation::Enumeration(
                    variants
                        .into_iter()
                        .map(|(name, variant)| wipple_util::WithInfo {
                            info: variant.info.clone(),
                            item: wipple_lower::Variant {
                                name: wipple_util::WithInfo {
                                    info: variant.info,
                                    item: name,
                                },
                                types: variant
                                    .item
                                    .value_types
                                    .into_iter()
                                    .map(convert_type)
                                    .collect(),
                            },
                        })
                        .collect(),
                )
            }
            wipple_typecheck::TypeRepresentation::Wrapper(r#type) => {
                wipple_lower::TypeRepresentation::Wrapper(convert_type(r#type))
            }
        })
    }
}

pub mod lower {
    pub type Info = <crate::Driver as wipple_lower::Driver>::Info;

    pub fn convert(
        top_level: wipple_util::WithInfo<crate::Info, wipple_syntax::TopLevel<crate::SyntaxDriver>>,
    ) -> wipple_util::WithInfo<Info, wipple_lower::UnresolvedFile<crate::Driver>> {
        top_level.map(|top_level| wipple_lower::UnresolvedFile {
            statements: top_level
                .statements
                .into_iter()
                .map(convert_statement)
                .collect(),
        })
    }

    fn convert_statement(
        statement: wipple_util::WithInfo<
            crate::Info,
            wipple_syntax::Statement<crate::SyntaxDriver>,
        >,
    ) -> wipple_util::WithInfo<Info, wipple_lower::UnresolvedStatement<crate::Driver>> {
        statement.map(|statement| match statement {
            wipple_syntax::Statement::Type {
                name,
                parameters,
                representation,
            } => wipple_lower::UnresolvedStatement::Type {
                name: name.map_info(crate::Info::from),
                parameters: parameters.into_iter().map(convert_type_parameter).collect(),
                representation: convert_type_representation(representation),
            },
            wipple_syntax::Statement::Trait {
                name,
                parameters,
                r#type,
            } => wipple_lower::UnresolvedStatement::Trait {
                name,
                parameters: parameters.into_iter().map(convert_type_parameter).collect(),
                r#type: convert_type(r#type),
            },
            wipple_syntax::Statement::Constant {
                name,
                parameters,
                bounds,
                r#type,
                body,
            } => wipple_lower::UnresolvedStatement::Constant {
                name,
                parameters: parameters.into_iter().map(convert_type_parameter).collect(),
                bounds: bounds.into_iter().map(convert_instance).collect(),
                r#type: convert_type(r#type),
                body: convert_expression(body),
            },
            wipple_syntax::Statement::Instance {
                parameters,
                bounds,
                instance,
                body,
            } => wipple_lower::UnresolvedStatement::Instance {
                parameters: parameters.into_iter().map(convert_type_parameter).collect(),
                bounds: bounds.into_iter().map(convert_instance).collect(),
                instance: convert_instance(instance),
                body: convert_expression(body),
            },
            wipple_syntax::Statement::Language { name, kind, item } => {
                let kind = kind.map(|kind| match kind {
                    wipple_syntax::LanguageDeclarationKind::Type => {
                        wipple_lower::LanguageDeclarationKind::Type
                    }
                    wipple_syntax::LanguageDeclarationKind::Trait => {
                        wipple_lower::LanguageDeclarationKind::Trait
                    }
                    wipple_syntax::LanguageDeclarationKind::Constant => {
                        wipple_lower::LanguageDeclarationKind::Constant
                    }
                });

                wipple_lower::UnresolvedStatement::Language { name, kind, item }
            }
            wipple_syntax::Statement::Assignment { pattern, value } => {
                wipple_lower::UnresolvedStatement::Assignment {
                    pattern: convert_pattern(pattern),
                    value: convert_expression(value),
                }
            }
            wipple_syntax::Statement::Expression(expression) => {
                wipple_lower::UnresolvedStatement::Expression(convert_expression(expression))
            }
        })
    }

    fn convert_type_parameter(
        type_parameter: wipple_util::WithInfo<
            crate::Info,
            wipple_syntax::TypeParameter<crate::SyntaxDriver>,
        >,
    ) -> wipple_util::WithInfo<Info, wipple_lower::UnresolvedTypeParameter<crate::Driver>> {
        type_parameter.map(|type_parameter| wipple_lower::UnresolvedTypeParameter {
            name: type_parameter.name,
            infer: type_parameter.infer,
            default: type_parameter.default.map(convert_type),
        })
    }

    fn convert_type_representation(
        type_representation: wipple_util::WithInfo<
            crate::Info,
            wipple_syntax::TypeRepresentation<crate::SyntaxDriver>,
        >,
    ) -> wipple_util::WithInfo<Info, wipple_lower::UnresolvedTypeRepresentation<crate::Driver>>
    {
        type_representation.map(|type_representation| match type_representation {
            wipple_syntax::TypeRepresentation::Marker => {
                wipple_lower::UnresolvedTypeRepresentation::Marker
            }
            wipple_syntax::TypeRepresentation::Structure(fields) => {
                wipple_lower::UnresolvedTypeRepresentation::Structure(
                    fields
                        .into_iter()
                        .map(|field| {
                            field.map(|field| wipple_lower::UnresolvedField {
                                name: field.name,
                                r#type: convert_type(field.r#type),
                            })
                        })
                        .collect(),
                )
            }
            wipple_syntax::TypeRepresentation::Enumeration(variants) => {
                wipple_lower::UnresolvedTypeRepresentation::Enumeration(
                    variants
                        .into_iter()
                        .map(|variant| {
                            variant.map(|variant| wipple_lower::UnresolvedVariant {
                                name: variant.name,
                                types: variant.types.into_iter().map(convert_type).collect(),
                            })
                        })
                        .collect(),
                )
            }
            wipple_syntax::TypeRepresentation::Wrapper(r#type) => {
                wipple_lower::UnresolvedTypeRepresentation::Wrapper(convert_type(r#type))
            }
        })
    }

    fn convert_type(
        r#type: wipple_util::WithInfo<crate::Info, wipple_syntax::Type<crate::SyntaxDriver>>,
    ) -> wipple_util::WithInfo<Info, wipple_lower::UnresolvedType<crate::Driver>> {
        r#type.map(|r#type| match r#type {
            wipple_syntax::Type::Error => wipple_lower::UnresolvedType::Error,
            wipple_syntax::Type::Placeholder => wipple_lower::UnresolvedType::Placeholder,
            wipple_syntax::Type::Declared { name, parameters } => {
                wipple_lower::UnresolvedType::Declared {
                    name,
                    parameters: parameters.into_iter().map(convert_type).collect(),
                }
            }
            wipple_syntax::Type::Function { inputs, output } => {
                wipple_lower::UnresolvedType::Function {
                    inputs: inputs.into_iter().map(convert_type).collect(),
                    output: convert_type(output.unboxed()).boxed(),
                }
            }
            wipple_syntax::Type::Tuple(elements) => wipple_lower::UnresolvedType::Tuple(
                elements.into_iter().map(convert_type).collect(),
            ),
            wipple_syntax::Type::Block(r#type) => {
                wipple_lower::UnresolvedType::Block(convert_type(r#type.unboxed()).boxed())
            }
            wipple_syntax::Type::Intrinsic => wipple_lower::UnresolvedType::Intrinsic,
        })
    }

    pub fn unconvert_type<
        D: wipple_syntax::Driver<Info = <crate::Driver as wipple_lower::Driver>::Info>,
    >(
        r#type: wipple_util::WithInfo<Info, wipple_lower::Type<crate::Driver>>,
    ) -> wipple_util::WithInfo<crate::Info, wipple_syntax::Type<D>> {
        let info = r#type.info.clone();

        r#type.map(|r#type| match r#type {
            wipple_lower::Type::Error => wipple_syntax::Type::Error,
            wipple_lower::Type::Placeholder => wipple_syntax::Type::Placeholder,
            wipple_lower::Type::Declared { path, parameters } => {
                let name = path.map(|mut path| path.pop().unwrap().name().map(ToString::to_string));

                wipple_syntax::Type::Declared {
                    name,
                    parameters: parameters.into_iter().map(unconvert_type).collect(),
                }
            }
            wipple_lower::Type::Parameter(mut parameter) => {
                let name = wipple_util::WithInfo {
                    info,
                    item: parameter.pop().unwrap().name().map(ToString::to_string),
                };

                wipple_syntax::Type::Declared {
                    name,
                    parameters: vec![],
                }
            }
            wipple_lower::Type::Function { inputs, output } => wipple_syntax::Type::Function {
                inputs: inputs.into_iter().map(unconvert_type).collect(),
                output: unconvert_type(output.unboxed()).boxed(),
            },
            wipple_lower::Type::Tuple(elements) => {
                wipple_syntax::Type::Tuple(elements.into_iter().map(unconvert_type).collect())
            }
            wipple_lower::Type::Block(r#type) => {
                wipple_syntax::Type::Block(unconvert_type(r#type.unboxed()).boxed())
            }
            wipple_lower::Type::Intrinsic => wipple_syntax::Type::Intrinsic,
        })
    }

    fn convert_instance(
        instance: wipple_util::WithInfo<crate::Info, wipple_syntax::Instance<crate::SyntaxDriver>>,
    ) -> wipple_util::WithInfo<Info, wipple_lower::UnresolvedInstance<crate::Driver>> {
        instance.map(|instance| wipple_lower::UnresolvedInstance {
            r#trait: instance.r#trait,
            parameters: instance.parameters.into_iter().map(convert_type).collect(),
        })
    }

    fn convert_expression(
        expression: wipple_util::WithInfo<
            crate::Info,
            wipple_syntax::Expression<crate::SyntaxDriver>,
        >,
    ) -> wipple_util::WithInfo<Info, wipple_lower::UnresolvedExpression<crate::Driver>> {
        expression.map(|expression| match expression {
            wipple_syntax::Expression::Error => wipple_lower::UnresolvedExpression::Error,
            wipple_syntax::Expression::Annotate { value, r#type } => {
                wipple_lower::UnresolvedExpression::Annotate {
                    value: convert_expression(value.unboxed()).boxed(),
                    r#type: convert_type(r#type),
                }
            }
            wipple_syntax::Expression::Name(name) => wipple_lower::UnresolvedExpression::Name(name),
            wipple_syntax::Expression::Number(number) => {
                wipple_lower::UnresolvedExpression::Number(number)
            }
            wipple_syntax::Expression::Text(text) => wipple_lower::UnresolvedExpression::Text(text),
            wipple_syntax::Expression::Format { segments, trailing } => {
                wipple_lower::UnresolvedExpression::Format {
                    segments: segments
                        .into_iter()
                        .map(|segment| wipple_lower::FormatSegment {
                            text: segment.text,
                            value: convert_expression(segment.value),
                        })
                        .collect(),
                    trailing,
                }
            }
            wipple_syntax::Expression::Block(statements) => {
                wipple_lower::UnresolvedExpression::Block(
                    statements.into_iter().map(convert_statement).collect(),
                )
            }
            wipple_syntax::Expression::Do(block) => {
                wipple_lower::UnresolvedExpression::Do(convert_expression(block.unboxed()).boxed())
            }
            wipple_syntax::Expression::Function { inputs, body } => {
                wipple_lower::UnresolvedExpression::Function {
                    inputs: inputs.into_iter().map(convert_pattern).collect(),
                    body: convert_expression(body.unboxed()).boxed(),
                }
            }
            wipple_syntax::Expression::Call { function, inputs } => {
                wipple_lower::UnresolvedExpression::Call {
                    function: convert_expression(function.unboxed()).boxed(),
                    inputs: inputs.into_iter().map(convert_expression).collect(),
                }
            }
            wipple_syntax::Expression::Apply { input, function } => {
                wipple_lower::UnresolvedExpression::Apply {
                    input: convert_expression(input.unboxed()).boxed(),
                    function: convert_expression(function.unboxed()).boxed(),
                }
            }
            wipple_syntax::Expression::BinaryOperator {
                operator,
                left,
                right,
            } => wipple_lower::UnresolvedExpression::BinaryOperator {
                operator: convert_binary_operator(operator),
                left: convert_expression(left.unboxed()).boxed(),
                right: convert_expression(right.unboxed()).boxed(),
            },
            wipple_syntax::Expression::As { value, r#type } => {
                wipple_lower::UnresolvedExpression::As {
                    value: convert_expression(value.unboxed()).boxed(),
                    r#type: convert_type(r#type),
                }
            }
            wipple_syntax::Expression::Is { value, pattern } => {
                wipple_lower::UnresolvedExpression::Is {
                    value: convert_expression(value.unboxed()).boxed(),
                    pattern: convert_pattern(pattern),
                }
            }
            wipple_syntax::Expression::When { input, arms } => {
                wipple_lower::UnresolvedExpression::When {
                    input: convert_expression(input.unboxed()).boxed(),
                    arms: arms
                        .into_iter()
                        .map(|arm| {
                            arm.map(|arm| wipple_lower::UnresolvedArm {
                                pattern: convert_pattern(arm.pattern),
                                body: convert_expression(arm.body),
                            })
                        })
                        .collect(),
                }
            }
            wipple_syntax::Expression::Intrinsic { name, inputs } => {
                wipple_lower::UnresolvedExpression::Intrinsic {
                    name,
                    inputs: inputs.into_iter().map(convert_expression).collect(),
                }
            }
            wipple_syntax::Expression::Tuple(elements) => {
                wipple_lower::UnresolvedExpression::Tuple(
                    elements.into_iter().map(convert_expression).collect(),
                )
            }
            wipple_syntax::Expression::Collection(elements) => {
                wipple_lower::UnresolvedExpression::Collection(
                    elements.into_iter().map(convert_expression).collect(),
                )
            }
            wipple_syntax::Expression::Structure(fields) => {
                wipple_lower::UnresolvedExpression::Structure(
                    fields
                        .into_iter()
                        .map(|field| {
                            field.map(|field| wipple_lower::UnresolvedFieldValue {
                                name: field.name,
                                value: convert_expression(field.value),
                            })
                        })
                        .collect(),
                )
            }
        })
    }

    fn convert_pattern(
        pattern: wipple_util::WithInfo<crate::Info, wipple_syntax::Pattern<crate::SyntaxDriver>>,
    ) -> wipple_util::WithInfo<Info, wipple_lower::UnresolvedPattern<crate::Driver>> {
        pattern.map(|pattern| match pattern {
            wipple_syntax::Pattern::Error => wipple_lower::UnresolvedPattern::Error,
            wipple_syntax::Pattern::Name(name) => wipple_lower::UnresolvedPattern::Name(name),
            wipple_syntax::Pattern::Number(number) => {
                wipple_lower::UnresolvedPattern::Number(number)
            }
            wipple_syntax::Pattern::Text(text) => wipple_lower::UnresolvedPattern::Text(text),
            wipple_syntax::Pattern::Wildcard => wipple_lower::UnresolvedPattern::Wildcard,
            wipple_syntax::Pattern::VariantOrName(name) => {
                wipple_lower::UnresolvedPattern::VariantOrName(name.item)
            }
            wipple_syntax::Pattern::Destructure(fields) => {
                wipple_lower::UnresolvedPattern::Destructure(
                    fields
                        .into_iter()
                        .map(|field| {
                            field.map(|field| wipple_lower::UnresolvedFieldPattern {
                                name: field.name,
                                pattern: convert_pattern(field.pattern),
                            })
                        })
                        .collect(),
                )
            }
            wipple_syntax::Pattern::Variant {
                variant,
                value_patterns,
            } => wipple_lower::UnresolvedPattern::Variant {
                variant,
                value_patterns: value_patterns.into_iter().map(convert_pattern).collect(),
            },
            wipple_syntax::Pattern::Tuple(elements) => wipple_lower::UnresolvedPattern::Tuple(
                elements.into_iter().map(convert_pattern).collect(),
            ),
            wipple_syntax::Pattern::Or { left, right } => wipple_lower::UnresolvedPattern::Or {
                left: convert_pattern(left.unboxed()).boxed(),
                right: convert_pattern(right.unboxed()).boxed(),
            },
            wipple_syntax::Pattern::Mutate(name) => wipple_lower::UnresolvedPattern::Mutate(name),
        })
    }

    fn convert_binary_operator(
        binary_operator: wipple_util::WithInfo<crate::Info, wipple_syntax::BinaryOperator>,
    ) -> wipple_util::WithInfo<Info, wipple_lower::UnresolvedBinaryOperator> {
        binary_operator.map(|binary_operator| match binary_operator {
            wipple_syntax::BinaryOperator::LessThan => {
                wipple_lower::UnresolvedBinaryOperator::LessThan
            }
            wipple_syntax::BinaryOperator::GreaterThan => {
                wipple_lower::UnresolvedBinaryOperator::GreaterThan
            }
            wipple_syntax::BinaryOperator::LessThanOrEqual => {
                wipple_lower::UnresolvedBinaryOperator::LessThanOrEqual
            }
            wipple_syntax::BinaryOperator::GreaterThanOrEqual => {
                wipple_lower::UnresolvedBinaryOperator::GreaterThanOrEqual
            }
            wipple_syntax::BinaryOperator::Equal => wipple_lower::UnresolvedBinaryOperator::Equal,
            wipple_syntax::BinaryOperator::NotEqual => {
                wipple_lower::UnresolvedBinaryOperator::NotEqual
            }
            wipple_syntax::BinaryOperator::Add => wipple_lower::UnresolvedBinaryOperator::Add,
            wipple_syntax::BinaryOperator::Subtract => {
                wipple_lower::UnresolvedBinaryOperator::Subtract
            }
            wipple_syntax::BinaryOperator::Multiply => {
                wipple_lower::UnresolvedBinaryOperator::Multiply
            }
            wipple_syntax::BinaryOperator::Divide => wipple_lower::UnresolvedBinaryOperator::Divide,
            wipple_syntax::BinaryOperator::Remainder => {
                wipple_lower::UnresolvedBinaryOperator::Remainder
            }
            wipple_syntax::BinaryOperator::Power => wipple_lower::UnresolvedBinaryOperator::Power,
            wipple_syntax::BinaryOperator::And => wipple_lower::UnresolvedBinaryOperator::And,
            wipple_syntax::BinaryOperator::Or => wipple_lower::UnresolvedBinaryOperator::Or,
            wipple_syntax::BinaryOperator::To => wipple_lower::UnresolvedBinaryOperator::To,
            wipple_syntax::BinaryOperator::By => wipple_lower::UnresolvedBinaryOperator::By,
        })
    }
}

pub mod typecheck {
    pub type Info = <crate::Driver as wipple_typecheck::Driver>::Info;

    pub fn convert_type_declaration(
        type_declaration: wipple_util::WithInfo<
            crate::Info,
            wipple_lower::TypeDeclaration<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<Info, wipple_typecheck::TypeDeclaration<crate::Driver>> {
        type_declaration.map(|type_declaration| wipple_typecheck::TypeDeclaration {
            parameters: type_declaration.parameters,
            representation: type_declaration.representation.map(|type_representation| {
                match type_representation {
                    wipple_lower::TypeRepresentation::Marker => {
                        wipple_typecheck::TypeRepresentation::Marker
                    }
                    wipple_lower::TypeRepresentation::Structure(fields) => {
                        wipple_typecheck::TypeRepresentation::Structure(
                            fields
                                .into_iter()
                                .map(|field| {
                                    (
                                        field.item.name.item,
                                        wipple_util::WithInfo {
                                            info: field.info,
                                            item: wipple_typecheck::StructureField {
                                                r#type: convert_type(field.item.r#type),
                                            },
                                        },
                                    )
                                })
                                .collect(),
                        )
                    }
                    wipple_lower::TypeRepresentation::Enumeration(variants) => {
                        wipple_typecheck::TypeRepresentation::Enumeration(
                            variants
                                .into_iter()
                                .map(|variant| {
                                    (
                                        variant.item.name.item,
                                        wipple_util::WithInfo {
                                            info: variant.info,
                                            item: wipple_typecheck::EnumerationVariant {
                                                value_types: variant
                                                    .item
                                                    .types
                                                    .into_iter()
                                                    .map(convert_type)
                                                    .collect(),
                                            },
                                        },
                                    )
                                })
                                .collect(),
                        )
                    }
                    wipple_lower::TypeRepresentation::Wrapper(r#type) => {
                        wipple_typecheck::TypeRepresentation::Wrapper(convert_type(r#type))
                    }
                }
            }),
        })
    }

    pub fn convert_trait_declaration(
        trait_declaration: wipple_util::WithInfo<
            crate::Info,
            wipple_lower::TraitDeclaration<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<Info, wipple_typecheck::TraitDeclaration<crate::Driver>> {
        trait_declaration.map(|trait_declaration| wipple_typecheck::TraitDeclaration {
            parameters: trait_declaration.parameters,
            r#type: convert_type(trait_declaration.r#type),
        })
    }

    pub fn convert_type_parameter_declaration(
        type_parameter_declaration: wipple_util::WithInfo<
            crate::Info,
            wipple_lower::TypeParameterDeclaration<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<Info, wipple_typecheck::TypeParameterDeclaration<crate::Driver>>
    {
        type_parameter_declaration.map(|type_parameter_declaration| {
            wipple_typecheck::TypeParameterDeclaration {
                infer: type_parameter_declaration.infer,
                default: type_parameter_declaration.default.map(convert_type),
            }
        })
    }

    pub fn convert_constant_declaration(
        constant_declaration: wipple_util::WithInfo<
            crate::Info,
            wipple_lower::ConstantDeclaration<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<Info, wipple_typecheck::ConstantDeclaration<crate::Driver>> {
        constant_declaration.map(
            |constant_declaration| wipple_typecheck::ConstantDeclaration {
                parameters: constant_declaration.parameters,
                bounds: constant_declaration
                    .bounds
                    .into_iter()
                    .map(convert_instance)
                    .collect(),
                r#type: convert_type(constant_declaration.r#type),
            },
        )
    }

    pub fn convert_instance_declaration(
        instance_declaration: wipple_util::WithInfo<
            crate::Info,
            wipple_lower::InstanceDeclaration<crate::Driver>,
        >,
    ) -> wipple_util::WithInfo<Info, wipple_typecheck::InstanceDeclaration<crate::Driver>> {
        instance_declaration.map(
            |instance_declaration| wipple_typecheck::InstanceDeclaration {
                parameters: instance_declaration.parameters,
                bounds: instance_declaration
                    .bounds
                    .into_iter()
                    .map(convert_instance)
                    .collect(),
                instance: convert_instance(instance_declaration.instance),
            },
        )
    }

    pub fn convert_type(
        r#type: wipple_util::WithInfo<crate::Info, wipple_lower::Type<crate::Driver>>,
    ) -> wipple_util::WithInfo<Info, wipple_typecheck::Type<crate::Driver>> {
        r#type.map(|r#type| match r#type {
            wipple_lower::Type::Error => {
                wipple_typecheck::Type::Unknown(wipple_typecheck::UnknownTypeId::none())
            }
            wipple_lower::Type::Placeholder => {
                wipple_typecheck::Type::Unknown(wipple_typecheck::UnknownTypeId::none())
            }
            wipple_lower::Type::Declared { path, parameters } => wipple_typecheck::Type::Declared {
                path: path.item,
                parameters: parameters.into_iter().map(convert_type).collect(),
            },
            wipple_lower::Type::Parameter(parameter) => {
                wipple_typecheck::Type::Parameter(parameter)
            }
            wipple_lower::Type::Function { inputs, output } => wipple_typecheck::Type::Function {
                inputs: inputs.into_iter().map(convert_type).collect(),
                output: convert_type(output.unboxed()).boxed(),
            },
            wipple_lower::Type::Tuple(elements) => {
                wipple_typecheck::Type::Tuple(elements.into_iter().map(convert_type).collect())
            }
            wipple_lower::Type::Block(r#type) => {
                wipple_typecheck::Type::Block(convert_type(r#type.unboxed()).boxed())
            }
            wipple_lower::Type::Intrinsic => wipple_typecheck::Type::Intrinsic,
        })
    }

    pub fn unconvert_type(
        r#type: wipple_util::WithInfo<Info, wipple_typecheck::Type<crate::Driver>>,
    ) -> wipple_util::WithInfo<crate::Info, wipple_lower::Type<crate::Driver>> {
        let info = r#type.info.clone();

        r#type.map(|r#type| match r#type {
            wipple_typecheck::Type::Unknown(_) => wipple_lower::Type::Error,
            wipple_typecheck::Type::Parameter(parameter) => {
                wipple_lower::Type::Parameter(parameter)
            }
            wipple_typecheck::Type::Declared { path, parameters } => wipple_lower::Type::Declared {
                path: wipple_util::WithInfo { info, item: path },
                parameters: parameters.into_iter().map(unconvert_type).collect(),
            },
            wipple_typecheck::Type::Function { inputs, output } => wipple_lower::Type::Function {
                inputs: inputs.into_iter().map(unconvert_type).collect(),
                output: unconvert_type(output.unboxed()).boxed(),
            },
            wipple_typecheck::Type::Tuple(elements) => {
                wipple_lower::Type::Tuple(elements.into_iter().map(unconvert_type).collect())
            }
            wipple_typecheck::Type::Block(r#type) => {
                wipple_lower::Type::Block(unconvert_type(r#type.unboxed()).boxed())
            }
            wipple_typecheck::Type::Intrinsic => wipple_lower::Type::Intrinsic,
        })
    }

    pub fn convert_instance(
        instance: wipple_util::WithInfo<crate::Info, wipple_lower::Instance<crate::Driver>>,
    ) -> wipple_util::WithInfo<Info, wipple_typecheck::Instance<crate::Driver>> {
        instance.map(|instance| wipple_typecheck::Instance {
            r#trait: instance.r#trait.item,
            parameters: instance.parameters.into_iter().map(convert_type).collect(),
        })
    }

    pub fn convert_expression(
        expression: wipple_util::WithInfo<crate::Info, wipple_lower::Expression<crate::Driver>>,
    ) -> wipple_util::WithInfo<Info, wipple_typecheck::UntypedExpression<crate::Driver>> {
        expression.map(|expression| match expression {
            wipple_lower::Expression::Error => wipple_typecheck::UntypedExpression::Unknown,
            wipple_lower::Expression::Assign { pattern, value } => {
                wipple_typecheck::UntypedExpression::Initialize {
                    pattern: convert_pattern(pattern),
                    value: convert_expression(value.unboxed()).boxed(),
                }
            }
            wipple_lower::Expression::Mutate { name, path, value } => {
                wipple_typecheck::UntypedExpression::Mutate {
                    name: name.item,
                    path,
                    value: convert_expression(value.unboxed()).boxed(),
                }
            }
            wipple_lower::Expression::Annotate { value, r#type } => {
                wipple_typecheck::UntypedExpression::Annotate {
                    value: convert_expression(value.unboxed()).boxed(),
                    r#type: convert_type(r#type),
                }
            }
            wipple_lower::Expression::Variable(name, variable) => {
                wipple_typecheck::UntypedExpression::Variable(name, variable)
            }
            wipple_lower::Expression::Number(number) => {
                wipple_typecheck::UntypedExpression::Number(number)
            }
            wipple_lower::Expression::Text(text) => wipple_typecheck::UntypedExpression::Text(text),
            wipple_lower::Expression::Constant(path) => {
                wipple_typecheck::UntypedExpression::Constant(path)
            }
            wipple_lower::Expression::Trait(path) => {
                wipple_typecheck::UntypedExpression::Trait(path)
            }
            wipple_lower::Expression::Format { segments, trailing } => {
                wipple_typecheck::UntypedExpression::Format {
                    segments: segments
                        .into_iter()
                        .map(|segment| wipple_typecheck::UntypedFormatSegment {
                            text: segment.text,
                            value: convert_expression(segment.value),
                        })
                        .collect(),
                    trailing,
                }
            }
            wipple_lower::Expression::Block(statements) => {
                wipple_typecheck::UntypedExpression::Block(
                    statements.into_iter().map(convert_expression).collect(),
                )
            }
            wipple_lower::Expression::Do(block) => {
                wipple_typecheck::UntypedExpression::Do(convert_expression(block.unboxed()).boxed())
            }
            wipple_lower::Expression::Function { inputs, body } => {
                wipple_typecheck::UntypedExpression::Function {
                    inputs: inputs.into_iter().map(convert_pattern).collect(),
                    body: convert_expression(body.unboxed()).boxed(),
                }
            }
            wipple_lower::Expression::Call { function, inputs } => {
                wipple_typecheck::UntypedExpression::Call {
                    function: convert_expression(function.unboxed()).boxed(),
                    inputs: inputs.into_iter().map(convert_expression).collect(),
                }
            }
            wipple_lower::Expression::When { input, arms } => {
                wipple_typecheck::UntypedExpression::When {
                    input: convert_expression(input.unboxed()).boxed(),
                    arms: arms
                        .into_iter()
                        .map(|arm| {
                            arm.map(|arm| wipple_typecheck::UntypedArm {
                                pattern: convert_pattern(arm.pattern),
                                body: convert_expression(arm.body),
                            })
                        })
                        .collect(),
                }
            }
            wipple_lower::Expression::Intrinsic { name, inputs } => {
                wipple_typecheck::UntypedExpression::Intrinsic {
                    name: name.item,
                    inputs: inputs.into_iter().map(convert_expression).collect(),
                }
            }
            wipple_lower::Expression::Tuple(elements) => {
                wipple_typecheck::UntypedExpression::Tuple(
                    elements.into_iter().map(convert_expression).collect(),
                )
            }
            wipple_lower::Expression::Collection(elements) => {
                wipple_typecheck::UntypedExpression::Collection(
                    elements.into_iter().map(convert_expression).collect(),
                )
            }
            wipple_lower::Expression::Marker(path) => {
                wipple_typecheck::UntypedExpression::Marker(path)
            }
            wipple_lower::Expression::Structure(elements) => {
                wipple_typecheck::UntypedExpression::Structure(
                    elements
                        .into_iter()
                        .filter_map(|element| {
                            element.filter_map(|element| {
                                Some(wipple_typecheck::UntypedStructureFieldValue {
                                    name: element.name.item?,
                                    value: convert_expression(element.value),
                                })
                            })
                        })
                        .collect(),
                )
            }
            wipple_lower::Expression::Variant { variant, values } => {
                wipple_typecheck::UntypedExpression::Variant {
                    variant,
                    values: values.into_iter().map(convert_expression).collect(),
                }
            }
            wipple_lower::Expression::Wrapper { r#type, value } => {
                wipple_typecheck::UntypedExpression::Wrapper {
                    r#type,
                    value: convert_expression(value.unboxed()).boxed(),
                }
            }
        })
    }

    pub fn convert_pattern(
        pattern: wipple_util::WithInfo<crate::Info, wipple_lower::Pattern<crate::Driver>>,
    ) -> wipple_util::WithInfo<Info, wipple_typecheck::Pattern<crate::Driver>> {
        pattern.map(|pattern| match pattern {
            wipple_lower::Pattern::Error => wipple_typecheck::Pattern::Unknown,
            wipple_lower::Pattern::Wildcard => wipple_typecheck::Pattern::Wildcard,
            wipple_lower::Pattern::Number(number) => wipple_typecheck::Pattern::Number(number),
            wipple_lower::Pattern::Text(text) => wipple_typecheck::Pattern::Text(text),
            wipple_lower::Pattern::Variable(name, variable) => {
                wipple_typecheck::Pattern::Variable(name, variable)
            }
            wipple_lower::Pattern::Destructure(fields) => wipple_typecheck::Pattern::Destructure(
                fields
                    .into_iter()
                    .filter_map(|field| {
                        field.filter_map(|field| {
                            Some(wipple_typecheck::FieldPattern {
                                name: field.name.item?,
                                pattern: convert_pattern(field.pattern),
                            })
                        })
                    })
                    .collect(),
            ),
            wipple_lower::Pattern::Variant {
                variant,
                value_patterns,
            } => wipple_typecheck::Pattern::Variant {
                variant,
                value_patterns: value_patterns.into_iter().map(convert_pattern).collect(),
            },
            wipple_lower::Pattern::Wrapper {
                path,
                value_pattern,
            } => wipple_typecheck::Pattern::Wrapper {
                path,
                value_pattern: convert_pattern(value_pattern.unboxed()).boxed(),
            },
            wipple_lower::Pattern::Tuple(elements) => wipple_typecheck::Pattern::Tuple(
                elements.into_iter().map(convert_pattern).collect(),
            ),
            wipple_lower::Pattern::Or { left, right } => wipple_typecheck::Pattern::Or {
                left: convert_pattern(left.unboxed()).boxed(),
                right: convert_pattern(right.unboxed()).boxed(),
            },
        })
    }
}
