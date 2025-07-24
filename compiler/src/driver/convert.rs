pub mod interface {
    pub fn convert_attribute(
        attribute: crate::util::WithInfo<crate::typecheck::Attribute>,
    ) -> crate::util::WithInfo<crate::lower::Attribute> {
        attribute.map(|attribute| match attribute {
            crate::typecheck::Attribute::Error => crate::lower::Attribute::Error,
            crate::typecheck::Attribute::Name(name) => crate::lower::Attribute::Name(name),
            crate::typecheck::Attribute::Valued { name, value } => {
                crate::lower::Attribute::Valued {
                    name,
                    value: convert_attribute_value(value),
                }
            }
        })
    }

    pub fn convert_attribute_value(
        attribute_value: crate::util::WithInfo<crate::typecheck::AttributeValue>,
    ) -> crate::util::WithInfo<crate::lower::AttributeValue> {
        attribute_value.map(|attribute_value| match attribute_value {
            crate::typecheck::AttributeValue::Error => crate::lower::AttributeValue::Error,
            crate::typecheck::AttributeValue::Name(name) => {
                crate::lower::AttributeValue::Name(name)
            }
            crate::typecheck::AttributeValue::Number(number) => {
                crate::lower::AttributeValue::Number(number)
            }
            crate::typecheck::AttributeValue::Text(text) => {
                crate::lower::AttributeValue::Text(text)
            }
        })
    }

    pub fn convert_syntax_declaration(
        syntax_declaration: crate::util::WithInfo<crate::typecheck::SyntaxDeclaration>,
    ) -> crate::util::WithInfo<crate::lower::SyntaxDeclaration> {
        syntax_declaration.map(|syntax_declaration| crate::lower::SyntaxDeclaration {
            attributes: syntax_declaration
                .attributes
                .into_iter()
                .map(convert_attribute)
                .collect(),
        })
    }

    pub fn convert_type_declaration(
        type_declaration: crate::util::WithInfo<crate::typecheck::TypeDeclaration>,
    ) -> crate::util::WithInfo<crate::lower::TypeDeclaration> {
        type_declaration.map(|type_declaration| crate::lower::TypeDeclaration {
            attributes: type_declaration
                .attributes
                .into_iter()
                .map(convert_attribute)
                .collect(),
            parameters: type_declaration.parameters,
            representation: convert_type_representation(type_declaration.representation),
        })
    }

    pub fn convert_trait_declaration(
        trait_declaration: crate::util::WithInfo<crate::typecheck::TraitDeclaration>,
    ) -> crate::util::WithInfo<crate::lower::TraitDeclaration> {
        trait_declaration.map(|trait_declaration| crate::lower::TraitDeclaration {
            attributes: trait_declaration
                .attributes
                .into_iter()
                .map(convert_attribute)
                .collect(),
            parameters: trait_declaration.parameters,
            r#type: trait_declaration.r#type.map(convert_type),
        })
    }

    pub fn convert_type_parameter_declaration(
        type_parameter_declaration: crate::util::WithInfo<
            crate::typecheck::TypeParameterDeclaration,
        >,
    ) -> crate::util::WithInfo<crate::lower::TypeParameterDeclaration> {
        type_parameter_declaration.map(|type_parameter_declaration| {
            crate::lower::TypeParameterDeclaration {
                infer: type_parameter_declaration.infer,
                default: type_parameter_declaration.default.map(convert_type),
            }
        })
    }

    pub fn convert_constant_declaration(
        constant_declaration: crate::util::WithInfo<crate::typecheck::ConstantDeclaration>,
    ) -> crate::util::WithInfo<crate::lower::ConstantDeclaration> {
        constant_declaration.map(|constant_declaration| crate::lower::ConstantDeclaration {
            attributes: constant_declaration
                .attributes
                .into_iter()
                .map(convert_attribute)
                .collect(),
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
        instance_declaration: crate::util::WithInfo<crate::typecheck::InstanceDeclaration>,
    ) -> crate::util::WithInfo<crate::lower::InstanceDeclaration> {
        instance_declaration.map(|instance_declaration| crate::lower::InstanceDeclaration {
            parameters: instance_declaration.parameters,
            bounds: instance_declaration
                .bounds
                .into_iter()
                .map(convert_instance)
                .collect(),
            instance: convert_instance(instance_declaration.instance),
            default: instance_declaration.default,
        })
    }

    pub fn convert_type(
        r#type: crate::util::WithInfo<crate::typecheck::Type>,
    ) -> crate::util::WithInfo<crate::lower::Type> {
        let type_info = r#type.info.clone();

        r#type.map(|r#type| match r#type {
            crate::typecheck::Type::Unknown => crate::lower::Type::Error,
            crate::typecheck::Type::Parameter(parameter) => {
                crate::lower::Type::Parameter(parameter)
            }
            crate::typecheck::Type::Declared { path, parameters } => crate::lower::Type::Declared {
                path: crate::util::WithInfo {
                    info: type_info.clone(),
                    item: path,
                },
                parameters: parameters.into_iter().map(convert_type).collect(),
            },
            crate::typecheck::Type::Function { inputs, output } => crate::lower::Type::Function {
                inputs: inputs.into_iter().map(convert_type).collect(),
                output: convert_type(output.unboxed()).boxed(),
            },
            crate::typecheck::Type::Tuple(elements) => {
                crate::lower::Type::Tuple(elements.into_iter().map(convert_type).collect())
            }
            crate::typecheck::Type::Block(r#type) => {
                crate::lower::Type::Block(convert_type(r#type.unboxed()).boxed())
            }
            crate::typecheck::Type::Intrinsic => crate::lower::Type::Intrinsic,
            crate::typecheck::Type::Message { segments, trailing } => crate::lower::Type::Message {
                segments: segments
                    .into_iter()
                    .map(|segment| crate::lower::FormatSegment {
                        text: segment.text,
                        value: convert_type(segment.r#type),
                    })
                    .collect(),
                trailing,
            },
            crate::typecheck::Type::Equal { left, right } => crate::lower::Type::Equal {
                left: convert_type(left.unboxed()).boxed(),
                right: convert_type(right.unboxed()).boxed(),
            },
        })
    }

    pub fn convert_instance(
        instance: crate::util::WithInfo<crate::typecheck::Instance>,
    ) -> crate::util::WithInfo<crate::lower::Instance> {
        let instance_info = instance.info.clone();

        instance.map(|instance| crate::lower::Instance {
            r#trait: crate::util::WithInfo {
                info: instance_info.clone(),
                item: instance.r#trait,
            },
            parameters: instance.parameters.into_iter().map(convert_type).collect(),
        })
    }

    pub fn convert_type_representation(
        type_representation: crate::util::WithInfo<crate::typecheck::TypeRepresentation>,
    ) -> crate::util::WithInfo<crate::lower::TypeRepresentation> {
        type_representation.map(|type_representation| match type_representation {
            crate::typecheck::TypeRepresentation::Marker => {
                crate::lower::TypeRepresentation::Marker
            }
            crate::typecheck::TypeRepresentation::Structure(fields) => {
                crate::lower::TypeRepresentation::Structure(
                    fields
                        .into_iter()
                        .map(|(name, field)| crate::util::WithInfo {
                            info: field.info.clone(),
                            item: crate::lower::Field {
                                index: field.item.index,
                                attributes: field
                                    .item
                                    .attributes
                                    .into_iter()
                                    .map(convert_attribute)
                                    .collect(),
                                name: crate::util::WithInfo {
                                    info: field.info,
                                    item: name,
                                },
                                r#type: convert_type(field.item.r#type),
                            },
                        })
                        .collect(),
                )
            }
            crate::typecheck::TypeRepresentation::Enumeration(variants) => {
                crate::lower::TypeRepresentation::Enumeration(
                    variants
                        .into_iter()
                        .map(|(name, variant)| crate::util::WithInfo {
                            info: variant.info.clone(),
                            item: crate::lower::Variant {
                                index: variant.item.index,
                                attributes: variant
                                    .item
                                    .attributes
                                    .into_iter()
                                    .map(convert_attribute)
                                    .collect(),
                                name: crate::util::WithInfo {
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
            crate::typecheck::TypeRepresentation::Wrapper(r#type) => {
                crate::lower::TypeRepresentation::Wrapper(convert_type(r#type))
            }
        })
    }
}

pub mod lower {
    pub fn convert(
        name: String,
        top_level: crate::util::WithInfo<crate::syntax::TopLevel>,
    ) -> crate::util::WithInfo<crate::lower::UnresolvedFile> {
        top_level.map(|top_level| crate::lower::UnresolvedFile {
            name,
            statements: top_level
                .statements
                .into_iter()
                .map(convert_statement)
                .collect(),
        })
    }

    fn convert_attribute(
        attribute: crate::util::WithInfo<crate::syntax::Attribute>,
    ) -> crate::util::WithInfo<crate::lower::Attribute> {
        attribute.map(|attribute| match attribute {
            crate::syntax::Attribute::Error => crate::lower::Attribute::Error,
            crate::syntax::Attribute::Name(name) => crate::lower::Attribute::Name(name),
            crate::syntax::Attribute::Valued { name, value } => crate::lower::Attribute::Valued {
                name,
                value: convert_attribute_value(value),
            },
        })
    }

    fn convert_attribute_value(
        attribute_value: crate::util::WithInfo<crate::syntax::AttributeValue>,
    ) -> crate::util::WithInfo<crate::lower::AttributeValue> {
        attribute_value.map(|attribute_value| match attribute_value {
            crate::syntax::AttributeValue::Error => crate::lower::AttributeValue::Error,
            crate::syntax::AttributeValue::Name(name) => crate::lower::AttributeValue::Name(name),
            crate::syntax::AttributeValue::Number(number) => {
                crate::lower::AttributeValue::Number(number)
            }
            crate::syntax::AttributeValue::Text(text) => crate::lower::AttributeValue::Text(text),
        })
    }

    fn convert_statement(
        statement: crate::util::WithInfo<crate::syntax::Statement>,
    ) -> crate::util::WithInfo<crate::lower::UnresolvedStatement> {
        statement.map(|statement| match statement {
            crate::syntax::Statement::Syntax { attributes, name } => {
                crate::lower::UnresolvedStatement::Syntax {
                    attributes: attributes.into_iter().map(convert_attribute).collect(),
                    name,
                }
            }
            crate::syntax::Statement::Type {
                attributes,
                name,
                parameters,
                representation,
            } => crate::lower::UnresolvedStatement::Type {
                attributes: attributes.into_iter().map(convert_attribute).collect(),
                name,
                parameters: parameters.into_iter().map(convert_type_parameter).collect(),
                representation: convert_type_representation(representation),
            },
            crate::syntax::Statement::Trait {
                attributes,
                name,
                parameters,
                r#type,
            } => crate::lower::UnresolvedStatement::Trait {
                attributes: attributes.into_iter().map(convert_attribute).collect(),
                name,
                parameters: parameters.into_iter().map(convert_type_parameter).collect(),
                r#type: r#type.map(convert_type),
            },
            crate::syntax::Statement::Constant {
                attributes,
                name,
                parameters,
                bounds,
                r#type,
                body,
            } => crate::lower::UnresolvedStatement::Constant {
                attributes: attributes.into_iter().map(convert_attribute).collect(),
                name,
                parameters: parameters.into_iter().map(convert_type_parameter).collect(),
                bounds: bounds.into_iter().map(convert_instance).collect(),
                r#type: convert_type(r#type),
                body: convert_expression(body),
            },
            crate::syntax::Statement::Instance {
                pattern,
                parameters,
                bounds,
                instance,
                body,
                default,
            } => crate::lower::UnresolvedStatement::Instance {
                pattern,
                parameters: parameters.into_iter().map(convert_type_parameter).collect(),
                bounds: bounds.into_iter().map(convert_instance).collect(),
                instance: convert_instance(instance),
                body: body.map(convert_expression),
                default,
            },
            crate::syntax::Statement::Assignment { pattern, value } => {
                crate::lower::UnresolvedStatement::Assignment {
                    pattern: convert_pattern(pattern),
                    value: convert_expression(value),
                }
            }
            crate::syntax::Statement::Expression(expression) => {
                crate::lower::UnresolvedStatement::Expression(convert_expression(expression))
            }
        })
    }

    fn convert_type_parameter(
        type_parameter: crate::util::WithInfo<crate::syntax::TypeParameter>,
    ) -> crate::util::WithInfo<crate::lower::UnresolvedTypeParameter> {
        type_parameter.map(|type_parameter| crate::lower::UnresolvedTypeParameter {
            name: type_parameter.name,
            infer: type_parameter.infer,
            default: type_parameter.default.map(convert_type),
        })
    }

    fn convert_type_representation(
        type_representation: crate::util::WithInfo<crate::syntax::TypeRepresentation>,
    ) -> crate::util::WithInfo<crate::lower::UnresolvedTypeRepresentation> {
        type_representation.map(|type_representation| match type_representation {
            crate::syntax::TypeRepresentation::Marker => {
                crate::lower::UnresolvedTypeRepresentation::Marker
            }
            crate::syntax::TypeRepresentation::Structure(fields) => {
                crate::lower::UnresolvedTypeRepresentation::Structure(
                    fields
                        .into_iter()
                        .enumerate()
                        .map(|(index, field)| {
                            field.map(|field| crate::lower::UnresolvedField {
                                index: index as u32,
                                attributes: field
                                    .attributes
                                    .into_iter()
                                    .map(convert_attribute)
                                    .collect(),
                                name: field.name,
                                r#type: convert_type(field.r#type),
                            })
                        })
                        .collect(),
                )
            }
            crate::syntax::TypeRepresentation::Enumeration(variants) => {
                crate::lower::UnresolvedTypeRepresentation::Enumeration(
                    variants
                        .into_iter()
                        .enumerate()
                        .map(|(index, variant)| {
                            variant.map(|variant| crate::lower::UnresolvedVariant {
                                index: index as u32,
                                attributes: variant
                                    .attributes
                                    .into_iter()
                                    .map(convert_attribute)
                                    .collect(),
                                name: variant.name,
                                types: variant.types.into_iter().map(convert_type).collect(),
                            })
                        })
                        .collect(),
                )
            }
            crate::syntax::TypeRepresentation::Wrapper(r#type) => {
                crate::lower::UnresolvedTypeRepresentation::Wrapper(convert_type(r#type))
            }
        })
    }

    fn convert_type(
        r#type: crate::util::WithInfo<crate::syntax::Type>,
    ) -> crate::util::WithInfo<crate::lower::UnresolvedType> {
        r#type.map(|r#type| match r#type {
            crate::syntax::Type::Error => crate::lower::UnresolvedType::Error,
            crate::syntax::Type::Placeholder => crate::lower::UnresolvedType::Placeholder,
            crate::syntax::Type::Declared { name, parameters } => {
                crate::lower::UnresolvedType::Declared {
                    name,
                    parameters: parameters.into_iter().map(convert_type).collect(),
                }
            }
            crate::syntax::Type::Function { inputs, output } => {
                crate::lower::UnresolvedType::Function {
                    inputs: inputs.into_iter().map(convert_type).collect(),
                    output: convert_type(output.unboxed()).boxed(),
                }
            }
            crate::syntax::Type::Tuple(elements) => crate::lower::UnresolvedType::Tuple(
                elements.into_iter().map(convert_type).collect(),
            ),
            crate::syntax::Type::Block(r#type) => {
                crate::lower::UnresolvedType::Block(convert_type(r#type.unboxed()).boxed())
            }
            crate::syntax::Type::Intrinsic => crate::lower::UnresolvedType::Intrinsic,
            crate::syntax::Type::Message { segments, trailing } => {
                crate::lower::UnresolvedType::Message {
                    segments: segments
                        .into_iter()
                        .map(|segment| crate::lower::FormatSegment {
                            text: segment.text,
                            value: convert_type(segment.value),
                        })
                        .collect(),
                    trailing,
                }
            }
            crate::syntax::Type::Equal { left, right } => crate::lower::UnresolvedType::Equal {
                left: convert_type(left.unboxed()).boxed(),
                right: convert_type(right.unboxed()).boxed(),
            },
        })
    }

    fn convert_instance(
        instance: crate::util::WithInfo<crate::syntax::Instance>,
    ) -> crate::util::WithInfo<crate::lower::UnresolvedInstance> {
        instance.map(|instance| crate::lower::UnresolvedInstance {
            r#trait: instance.r#trait,
            parameters: instance.parameters.into_iter().map(convert_type).collect(),
        })
    }

    fn convert_expression(
        expression: crate::util::WithInfo<crate::syntax::Expression>,
    ) -> crate::util::WithInfo<crate::lower::UnresolvedExpression> {
        expression.map(|expression| match expression {
            crate::syntax::Expression::Error => crate::lower::UnresolvedExpression::Error,
            crate::syntax::Expression::Annotate { value, r#type } => {
                crate::lower::UnresolvedExpression::Annotate {
                    value: convert_expression(value.unboxed()).boxed(),
                    r#type: convert_type(r#type),
                }
            }
            crate::syntax::Expression::Name(name) => crate::lower::UnresolvedExpression::Name(name),
            crate::syntax::Expression::Number(number) => {
                crate::lower::UnresolvedExpression::Number(number)
            }
            crate::syntax::Expression::Text(text) => crate::lower::UnresolvedExpression::Text(text),
            crate::syntax::Expression::Format { segments, trailing } => {
                crate::lower::UnresolvedExpression::Format {
                    segments: segments
                        .into_iter()
                        .map(|segment| crate::lower::FormatSegment {
                            text: segment.text,
                            value: convert_expression(segment.value),
                        })
                        .collect(),
                    trailing,
                }
            }
            crate::syntax::Expression::Block(statements) => {
                crate::lower::UnresolvedExpression::Block(
                    statements.into_iter().map(convert_statement).collect(),
                )
            }
            crate::syntax::Expression::Do(block) => {
                crate::lower::UnresolvedExpression::Do(convert_expression(block.unboxed()).boxed())
            }
            crate::syntax::Expression::Function { inputs, body } => {
                crate::lower::UnresolvedExpression::Function {
                    inputs: inputs.into_iter().map(convert_pattern).collect(),
                    body: convert_expression(body.unboxed()).boxed(),
                }
            }
            crate::syntax::Expression::Call { function, inputs } => {
                crate::lower::UnresolvedExpression::Call {
                    function: convert_expression(function.unboxed()).boxed(),
                    inputs: inputs.into_iter().map(convert_expression).collect(),
                }
            }
            crate::syntax::Expression::Apply { input, function } => {
                crate::lower::UnresolvedExpression::Apply {
                    input: convert_expression(input.unboxed()).boxed(),
                    function: convert_expression(function.unboxed()).boxed(),
                }
            }
            crate::syntax::Expression::BinaryOperator {
                operator,
                left,
                right,
            } => crate::lower::UnresolvedExpression::BinaryOperator {
                operator: convert_binary_operator(operator),
                left: convert_expression(left.unboxed()).boxed(),
                right: convert_expression(right.unboxed()).boxed(),
            },
            crate::syntax::Expression::As { value, r#type } => {
                crate::lower::UnresolvedExpression::As {
                    value: convert_expression(value.unboxed()).boxed(),
                    r#type: convert_type(r#type),
                }
            }
            crate::syntax::Expression::Is { value, pattern } => {
                crate::lower::UnresolvedExpression::Is {
                    value: convert_expression(value.unboxed()).boxed(),
                    pattern: convert_pattern(pattern),
                }
            }
            crate::syntax::Expression::When { input, arms } => {
                crate::lower::UnresolvedExpression::When {
                    input: convert_expression(input.unboxed()).boxed(),
                    arms: arms
                        .into_iter()
                        .map(|arm| {
                            arm.map(|arm| crate::lower::UnresolvedArm {
                                pattern: convert_pattern(arm.pattern),
                                body: convert_expression(arm.body),
                            })
                        })
                        .collect(),
                }
            }
            crate::syntax::Expression::Intrinsic { name, inputs } => {
                crate::lower::UnresolvedExpression::Intrinsic {
                    name,
                    inputs: inputs.into_iter().map(convert_expression).collect(),
                }
            }
            crate::syntax::Expression::Tuple(elements) => {
                crate::lower::UnresolvedExpression::Tuple(
                    elements.into_iter().map(convert_expression).collect(),
                )
            }
            crate::syntax::Expression::Collection(elements) => {
                crate::lower::UnresolvedExpression::Collection(
                    elements.into_iter().map(convert_expression).collect(),
                )
            }
            crate::syntax::Expression::Structure(fields) => {
                crate::lower::UnresolvedExpression::Structure(
                    fields
                        .into_iter()
                        .map(|field| {
                            field.map(|field| crate::lower::UnresolvedFieldValue {
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
        pattern: crate::util::WithInfo<crate::syntax::Pattern>,
    ) -> crate::util::WithInfo<crate::lower::UnresolvedPattern> {
        pattern.map(|pattern| match pattern {
            crate::syntax::Pattern::Error => crate::lower::UnresolvedPattern::Error,
            crate::syntax::Pattern::Name(name) => crate::lower::UnresolvedPattern::Name(name),
            crate::syntax::Pattern::Number(number) => {
                crate::lower::UnresolvedPattern::Number(number)
            }
            crate::syntax::Pattern::Text(text) => crate::lower::UnresolvedPattern::Text(text),
            crate::syntax::Pattern::Wildcard => crate::lower::UnresolvedPattern::Wildcard,
            crate::syntax::Pattern::VariantOrName(name) => {
                crate::lower::UnresolvedPattern::VariantOrName(name.item)
            }
            crate::syntax::Pattern::Destructure(fields) => {
                crate::lower::UnresolvedPattern::Destructure(
                    fields
                        .into_iter()
                        .map(|field| {
                            field.map(|field| crate::lower::UnresolvedFieldPattern {
                                name: field.name,
                                pattern: convert_pattern(field.pattern),
                            })
                        })
                        .collect(),
                )
            }
            crate::syntax::Pattern::Variant {
                variant,
                value_patterns,
            } => crate::lower::UnresolvedPattern::Variant {
                variant,
                value_patterns: value_patterns.into_iter().map(convert_pattern).collect(),
            },
            crate::syntax::Pattern::Tuple(elements) => crate::lower::UnresolvedPattern::Tuple(
                elements.into_iter().map(convert_pattern).collect(),
            ),
            crate::syntax::Pattern::Or { left, right } => crate::lower::UnresolvedPattern::Or {
                left: convert_pattern(left.unboxed()).boxed(),
                right: convert_pattern(right.unboxed()).boxed(),
            },
            crate::syntax::Pattern::Mutate(name) => crate::lower::UnresolvedPattern::Mutate(name),
            crate::syntax::Pattern::Annotate { pattern, r#type } => {
                crate::lower::UnresolvedPattern::Annotate {
                    pattern: convert_pattern(pattern.unboxed()).boxed(),
                    r#type: convert_type(r#type),
                }
            }
        })
    }

    fn convert_binary_operator(
        binary_operator: crate::util::WithInfo<crate::syntax::BinaryOperator>,
    ) -> crate::util::WithInfo<crate::lower::UnresolvedBinaryOperator> {
        binary_operator.map(|binary_operator| match binary_operator {
            crate::syntax::BinaryOperator::LessThan => {
                crate::lower::UnresolvedBinaryOperator::LessThan
            }
            crate::syntax::BinaryOperator::GreaterThan => {
                crate::lower::UnresolvedBinaryOperator::GreaterThan
            }
            crate::syntax::BinaryOperator::LessThanOrEqual => {
                crate::lower::UnresolvedBinaryOperator::LessThanOrEqual
            }
            crate::syntax::BinaryOperator::GreaterThanOrEqual => {
                crate::lower::UnresolvedBinaryOperator::GreaterThanOrEqual
            }
            crate::syntax::BinaryOperator::Equal => crate::lower::UnresolvedBinaryOperator::Equal,
            crate::syntax::BinaryOperator::NotEqual => {
                crate::lower::UnresolvedBinaryOperator::NotEqual
            }
            crate::syntax::BinaryOperator::Add => crate::lower::UnresolvedBinaryOperator::Add,
            crate::syntax::BinaryOperator::Subtract => {
                crate::lower::UnresolvedBinaryOperator::Subtract
            }
            crate::syntax::BinaryOperator::Multiply => {
                crate::lower::UnresolvedBinaryOperator::Multiply
            }
            crate::syntax::BinaryOperator::Divide => crate::lower::UnresolvedBinaryOperator::Divide,
            crate::syntax::BinaryOperator::Remainder => {
                crate::lower::UnresolvedBinaryOperator::Remainder
            }
            crate::syntax::BinaryOperator::Power => crate::lower::UnresolvedBinaryOperator::Power,
            crate::syntax::BinaryOperator::And => crate::lower::UnresolvedBinaryOperator::And,
            crate::syntax::BinaryOperator::Or => crate::lower::UnresolvedBinaryOperator::Or,
            crate::syntax::BinaryOperator::To => crate::lower::UnresolvedBinaryOperator::To,
            crate::syntax::BinaryOperator::By => crate::lower::UnresolvedBinaryOperator::By,
        })
    }
}

pub mod typecheck {
    pub fn convert_attribute(
        attribute: crate::util::WithInfo<crate::lower::Attribute>,
    ) -> crate::util::WithInfo<crate::typecheck::Attribute> {
        attribute.map(|attribute| match attribute {
            crate::lower::Attribute::Error => crate::typecheck::Attribute::Error,
            crate::lower::Attribute::Name(name) => crate::typecheck::Attribute::Name(name),
            crate::lower::Attribute::Valued { name, value } => {
                crate::typecheck::Attribute::Valued {
                    name,
                    value: convert_attribute_value(value),
                }
            }
        })
    }

    pub fn convert_attribute_value(
        attribute_value: crate::util::WithInfo<crate::lower::AttributeValue>,
    ) -> crate::util::WithInfo<crate::typecheck::AttributeValue> {
        attribute_value.map(|attribute_value| match attribute_value {
            crate::lower::AttributeValue::Error => crate::typecheck::AttributeValue::Error,
            crate::lower::AttributeValue::Name(name) => {
                crate::typecheck::AttributeValue::Name(name)
            }
            crate::lower::AttributeValue::Number(number) => {
                crate::typecheck::AttributeValue::Number(number)
            }
            crate::lower::AttributeValue::Text(text) => {
                crate::typecheck::AttributeValue::Text(text)
            }
        })
    }

    pub fn convert_syntax_declaration(
        syntax_declaration: crate::util::WithInfo<crate::lower::SyntaxDeclaration>,
    ) -> crate::util::WithInfo<crate::typecheck::SyntaxDeclaration> {
        syntax_declaration.map(|syntax_declaration| crate::typecheck::SyntaxDeclaration {
            attributes: syntax_declaration
                .attributes
                .into_iter()
                .map(convert_attribute)
                .collect(),
        })
    }

    pub fn convert_type_declaration(
        type_declaration: crate::util::WithInfo<crate::lower::TypeDeclaration>,
    ) -> crate::util::WithInfo<crate::typecheck::TypeDeclaration> {
        type_declaration.map(|type_declaration| crate::typecheck::TypeDeclaration {
            attributes: type_declaration
                .attributes
                .into_iter()
                .map(convert_attribute)
                .collect(),
            parameters: type_declaration.parameters,
            representation: type_declaration.representation.map(|type_representation| {
                match type_representation {
                    crate::lower::TypeRepresentation::Marker => {
                        crate::typecheck::TypeRepresentation::Marker
                    }
                    crate::lower::TypeRepresentation::Structure(fields) => {
                        crate::typecheck::TypeRepresentation::Structure(
                            fields
                                .into_iter()
                                .map(|field| {
                                    (
                                        field.item.name.item,
                                        crate::util::WithInfo {
                                            info: field.info,
                                            item: crate::typecheck::StructureField {
                                                index: field.item.index,
                                                attributes: field
                                                    .item
                                                    .attributes
                                                    .into_iter()
                                                    .map(convert_attribute)
                                                    .collect(),
                                                r#type: convert_type(field.item.r#type),
                                            },
                                        },
                                    )
                                })
                                .collect(),
                        )
                    }
                    crate::lower::TypeRepresentation::Enumeration(variants) => {
                        crate::typecheck::TypeRepresentation::Enumeration(
                            variants
                                .into_iter()
                                .map(|variant| {
                                    (
                                        variant.item.name.item,
                                        crate::util::WithInfo {
                                            info: variant.info,
                                            item: crate::typecheck::EnumerationVariant {
                                                index: variant.item.index,
                                                attributes: variant
                                                    .item
                                                    .attributes
                                                    .into_iter()
                                                    .map(convert_attribute)
                                                    .collect(),
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
                    crate::lower::TypeRepresentation::Wrapper(r#type) => {
                        crate::typecheck::TypeRepresentation::Wrapper(convert_type(r#type))
                    }
                }
            }),
        })
    }

    pub fn convert_trait_declaration(
        trait_declaration: crate::util::WithInfo<crate::lower::TraitDeclaration>,
    ) -> crate::util::WithInfo<crate::typecheck::TraitDeclaration> {
        trait_declaration.map(|trait_declaration| crate::typecheck::TraitDeclaration {
            attributes: trait_declaration
                .attributes
                .into_iter()
                .map(convert_attribute)
                .collect(),
            parameters: trait_declaration.parameters,
            r#type: trait_declaration.r#type.map(convert_type),
        })
    }

    pub fn convert_type_parameter_declaration(
        type_parameter_declaration: crate::util::WithInfo<crate::lower::TypeParameterDeclaration>,
    ) -> crate::util::WithInfo<crate::typecheck::TypeParameterDeclaration> {
        type_parameter_declaration.map(|type_parameter_declaration| {
            crate::typecheck::TypeParameterDeclaration {
                infer: type_parameter_declaration.infer,
                default: type_parameter_declaration.default.map(convert_type),
            }
        })
    }

    pub fn convert_constant_declaration(
        constant_declaration: crate::util::WithInfo<crate::lower::ConstantDeclaration>,
    ) -> crate::util::WithInfo<crate::typecheck::ConstantDeclaration> {
        constant_declaration.map(|constant_declaration| {
            let r#type = convert_type(constant_declaration.r#type);

            crate::typecheck::ConstantDeclaration {
                attributes: constant_declaration
                    .attributes
                    .into_iter()
                    .map(convert_attribute)
                    .collect(),
                parameters: constant_declaration.parameters,
                bounds: constant_declaration
                    .bounds
                    .into_iter()
                    .map(convert_instance)
                    .collect(),
                r#type,
            }
        })
    }

    pub fn convert_instance_declaration(
        instance_declaration: crate::util::WithInfo<crate::lower::InstanceDeclaration>,
    ) -> crate::util::WithInfo<crate::typecheck::InstanceDeclaration> {
        instance_declaration.map(
            |instance_declaration| crate::typecheck::InstanceDeclaration {
                parameters: instance_declaration.parameters,
                bounds: instance_declaration
                    .bounds
                    .into_iter()
                    .map(convert_instance)
                    .collect(),
                instance: convert_instance(instance_declaration.instance),
                default: instance_declaration.default,
            },
        )
    }

    pub fn convert_type(
        r#type: crate::util::WithInfo<crate::lower::Type>,
    ) -> crate::util::WithInfo<crate::typecheck::Type> {
        r#type.map(|r#type| match r#type {
            crate::lower::Type::Error => crate::typecheck::Type::Unknown,
            crate::lower::Type::Placeholder => crate::typecheck::Type::Unknown,
            crate::lower::Type::Declared { path, parameters } => crate::typecheck::Type::Declared {
                path: path.item,
                parameters: parameters.into_iter().map(convert_type).collect(),
            },
            crate::lower::Type::Parameter(parameter) => {
                crate::typecheck::Type::Parameter(parameter)
            }
            crate::lower::Type::Function { inputs, output } => crate::typecheck::Type::Function {
                inputs: inputs.into_iter().map(convert_type).collect(),
                output: convert_type(output.unboxed()).boxed(),
            },
            crate::lower::Type::Tuple(elements) => {
                crate::typecheck::Type::Tuple(elements.into_iter().map(convert_type).collect())
            }
            crate::lower::Type::Block(r#type) => {
                crate::typecheck::Type::Block(convert_type(r#type.unboxed()).boxed())
            }
            crate::lower::Type::Intrinsic => crate::typecheck::Type::Intrinsic,
            crate::lower::Type::Message { segments, trailing } => crate::typecheck::Type::Message {
                segments: segments
                    .into_iter()
                    .map(|segment| crate::typecheck::MessageTypeFormatSegment {
                        text: segment.text,
                        r#type: convert_type(segment.value),
                    })
                    .collect(),
                trailing,
            },
            crate::lower::Type::Equal { left, right } => crate::typecheck::Type::Equal {
                left: convert_type(left.unboxed()).boxed(),
                right: convert_type(right.unboxed()).boxed(),
            },
        })
    }

    pub fn convert_instance(
        instance: crate::util::WithInfo<crate::lower::Instance>,
    ) -> crate::util::WithInfo<crate::typecheck::Instance> {
        instance.map(|instance| crate::typecheck::Instance {
            r#trait: instance.r#trait.item,
            parameters: instance.parameters.into_iter().map(convert_type).collect(),
        })
    }

    pub fn convert_item(item: crate::lower::Item) -> crate::typecheck::UntypedItem {
        crate::typecheck::UntypedItem {
            body: convert_expression(item.body),
            captures: item.captures,
        }
    }

    pub fn convert_top_level_code(
        top_level_code: crate::lower::TopLevelCode,
    ) -> crate::typecheck::UntypedTopLevelCode {
        crate::typecheck::UntypedTopLevelCode {
            statements: top_level_code
                .statements
                .into_iter()
                .map(convert_expression)
                .collect(),
        }
    }

    pub fn convert_expression(
        expression: crate::util::WithInfo<crate::lower::Expression>,
    ) -> crate::util::WithInfo<crate::typecheck::UntypedExpression> {
        expression.map(|expression| match expression {
            crate::lower::Expression::Error => crate::typecheck::UntypedExpression::Unknown,
            crate::lower::Expression::Assign { pattern, value } => {
                crate::typecheck::UntypedExpression::Initialize {
                    pattern: convert_pattern(pattern),
                    value: convert_expression(value.unboxed()).boxed(),
                }
            }
            crate::lower::Expression::Mutate { name, path, value } => {
                crate::typecheck::UntypedExpression::Mutate {
                    name: name.item,
                    path,
                    value: convert_expression(value.unboxed()).boxed(),
                }
            }
            crate::lower::Expression::Annotate { value, r#type } => {
                crate::typecheck::UntypedExpression::Annotate {
                    value: convert_expression(value.unboxed()).boxed(),
                    r#type: convert_type(r#type),
                }
            }
            crate::lower::Expression::Variable(name, variable) => {
                crate::typecheck::UntypedExpression::Variable(name, variable)
            }
            crate::lower::Expression::Number(number) => {
                crate::typecheck::UntypedExpression::Number(number)
            }
            crate::lower::Expression::Text(text) => crate::typecheck::UntypedExpression::Text(text),
            crate::lower::Expression::Constant(path) => {
                crate::typecheck::UntypedExpression::Constant(path)
            }
            crate::lower::Expression::Trait(path) => {
                crate::typecheck::UntypedExpression::Trait(path)
            }
            crate::lower::Expression::Format { segments, trailing } => {
                crate::typecheck::UntypedExpression::Format {
                    segments: segments
                        .into_iter()
                        .map(|segment| crate::typecheck::UntypedFormatSegment {
                            text: segment.text,
                            value: convert_expression(segment.value),
                        })
                        .collect(),
                    trailing,
                }
            }
            crate::lower::Expression::Block {
                statements,
                captures,
            } => crate::typecheck::UntypedExpression::Block {
                statements: statements.into_iter().map(convert_expression).collect(),
                top_level: false,
                captures,
            },
            crate::lower::Expression::Do(block) => {
                crate::typecheck::UntypedExpression::Do(convert_expression(block.unboxed()).boxed())
            }
            crate::lower::Expression::Function {
                inputs,
                body,
                captures,
            } => crate::typecheck::UntypedExpression::Function {
                inputs: inputs.into_iter().map(convert_pattern).collect(),
                body: convert_expression(body.unboxed()).boxed(),
                captures,
            },
            crate::lower::Expression::Call { function, inputs } => {
                crate::typecheck::UntypedExpression::Call {
                    function: convert_expression(function.unboxed()).boxed(),
                    inputs: inputs.into_iter().map(convert_expression).collect(),
                }
            }
            crate::lower::Expression::When { input, arms } => {
                crate::typecheck::UntypedExpression::When {
                    input: convert_expression(input.unboxed()).boxed(),
                    arms: arms
                        .into_iter()
                        .map(|arm| {
                            arm.map(|arm| crate::typecheck::UntypedArm {
                                pattern: convert_pattern(arm.pattern),
                                body: convert_expression(arm.body),
                            })
                        })
                        .collect(),
                }
            }
            crate::lower::Expression::Intrinsic { name, inputs } => {
                crate::typecheck::UntypedExpression::Intrinsic {
                    name: name.item,
                    inputs: inputs.into_iter().map(convert_expression).collect(),
                }
            }
            crate::lower::Expression::Tuple(elements) => {
                crate::typecheck::UntypedExpression::Tuple(
                    elements.into_iter().map(convert_expression).collect(),
                )
            }
            crate::lower::Expression::Collection(elements) => {
                crate::typecheck::UntypedExpression::Collection(
                    elements.into_iter().map(convert_expression).collect(),
                )
            }
            crate::lower::Expression::Marker(path) => {
                crate::typecheck::UntypedExpression::Marker(path)
            }
            crate::lower::Expression::Structure(elements) => {
                crate::typecheck::UntypedExpression::Structure(
                    elements
                        .into_iter()
                        .filter_map(|element| {
                            element.filter_map(|element| {
                                Some(crate::typecheck::UntypedStructureFieldValue {
                                    name: element.name.item?,
                                    value: convert_expression(element.value),
                                })
                            })
                        })
                        .collect(),
                )
            }
            crate::lower::Expression::Variant { variant, values } => {
                crate::typecheck::UntypedExpression::Variant {
                    variant,
                    values: values.into_iter().map(convert_expression).collect(),
                }
            }
            crate::lower::Expression::Wrapper { r#type, value } => {
                crate::typecheck::UntypedExpression::Wrapper {
                    r#type,
                    value: convert_expression(value.unboxed()).boxed(),
                }
            }
        })
    }

    pub fn convert_pattern(
        pattern: crate::util::WithInfo<crate::lower::Pattern>,
    ) -> crate::util::WithInfo<crate::typecheck::Pattern> {
        pattern.map(|pattern| match pattern {
            crate::lower::Pattern::Error => crate::typecheck::Pattern::Unknown,
            crate::lower::Pattern::Wildcard => crate::typecheck::Pattern::Wildcard,
            crate::lower::Pattern::Number(number) => crate::typecheck::Pattern::Number(number),
            crate::lower::Pattern::Text(text) => crate::typecheck::Pattern::Text(text),
            crate::lower::Pattern::Variable(name, variable) => {
                crate::typecheck::Pattern::Variable(name, variable)
            }
            crate::lower::Pattern::Destructure(fields) => crate::typecheck::Pattern::Destructure {
                structure: None, // will be inferred during typechecking
                field_patterns: fields
                    .into_iter()
                    .filter_map(|field| {
                        field.filter_map(|field| {
                            Some(crate::typecheck::FieldPattern {
                                name: field.name.item?,
                                pattern: convert_pattern(field.pattern),
                            })
                        })
                    })
                    .collect(),
            },
            crate::lower::Pattern::Variant {
                variant,
                value_patterns,
            } => crate::typecheck::Pattern::Variant {
                variant,
                value_patterns: value_patterns.into_iter().map(convert_pattern).collect(),
            },
            crate::lower::Pattern::Marker(path) => crate::typecheck::Pattern::Marker(path),
            crate::lower::Pattern::Wrapper {
                path,
                value_pattern,
            } => crate::typecheck::Pattern::Wrapper {
                path,
                value_pattern: convert_pattern(value_pattern.unboxed()).boxed(),
            },
            crate::lower::Pattern::Tuple(elements) => crate::typecheck::Pattern::Tuple(
                elements.into_iter().map(convert_pattern).collect(),
            ),
            crate::lower::Pattern::Or { left, right } => crate::typecheck::Pattern::Or {
                left: convert_pattern(left.unboxed()).boxed(),
                right: convert_pattern(right.unboxed()).boxed(),
            },
            crate::lower::Pattern::Annotate { pattern, r#type } => {
                crate::typecheck::Pattern::Annotate {
                    pattern: convert_pattern(pattern.unboxed()).boxed(),
                    r#type: convert_type(r#type),
                }
            }
        })
    }
}
