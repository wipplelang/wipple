use std::{env, fs};
use wipple_parser::reader::ReadOptions;
use wipple_util::WithInfo;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let code = fs::read_to_string(&args[1]).expect("failed to open file");

    let options = ReadOptions {
        strip_comments: true,
    };

    let tokenize_result = wipple_parser::reader::tokenize(&code);
    dbg!(&tokenize_result.errors);

    let read_result = wipple_parser::reader::read(tokenize_result.tokens, options);
    dbg!(&read_result.errors);

    eprintln!("{}", read_result.top_level);
    dbg!(&read_result.errors);

    let syntax_result =
        wipple_parser::syntax::parse(&wipple_parser::syntax::Driver, read_result.top_level);
    dbg!(&syntax_result);

    let parse_result =
        wipple_syntax::parse(&wipple_parser::syntax::Driver, syntax_result.top_level);
    dbg!(&parse_result);

    let lower_driver = lower::Driver;

    let lower_result = wipple_lower::resolve(
        &lower_driver,
        lower::convert(parse_result.top_level),
        Vec::new(),
    );

    dbg!(&lower_result);

    let typecheck_driver = typecheck::Driver {
        module: lower_result.module,
    };

    for typecheck_result in std::iter::once(wipple_typecheck::resolve(
        &typecheck_driver,
        WithInfo {
            info: typecheck::Info { span: 0..0 },
            item: typecheck_driver
                .module
                .code
                .iter()
                .cloned()
                .map(typecheck::convert_expression)
                .collect::<Vec<_>>(),
        },
    ))
    .chain(
        typecheck_driver
            .module
            .constant_declarations
            .values()
            .cloned()
            .map(|constant_declaration| {
                wipple_typecheck::resolve(
                    &typecheck_driver,
                    typecheck::convert_constant_declaration(constant_declaration),
                )
            }),
    )
    .chain(
        typecheck_driver
            .module
            .instance_declarations
            .iter()
            .cloned()
            .map(|instance_declaration| {
                wipple_typecheck::resolve(
                    &typecheck_driver,
                    typecheck::convert_instance_declaration(instance_declaration),
                )
            }),
    ) {
        dbg!(&typecheck_result);
    }
}

// TODO: Move all of this to wipple-driver

const RECURSION_LIMIT: u32 = 64;

mod lower {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct Driver;

    impl wipple_lower::Driver for Driver {
        type Info = wipple_parser::syntax::Info;
        type Number = String;
    }

    pub type Info = <Driver as wipple_lower::Driver>::Info;

    type SyntaxDriver = wipple_parser::syntax::Driver;
    type SyntaxInfo = <SyntaxDriver as wipple_syntax::Driver>::Info;

    pub fn convert(
        top_level: WithInfo<SyntaxInfo, wipple_syntax::TopLevel<SyntaxDriver>>,
    ) -> WithInfo<Info, wipple_lower::UnresolvedFile<Driver>> {
        top_level.map(|top_level| wipple_lower::UnresolvedFile {
            statements: top_level
                .statements
                .into_iter()
                .map(convert_statement)
                .collect(),
        })
    }

    fn convert_statement(
        statement: WithInfo<SyntaxInfo, wipple_syntax::Statement<SyntaxDriver>>,
    ) -> WithInfo<Info, wipple_lower::UnresolvedStatement<Driver>> {
        statement.map(|statement| match statement {
            wipple_syntax::Statement::Type {
                name,
                parameters,
                representation,
            } => wipple_lower::UnresolvedStatement::Type {
                name,
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
            wipple_syntax::Statement::Language { name, item } => {
                wipple_lower::UnresolvedStatement::Language { name, item }
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
        type_parameter: WithInfo<SyntaxInfo, wipple_syntax::TypeParameter<SyntaxDriver>>,
    ) -> WithInfo<Info, wipple_lower::UnresolvedTypeParameter<Driver>> {
        type_parameter.map(|type_parameter| wipple_lower::UnresolvedTypeParameter {
            name: type_parameter.name,
            infer: type_parameter.infer,
            default: type_parameter.default.map(convert_type),
        })
    }

    fn convert_type_representation(
        type_representation: WithInfo<SyntaxInfo, wipple_syntax::TypeRepresentation<SyntaxDriver>>,
    ) -> WithInfo<Info, wipple_lower::UnresolvedTypeRepresentation<Driver>> {
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
        })
    }

    fn convert_type(
        r#type: WithInfo<SyntaxInfo, wipple_syntax::Type<SyntaxDriver>>,
    ) -> WithInfo<Info, wipple_lower::UnresolvedType<Driver>> {
        r#type.map(|r#type| match r#type {
            wipple_syntax::Type::Error => wipple_lower::UnresolvedType::Error,
            wipple_syntax::Type::Placeholder => wipple_lower::UnresolvedType::Placeholder,
            wipple_syntax::Type::Declared { name, parameters } => {
                wipple_lower::UnresolvedType::Declared {
                    name,
                    parameters: parameters.into_iter().map(convert_type).collect(),
                }
            }
            wipple_syntax::Type::Function { input, output } => {
                wipple_lower::UnresolvedType::Function {
                    input: convert_type(input.unboxed()).boxed(),
                    output: convert_type(output.unboxed()).boxed(),
                }
            }
            wipple_syntax::Type::Tuple(elements) => wipple_lower::UnresolvedType::Tuple(
                elements.into_iter().map(convert_type).collect(),
            ),
            wipple_syntax::Type::Lazy(r#type) => {
                wipple_lower::UnresolvedType::Lazy(convert_type(r#type.unboxed()).boxed())
            }
        })
    }

    fn convert_instance(
        instance: WithInfo<SyntaxInfo, wipple_syntax::Instance<SyntaxDriver>>,
    ) -> WithInfo<Info, wipple_lower::UnresolvedInstance<Driver>> {
        instance.map(|instance| wipple_lower::UnresolvedInstance {
            r#trait: instance.r#trait,
            parameters: instance.parameters.into_iter().map(convert_type).collect(),
        })
    }

    fn convert_expression(
        expression: WithInfo<SyntaxInfo, wipple_syntax::Expression<SyntaxDriver>>,
    ) -> WithInfo<Info, wipple_lower::UnresolvedExpression<Driver>> {
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
            wipple_syntax::Expression::Function { pattern, body } => {
                wipple_lower::UnresolvedExpression::Function {
                    pattern: convert_pattern(pattern),
                    body: convert_expression(body.unboxed()).boxed(),
                }
            }
            wipple_syntax::Expression::Call { function, input } => {
                wipple_lower::UnresolvedExpression::Call {
                    function: convert_expression(function.unboxed()).boxed(),
                    input: convert_expression(input.unboxed()).boxed(),
                }
            }
            wipple_syntax::Expression::Apply { input, function } => {
                wipple_lower::UnresolvedExpression::Apply {
                    input: input.map(|input| convert_expression(input.unboxed()).boxed()),
                    function: function
                        .map(|function| convert_expression(function.unboxed()).boxed()),
                }
            }
            wipple_syntax::Expression::BinaryOperator {
                operator,
                left,
                right,
            } => wipple_lower::UnresolvedExpression::BinaryOperator {
                operator: convert_binary_operator(operator),
                left: left.map(|left| convert_expression(left.unboxed()).boxed()),
                right: right.map(|right| convert_expression(right.unboxed()).boxed()),
            },
            wipple_syntax::Expression::As { value, r#type } => {
                wipple_lower::UnresolvedExpression::As {
                    value: value.map(|value| convert_expression(value.unboxed()).boxed()),
                    r#type: convert_type(r#type),
                }
            }
            wipple_syntax::Expression::Is { value, pattern } => {
                wipple_lower::UnresolvedExpression::Is {
                    value: value.map(|value| convert_expression(value.unboxed()).boxed()),
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
                                condition: arm.condition.map(convert_expression),
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
            wipple_syntax::Expression::Semantics { name, body } => {
                wipple_lower::UnresolvedExpression::Semantics {
                    name,
                    body: convert_expression(body.unboxed()).boxed(),
                }
            }
        })
    }

    fn convert_pattern(
        pattern: WithInfo<SyntaxInfo, wipple_syntax::Pattern<SyntaxDriver>>,
    ) -> WithInfo<Info, wipple_lower::UnresolvedPattern<Driver>> {
        pattern.map(|pattern| match pattern {
            wipple_syntax::Pattern::Error => wipple_lower::UnresolvedPattern::Error,
            wipple_syntax::Pattern::Name(name) => wipple_lower::UnresolvedPattern::Name(name),
            wipple_syntax::Pattern::Number(number) => {
                wipple_lower::UnresolvedPattern::Number(number)
            }
            wipple_syntax::Pattern::Text(text) => wipple_lower::UnresolvedPattern::Text(text),
            wipple_syntax::Pattern::Wildcard => wipple_lower::UnresolvedPattern::Wildcard,
            wipple_syntax::Pattern::VariantOrName(name) => {
                wipple_lower::UnresolvedPattern::VariantOrName(name)
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
        })
    }

    fn convert_binary_operator(
        binary_operator: WithInfo<SyntaxInfo, wipple_syntax::BinaryOperator>,
    ) -> WithInfo<Info, wipple_lower::UnresolvedBinaryOperator> {
        binary_operator.map(|binary_operator| match binary_operator {
            wipple_syntax::BinaryOperator::Compose => {
                wipple_lower::UnresolvedBinaryOperator::Compose
            }
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
        })
    }
}

mod typecheck {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct Driver {
        pub module: wipple_lower::Module<lower::Driver>,
    }

    impl wipple_typecheck::Driver for Driver {
        type Info = wipple_parser::syntax::Info;
        type Number = String;
        type Path = wipple_lower::Path;

        fn recursion_limit(&self) -> u32 {
            RECURSION_LIMIT
        }

        fn top_level_info(&self) -> Self::Info {
            wipple_parser::syntax::Info { span: 0..0 }
        }

        fn path_for_language_type(&self, language_item: &'static str) -> Option<Self::Path> {
            Some(
                self.module
                    .language_declarations
                    .get(language_item)?
                    .item
                    .clone(),
            )
        }

        fn path_for_language_trait(&self, language_item: &'static str) -> Option<Self::Path> {
            Some(
                self.module
                    .language_declarations
                    .get(language_item)?
                    .item
                    .clone(),
            )
        }

        fn paths_are_equal(&self, left: &Self::Path, right: &Self::Path) -> bool {
            left == right
        }

        fn get_type_declaration(
            &self,
            path: &Self::Path,
        ) -> WithInfo<Self::Info, wipple_typecheck::TypeDeclaration<Self>> {
            convert_type_declaration(
                self.module
                    .type_declarations
                    .get(path)
                    .expect("missing type declaration")
                    .clone(),
            )
        }

        fn get_trait_declaration(
            &self,
            path: &Self::Path,
        ) -> WithInfo<Self::Info, wipple_typecheck::TraitDeclaration<Self>> {
            convert_trait_declaration(
                self.module
                    .trait_declarations
                    .get(path)
                    .expect("missing trait declaration")
                    .clone(),
            )
        }

        fn get_type_parameter_declaration(
            &self,
            path: &Self::Path,
        ) -> WithInfo<Self::Info, wipple_typecheck::TypeParameterDeclaration<Self>> {
            convert_type_parameter_declaration(
                self.module
                    .type_parameter_declarations
                    .get(path)
                    .expect("missing type parameter declaration")
                    .clone(),
            )
        }

        fn get_constant_declaration(
            &self,
            path: &Self::Path,
        ) -> WithInfo<Self::Info, wipple_typecheck::ConstantDeclaration<Self>> {
            convert_constant_declaration(
                self.module
                    .constant_declarations
                    .get(path)
                    .expect("missing constant declaration")
                    .clone(),
            )
        }

        fn get_instances_for_trait(
            &self,
            path: &Self::Path,
        ) -> Vec<WithInfo<Self::Info, wipple_typecheck::InstanceDeclaration<Self>>> {
            self.module
                .instance_declarations
                .iter()
                .filter(|instance| instance.item.instance.item.r#trait.item == *path)
                .cloned()
                .map(convert_instance_declaration)
                .collect()
        }

        fn get_enumeration_for_variant(&self, variant: &Self::Path) -> Self::Path {
            // The parent of a variant is its enumeration
            variant[0..variant.len() - 1].to_vec()
        }
    }

    pub type Info = <Driver as wipple_typecheck::Driver>::Info;

    pub fn convert_type_declaration(
        type_declaration: WithInfo<lower::Info, wipple_lower::TypeDeclaration<lower::Driver>>,
    ) -> WithInfo<Info, wipple_typecheck::TypeDeclaration<Driver>> {
        type_declaration.map(|type_declaration| wipple_typecheck::TypeDeclaration {
            parameters: type_declaration
                .parameters
                .into_iter()
                .map(|parameter| parameter.item)
                .collect(),
            representation: match type_declaration.representation.item {
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
                                    WithInfo {
                                        info: field.info,
                                        item: wipple_typecheck::StructureField {
                                            r#type: convert_type(field.item.r#type).item,
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
                                    WithInfo {
                                        info: variant.info,
                                        item: wipple_typecheck::EnumerationVariant {
                                            value_types: variant
                                                .item
                                                .types
                                                .into_iter()
                                                .map(|r#type| convert_type(r#type).item)
                                                .collect(),
                                        },
                                    },
                                )
                            })
                            .collect(),
                    )
                }
            },
        })
    }

    pub fn convert_trait_declaration(
        trait_declaration: WithInfo<lower::Info, wipple_lower::TraitDeclaration<lower::Driver>>,
    ) -> WithInfo<Info, wipple_typecheck::TraitDeclaration<Driver>> {
        trait_declaration.map(|trait_declaration| wipple_typecheck::TraitDeclaration {
            parameters: trait_declaration
                .parameters
                .into_iter()
                .map(|parameter| parameter.item)
                .collect(),
            r#type: convert_type(trait_declaration.r#type).item,
        })
    }

    pub fn convert_type_parameter_declaration(
        type_parameter_declaration: WithInfo<
            lower::Info,
            wipple_lower::TypeParameterDeclaration<lower::Driver>,
        >,
    ) -> WithInfo<Info, wipple_typecheck::TypeParameterDeclaration<Driver>> {
        type_parameter_declaration.map(|type_parameter_declaration| {
            wipple_typecheck::TypeParameterDeclaration {
                infer: type_parameter_declaration.infer.is_some(),
                default: type_parameter_declaration
                    .default
                    .map(|r#type| convert_type(r#type).item),
            }
        })
    }

    pub fn convert_constant_declaration(
        constant_declaration: WithInfo<
            lower::Info,
            wipple_lower::ConstantDeclaration<lower::Driver>,
        >,
    ) -> WithInfo<Info, wipple_typecheck::ConstantDeclaration<Driver>> {
        constant_declaration.map(
            |constant_declaration| wipple_typecheck::ConstantDeclaration {
                parameters: constant_declaration
                    .parameters
                    .into_iter()
                    .map(|parameter| parameter.item)
                    .collect(),
                bounds: constant_declaration
                    .bounds
                    .into_iter()
                    .map(convert_instance)
                    .collect(),
                r#type: convert_type(constant_declaration.r#type),
                body: convert_expression(constant_declaration.body),
            },
        )
    }

    pub fn convert_instance_declaration(
        instance_declaration: WithInfo<
            lower::Info,
            wipple_lower::InstanceDeclaration<lower::Driver>,
        >,
    ) -> WithInfo<Info, wipple_typecheck::InstanceDeclaration<Driver>> {
        instance_declaration.map(
            |instance_declaration| wipple_typecheck::InstanceDeclaration {
                parameters: instance_declaration
                    .parameters
                    .into_iter()
                    .map(|parameter| parameter.item)
                    .collect(),
                bounds: instance_declaration
                    .bounds
                    .into_iter()
                    .map(convert_instance)
                    .collect(),
                instance: convert_instance(instance_declaration.instance),
                body: convert_expression(instance_declaration.body),
            },
        )
    }

    pub fn convert_type(
        r#type: WithInfo<lower::Info, wipple_lower::Type<lower::Driver>>,
    ) -> WithInfo<Info, wipple_typecheck::Type<Driver>> {
        r#type.map(|r#type| match r#type {
            wipple_lower::Type::Error => wipple_typecheck::Type::Unknown,
            wipple_lower::Type::Placeholder => todo!("implicit type parameters"),
            wipple_lower::Type::Declared { path, parameters } => wipple_typecheck::Type::Declared {
                path: path.item,
                parameters: parameters
                    .into_iter()
                    .map(|r#type| convert_type(r#type).item)
                    .collect(),
            },
            wipple_lower::Type::Function { input, output } => wipple_typecheck::Type::Function {
                input: convert_type(input.unboxed()).boxed().item,
                output: convert_type(output.unboxed()).boxed().item,
            },
            wipple_lower::Type::Tuple(elements) => wipple_typecheck::Type::Tuple(
                elements
                    .into_iter()
                    .map(|r#type| convert_type(r#type).item)
                    .collect(),
            ),
            wipple_lower::Type::Lazy(r#type) => {
                wipple_typecheck::Type::Lazy(convert_type(r#type.unboxed()).boxed().item)
            }
        })
    }

    pub fn convert_instance(
        instance: WithInfo<lower::Info, wipple_lower::Instance<lower::Driver>>,
    ) -> WithInfo<Info, wipple_typecheck::Instance<Driver>> {
        instance.map(|instance| wipple_typecheck::Instance {
            r#trait: instance.r#trait.item,
            parameters: instance
                .parameters
                .into_iter()
                .map(|r#type| convert_type(r#type).item)
                .collect(),
        })
    }

    pub fn convert_expression(
        expression: WithInfo<lower::Info, wipple_lower::Expression<lower::Driver>>,
    ) -> WithInfo<Info, wipple_typecheck::UntypedExpression<Driver>> {
        expression.map(|expression| match expression {
            wipple_lower::Expression::Error => wipple_typecheck::UntypedExpression::Unknown,
            wipple_lower::Expression::Assign { pattern, value } => {
                wipple_typecheck::UntypedExpression::Initialize {
                    pattern: convert_pattern(pattern),
                    value: convert_expression(value.unboxed()).boxed(),
                }
            }
            wipple_lower::Expression::Annotate { value, r#type } => {
                wipple_typecheck::UntypedExpression::Annotate {
                    value: convert_expression(value.unboxed()).boxed(),
                    r#type: convert_type(r#type).item,
                }
            }
            wipple_lower::Expression::Variable(variable) => {
                wipple_typecheck::UntypedExpression::Variable(variable)
            }
            wipple_lower::Expression::Number(number) => {
                wipple_typecheck::UntypedExpression::Number(number)
            }
            wipple_lower::Expression::Text(text) => wipple_typecheck::UntypedExpression::Text(text),
            wipple_lower::Expression::Marker(path) => {
                wipple_typecheck::UntypedExpression::Marker(path)
            }
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
            wipple_lower::Expression::Function { pattern, body } => {
                wipple_typecheck::UntypedExpression::Function {
                    pattern: convert_pattern(pattern),
                    body: convert_expression(body.unboxed()).boxed(),
                }
            }
            wipple_lower::Expression::Call { function, input } => {
                wipple_typecheck::UntypedExpression::Call {
                    function: convert_expression(function.unboxed()).boxed(),
                    input: convert_expression(input.unboxed()).boxed(),
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
                                condition: arm.condition.map(convert_expression),
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
            wipple_lower::Expression::Structure(elements) => {
                wipple_typecheck::UntypedExpression::Structure(
                    elements
                        .into_iter()
                        .map(|element| {
                            element.map(|element| wipple_typecheck::UntypedStructureFieldValue {
                                name: element.name.item,
                                value: convert_expression(element.value),
                            })
                        })
                        .collect(),
                )
            }
            wipple_lower::Expression::Semantics { name, body } => {
                wipple_typecheck::UntypedExpression::Semantics {
                    name: name.item,
                    body: convert_expression(body.unboxed()).boxed(),
                }
            }
        })
    }

    pub fn convert_pattern(
        pattern: WithInfo<lower::Info, wipple_lower::Pattern<lower::Driver>>,
    ) -> WithInfo<Info, wipple_typecheck::Pattern<Driver>> {
        pattern.map(|pattern| match pattern {
            wipple_lower::Pattern::Error => wipple_typecheck::Pattern::Unknown,
            wipple_lower::Pattern::Wildcard => wipple_typecheck::Pattern::Wildcard,
            wipple_lower::Pattern::Number(number) => wipple_typecheck::Pattern::Number(number),
            wipple_lower::Pattern::Text(text) => wipple_typecheck::Pattern::Text(text),
            wipple_lower::Pattern::Variable(variable) => {
                wipple_typecheck::Pattern::Variable(variable)
            }
            wipple_lower::Pattern::Destructure(fields) => wipple_typecheck::Pattern::Destructure(
                fields
                    .into_iter()
                    .map(|field| {
                        field.map(|field| wipple_typecheck::FieldPattern {
                            name: field.name.item,
                            pattern: convert_pattern(field.pattern),
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
