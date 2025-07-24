use crate::{ir::Driver, lower::Path, util::WithInfo};
use itertools::Itertools;
use std::{collections::HashMap, mem};

pub fn compile<'a>(
    driver: &dyn Driver,
    path: Path,
    attributes: &'a [WithInfo<crate::typecheck::Attribute>],
    expression: WithInfo<&'a crate::typecheck::TypedExpression>,
    captures: &[Path],
) -> HashMap<Path, crate::ir::Item> {
    let mut info = Info {
        driver,
        path: path.clone(),
        items: Default::default(),
        context: Context::default(),
    };

    info.compile(path, captures, attributes, expression);

    info.items
}

struct Info<'a> {
    driver: &'a dyn Driver,
    path: Path,
    items: HashMap<Path, crate::ir::Item>,
    context: Context,
}

#[derive(Debug, Clone, Default)]
struct Context {
    next_block_id: u32,
    block_stack: Vec<(u32, Vec<crate::ir::Instruction>)>,
    variables: Vec<Path>,
}

impl Info<'_> {
    fn begin_block(&mut self) -> u32 {
        let block_id = self.context.next_block_id;
        self.context.next_block_id += 1;
        self.context.block_stack.push((block_id, Vec::new()));
        block_id
    }

    fn end_block(&mut self) {
        let (_, instructions) = self
            .context
            .block_stack
            .pop()
            .expect("block stack is empty");

        self.push_instruction(crate::ir::Instruction::Block(instructions));
    }

    fn current_block(&self) -> Option<u32> {
        let (id, _) = self.context.block_stack.last()?;
        Some(*id)
    }

    fn break_out_of_block(&mut self, id: u32) {
        let relative = self
            .context
            .block_stack
            .iter()
            .rev()
            .take_while(|&&(block_id, _)| block_id != id)
            .count() as u32;

        self.push_instruction(crate::ir::Instruction::Break(relative));
    }

    fn break_out_of_block_if_not(&mut self, variant: u32, id: u32) {
        let relative = self
            .context
            .block_stack
            .iter()
            .rev()
            .take_while(|&&(block_id, _)| block_id != id)
            .count() as u32;

        self.push_instruction(crate::ir::Instruction::BreakIfNot {
            variant,
            count: relative,
        });
    }

    fn push_instruction(&mut self, instruction: crate::ir::Instruction) {
        let instructions = self
            .context
            .block_stack
            .last_mut()
            .map(|(_, instructions)| instructions)
            .unwrap_or_else(|| &mut self.items.get_mut(&self.path).unwrap().instructions);

        instructions.push(instruction);
    }

    fn true_variant(&self) -> Option<u32> {
        let true_variant = self.driver.true_variant()?;
        let enumeration = self.driver.get_enumeration_for_variant(&true_variant);

        match self
            .driver
            .get_type_declaration(&enumeration)
            .item
            .representation
            .item
        {
            crate::typecheck::TypeRepresentation::Enumeration(variants) => variants
                .iter()
                .find_map(|(path, value)| (path == &true_variant).then_some(value.item.index)),
            _ => None,
        }
    }
}

impl Info<'_> {
    fn compile(
        &mut self,
        path: Path,
        captures: &[Path],
        attributes: &[WithInfo<crate::typecheck::Attribute>],
        expression: WithInfo<&crate::typecheck::TypedExpression>,
    ) {
        compile_item_with_captures(
            path,
            captures,
            attributes,
            expression,
            self,
            |expression, info| compile_expression(expression, true, info),
        );
    }
}

fn compile_item_with_captures<'a>(
    path: Path,
    captures_paths: &[Path],
    _attributes: &'a [WithInfo<crate::typecheck::Attribute>],
    expression: WithInfo<&'a crate::typecheck::TypedExpression>,
    info: &mut Info<'_>,
    body: impl FnOnce(WithInfo<&'a crate::typecheck::TypedExpression>, &mut Info<'_>) -> Option<()>,
) -> Option<Vec<u32>> {
    let captures = captures_paths
        .iter()
        .map(|path| {
            Some(
                info.context
                    .variables
                    .iter()
                    .position(|p| p == path)
                    .unwrap_or_else(|| panic!("{path:?}")) as u32,
            )
        })
        .collect::<Option<Vec<_>>>()?;

    info.items.insert(
        path.clone(),
        crate::ir::Item {
            captures: captures.len() as u32,
            expression: expression.as_deref().map(Clone::clone),
            instructions: Vec::new(),
        },
    );

    let context = Context {
        variables: captures_paths.to_vec(),
        ..Default::default()
    };

    let prev_path = mem::replace(&mut info.path, path);
    let prev_context = mem::replace(&mut info.context, context);

    body(expression, info)?;
    assert!(info.context.block_stack.is_empty());
    info.push_instruction(crate::ir::Instruction::Return);

    info.context = prev_context;
    info.path = prev_path;

    Some(captures)
}

#[must_use]
fn compile_expression(
    expression: WithInfo<&crate::typecheck::TypedExpression>,
    tail: bool,
    info: &mut Info<'_>,
) -> Option<()> {
    match &expression.item.kind {
        crate::typecheck::TypedExpressionKind::Unknown(_) => return None,
        crate::typecheck::TypedExpressionKind::Variable(_, path) => {
            let variable = info.context.variables.iter().position(|p| p == path)? as u32;
            info.push_instruction(crate::ir::Instruction::Variable(variable));
        }
        crate::typecheck::TypedExpressionKind::Constant {
            path, parameters, ..
        } => {
            info.push_instruction(crate::ir::Instruction::Constant {
                path: path.clone(),
                parameters: parameters
                    .iter()
                    .map(type_descriptor)
                    .collect::<Option<_>>()?,
            });
        }
        crate::typecheck::TypedExpressionKind::Trait {
            path, parameters, ..
        } => info.push_instruction(crate::ir::Instruction::Instance {
            trait_path: path.clone(),
            parameters: parameters
                .iter()
                .map(type_descriptor)
                .collect::<Option<_>>()?,
        }),
        crate::typecheck::TypedExpressionKind::Number(number) => {
            info.push_instruction(crate::ir::Instruction::Number(number.clone()));
        }
        crate::typecheck::TypedExpressionKind::Text(text) => {
            info.push_instruction(crate::ir::Instruction::Text(text.clone()));
        }
        crate::typecheck::TypedExpressionKind::Block {
            statements,
            captures,
        } => {
            let block_path = info
                .driver
                .item_path_in(&info.path, info.items.len() as u32);

            let captures = compile_item_with_captures(
                block_path.clone(),
                captures,
                &[],
                expression.clone(),
                info,
                |_, info| {
                    for (index, statement) in statements.iter().enumerate() {
                        let last_statement = index + 1 == statements.len();

                        compile_expression(statement.as_ref(), tail && last_statement, info)?;

                        if !last_statement {
                            info.push_instruction(crate::ir::Instruction::Drop);
                        }
                    }

                    Some(())
                },
            )?;

            info.push_instruction(crate::ir::Instruction::Function {
                captures,
                path: block_path,
            });
        }
        crate::typecheck::TypedExpressionKind::Do(block) => {
            compile_expression(block.as_deref(), false, info)?;

            info.push_instruction(if tail {
                crate::ir::Instruction::TailDo
            } else {
                crate::ir::Instruction::Do
            });
        }
        crate::typecheck::TypedExpressionKind::Function {
            inputs,
            body,
            captures,
        } => {
            let function_path = info
                .driver
                .item_path_in(&info.path, info.items.len() as u32);

            let captures = compile_item_with_captures(
                function_path.clone(),
                captures,
                &[],
                expression.clone(),
                info,
                |_, info| {
                    for pattern in inputs {
                        compile_exhaustive_pattern(pattern.as_ref(), info)?;
                    }

                    compile_expression(body.as_deref(), true, info)?;

                    Some(())
                },
            )?;

            info.push_instruction(crate::ir::Instruction::Function {
                captures,
                path: function_path,
            });
        }
        crate::typecheck::TypedExpressionKind::Call { function, inputs } => {
            compile_expression(function.as_deref(), false, info)?;

            for input in inputs {
                compile_expression(input.as_ref(), false, info)?;
            }

            info.push_instruction(if tail {
                crate::ir::Instruction::TailCall(inputs.len() as u32)
            } else {
                crate::ir::Instruction::Call(inputs.len() as u32)
            });
        }
        crate::typecheck::TypedExpressionKind::When { input, arms } => {
            compile_expression(input.as_deref(), false, info)?;
            compile_exhaustive_arms(arms.iter().map(|arm| arm.as_ref()), tail, info)?;
        }
        crate::typecheck::TypedExpressionKind::Intrinsic { name, inputs } => {
            for input in inputs {
                compile_expression(input.as_ref(), false, info)?;
            }

            info.push_instruction(crate::ir::Instruction::Intrinsic {
                name: name.clone(),
                inputs: inputs.len() as u32,
            });
        }
        crate::typecheck::TypedExpressionKind::Initialize { pattern, value } => {
            compile_expression(value.as_deref(), false, info)?;
            compile_exhaustive_pattern(pattern.as_ref(), info)?;
            info.push_instruction(crate::ir::Instruction::Marker);
        }
        crate::typecheck::TypedExpressionKind::Mutate { path, value, .. } => {
            let variable = info
                .context
                .variables
                .iter()
                .position(|p| *p == path.item)? as u32;

            compile_expression(value.as_deref(), false, info)?;

            info.push_instruction(crate::ir::Instruction::Mutate(variable));
            info.push_instruction(crate::ir::Instruction::Marker);
        }
        crate::typecheck::TypedExpressionKind::Marker(_) => {
            info.push_instruction(crate::ir::Instruction::Marker);
        }
        crate::typecheck::TypedExpressionKind::Structure { fields, .. } => {
            let fields = fields
                .iter()
                .map(|field| {
                    compile_expression(field.item.value.as_ref(), false, info)?;
                    field.item.index
                })
                .collect::<Option<_>>()?;

            info.push_instruction(crate::ir::Instruction::Structure(fields));
        }
        crate::typecheck::TypedExpressionKind::Variant { variant, values } => {
            for value in values {
                compile_expression(value.as_ref(), false, info)?;
            }

            // TODO: Get variant index

            let enumeration_path = info.driver.get_enumeration_for_variant(&variant.item);
            let enumeration = info.driver.get_type_declaration(&enumeration_path);

            let variant_index = match enumeration.item.representation.item {
                crate::typecheck::TypeRepresentation::Enumeration(variants) => {
                    variants.iter().find_map(|(path, value)| {
                        (path == &variant.item).then_some(value.item.index)
                    })?
                }
                _ => return None,
            };

            info.push_instruction(crate::ir::Instruction::Variant {
                variant: variant_index,
                elements: values.len() as u32,
            });
        }
        crate::typecheck::TypedExpressionKind::Wrapper(value) => {
            compile_expression(value.as_deref(), false, info)?;

            info.push_instruction(crate::ir::Instruction::Wrapper);
        }
        crate::typecheck::TypedExpressionKind::Tuple(elements) => {
            for element in elements {
                compile_expression(element.as_ref(), false, info)?;
            }

            info.push_instruction(crate::ir::Instruction::Tuple(elements.len() as u32));
        }
        crate::typecheck::TypedExpressionKind::Format { segments, trailing } => {
            let segments = segments
                .iter()
                .map(|segment| {
                    compile_expression(segment.value.as_ref(), false, info)?;
                    Some(segment.text.clone())
                })
                .collect::<Option<_>>()?;

            info.push_instruction(crate::ir::Instruction::Format {
                segments,
                trailing: trailing.clone(),
            });
        }
    }

    Some(())
}

#[must_use]
fn compile_exhaustive_pattern(
    pattern: WithInfo<&crate::typecheck::Pattern>,
    info: &mut Info<'_>,
) -> Option<()> {
    let entry_block = info.current_block();

    let continue_block = info.begin_block();
    let else_block_id = info.begin_block();

    compile_pattern(pattern, else_block_id, info)?;
    info.push_instruction(crate::ir::Instruction::Drop);

    info.break_out_of_block(continue_block);

    info.end_block();

    info.push_instruction(crate::ir::Instruction::Unreachable);
    info.end_block();

    assert_eq!(info.current_block(), entry_block);

    Some(())
}

#[must_use]
fn compile_exhaustive_arms<'a>(
    mut arms: impl ExactSizeIterator<Item = WithInfo<&'a crate::typecheck::TypedArm>>,
    tail: bool,
    info: &mut Info<'_>,
) -> Option<()> {
    let entry_block = info.current_block();

    let continue_block = info.begin_block();
    let else_block_ids = (0..arms.len())
        .map(|_| info.begin_block())
        .collect::<Vec<_>>();

    for (arm, else_block_id) in arms.by_ref().zip(else_block_ids.into_iter().rev()) {
        compile_pattern(arm.item.pattern.as_ref(), else_block_id, info)?;
        info.push_instruction(crate::ir::Instruction::Drop);

        compile_expression(arm.item.body.as_ref(), tail, info)?;
        info.break_out_of_block(continue_block);

        info.end_block();
    }

    info.push_instruction(crate::ir::Instruction::Unreachable);
    info.end_block();

    assert_eq!(info.current_block(), entry_block);

    Some(())
}

#[must_use]
fn compile_pattern(
    pattern: WithInfo<&crate::typecheck::Pattern>,
    else_block_id: u32,
    info: &mut Info<'_>,
) -> Option<()> {
    match &pattern.item {
        crate::typecheck::Pattern::Unknown => return None,
        crate::typecheck::Pattern::Wildcard | crate::typecheck::Pattern::Marker(_) => {}
        crate::typecheck::Pattern::Number(number) => {
            info.push_instruction(crate::ir::Instruction::Copy);

            info.push_instruction(crate::ir::Instruction::Number(number.clone()));

            let continue_block_id = info.begin_block();
            let drop_block_id = info.begin_block();

            info.push_instruction(crate::ir::Instruction::Intrinsic {
                name: info.driver.number_equality_intrinsic()?,
                inputs: 2,
            });

            info.break_out_of_block_if_not(info.true_variant()?, drop_block_id);
            info.push_instruction(crate::ir::Instruction::Drop);
            info.break_out_of_block(continue_block_id);
            info.end_block();

            info.push_instruction(crate::ir::Instruction::Drop);
            info.break_out_of_block(else_block_id);
            info.end_block();
        }
        crate::typecheck::Pattern::Text(text) => {
            info.push_instruction(crate::ir::Instruction::Copy);

            info.push_instruction(crate::ir::Instruction::Text(text.clone()));

            let continue_block_id = info.begin_block();
            let drop_block_id = info.begin_block();

            info.push_instruction(crate::ir::Instruction::Intrinsic {
                name: info.driver.text_equality_intrinsic()?,
                inputs: 2,
            });

            info.break_out_of_block_if_not(info.true_variant()?, drop_block_id);
            info.push_instruction(crate::ir::Instruction::Drop);
            info.break_out_of_block(continue_block_id);
            info.end_block();

            info.push_instruction(crate::ir::Instruction::Drop);
            info.break_out_of_block(else_block_id);
            info.end_block();
        }
        crate::typecheck::Pattern::Variable(_, path) => {
            let variable = info.context.variables.len() as u32;
            info.context.variables.push(path.clone());
            info.push_instruction(crate::ir::Instruction::Initialize(variable));
        }
        crate::typecheck::Pattern::Destructure {
            structure,
            field_patterns,
        } => {
            let structure = &structure.as_ref()?.item;

            let field_indices = match info
                .driver
                .get_type_declaration(structure)
                .item
                .representation
                .item
            {
                crate::typecheck::TypeRepresentation::Structure(fields) => fields
                    .into_iter()
                    .map(|(name, field)| (name, field.item.index))
                    .collect::<HashMap<_, _>>(),
                _ => return None,
            };

            let entry_block = info.current_block();

            let continue_block_id = info.begin_block();
            let break_block_id = info.begin_block();

            for field in field_patterns {
                let field_index = field_indices.get(&field.item.name).copied()?;
                info.push_instruction(crate::ir::Instruction::Field(field_index));
                compile_pattern(field.item.pattern.as_ref(), break_block_id, info)?;
                info.push_instruction(crate::ir::Instruction::Drop);
            }

            info.break_out_of_block(continue_block_id);
            info.end_block();

            info.push_instruction(crate::ir::Instruction::Drop);
            info.break_out_of_block(else_block_id);
            info.end_block();

            assert_eq!(info.current_block(), entry_block);
        }
        crate::typecheck::Pattern::Variant {
            variant,
            value_patterns,
        } => {
            let enumeration = info.driver.get_enumeration_for_variant(&variant.item);

            let variant_index = match info
                .driver
                .get_type_declaration(&enumeration)
                .item
                .representation
                .item
            {
                crate::typecheck::TypeRepresentation::Enumeration(variants) => variants
                    .into_iter()
                    .find_map(|(path, value)| (path == variant.item).then_some(value.item.index))?,
                _ => return None,
            };

            info.break_out_of_block_if_not(variant_index, else_block_id);

            let entry_block = info.current_block();

            let continue_block_id = info.begin_block();
            let break_block_id = info.begin_block();

            for (index, pattern) in value_patterns.iter().enumerate() {
                info.push_instruction(crate::ir::Instruction::VariantElement(index as u32));
                compile_pattern(pattern.as_ref(), break_block_id, info)?;
                info.push_instruction(crate::ir::Instruction::Drop);
            }

            info.break_out_of_block(continue_block_id);
            info.end_block();

            info.push_instruction(crate::ir::Instruction::Drop);
            info.break_out_of_block(else_block_id);
            info.end_block();

            assert_eq!(info.current_block(), entry_block);
        }
        crate::typecheck::Pattern::Wrapper { value_pattern, .. } => {
            let entry_block = info.current_block();

            let continue_block_id = info.begin_block();
            let break_block_id = info.begin_block();

            info.push_instruction(crate::ir::Instruction::Unwrap);
            compile_pattern(value_pattern.as_deref(), break_block_id, info)?;
            info.push_instruction(crate::ir::Instruction::Drop);

            info.break_out_of_block(continue_block_id);
            info.end_block();

            info.push_instruction(crate::ir::Instruction::Drop);
            info.break_out_of_block(else_block_id);
            info.end_block();

            assert_eq!(info.current_block(), entry_block);
        }
        crate::typecheck::Pattern::Tuple(elements) => {
            let entry_block = info.current_block();

            let continue_block_id = info.begin_block();
            let break_block_id = info.begin_block();

            for (index, pattern) in elements.iter().enumerate() {
                info.push_instruction(crate::ir::Instruction::TupleElement(index as u32));
                compile_pattern(pattern.as_ref(), break_block_id, info)?;
                info.push_instruction(crate::ir::Instruction::Drop);
            }

            info.break_out_of_block(continue_block_id);
            info.end_block();

            info.push_instruction(crate::ir::Instruction::Drop);
            info.break_out_of_block(else_block_id);
            info.end_block();

            assert_eq!(info.current_block(), entry_block);
        }
        crate::typecheck::Pattern::Or { left, right } => {
            let continue_block_id = info.begin_block();

            let left_block_id = info.begin_block();
            compile_pattern(left.as_deref(), left_block_id, info)?;
            info.break_out_of_block(continue_block_id);
            info.end_block();

            compile_pattern(right.as_deref(), else_block_id, info)?;
            info.break_out_of_block(continue_block_id);

            info.end_block();
        }
        crate::typecheck::Pattern::Annotate { pattern, .. } => {
            compile_pattern(pattern.as_deref(), else_block_id, info)?;
        }
    }

    Some(())
}

#[must_use]
pub fn type_descriptor(r#type: &crate::typecheck::Type) -> Option<crate::ir::TypeDescriptor> {
    match r#type {
        crate::typecheck::Type::Unknown => None,
        crate::typecheck::Type::Parameter(parameter) => {
            Some(crate::ir::TypeDescriptor::Parameter(parameter.clone()))
        }
        crate::typecheck::Type::Declared { path, parameters } => {
            Some(crate::ir::TypeDescriptor::Named {
                path: path.clone(),
                parameters: parameters
                    .iter()
                    .map(|r#type| type_descriptor(&r#type.item))
                    .collect::<Option<_>>()?,
            })
        }
        crate::typecheck::Type::Function { inputs, output } => {
            Some(crate::ir::TypeDescriptor::Function {
                inputs: inputs
                    .iter()
                    .map(|r#type| type_descriptor(&r#type.item))
                    .collect::<Option<_>>()?,
                output: Box::new(type_descriptor(&output.item)?),
            })
        }
        crate::typecheck::Type::Tuple(elements) => Some(crate::ir::TypeDescriptor::Tuple(
            elements
                .iter()
                .map(|r#type| type_descriptor(&r#type.item))
                .collect::<Option<_>>()?,
        )),
        crate::typecheck::Type::Block(r#type) => Some(crate::ir::TypeDescriptor::Block(Box::new(
            type_descriptor(&r#type.item)?,
        ))),
        crate::typecheck::Type::Intrinsic | crate::typecheck::Type::Message { .. } => {
            Some(crate::ir::TypeDescriptor::Intrinsic)
        }
        crate::typecheck::Type::Equal { left, right } => Some(crate::ir::TypeDescriptor::Equal {
            left: Box::new(type_descriptor(&left.item)?),
            right: Box::new(type_descriptor(&right.item)?),
        }),
    }
}

pub fn layout_descriptor(
    type_declaration: &crate::typecheck::TypeDeclaration,
) -> Option<crate::ir::LayoutDescriptor> {
    match &type_declaration.representation.item {
        crate::typecheck::TypeRepresentation::Marker => Some(crate::ir::LayoutDescriptor::Marker),
        crate::typecheck::TypeRepresentation::Structure(fields) => {
            let fields = fields
                .values()
                .map(|field| Some((field.item.index, type_descriptor(&field.item.r#type.item)?)))
                .collect::<Option<Vec<_>>>()?
                .into_iter()
                .sorted_by_key(|(index, _)| *index)
                .map(|(_, r#type)| r#type)
                .collect();

            Some(crate::ir::LayoutDescriptor::Structure(fields))
        }
        crate::typecheck::TypeRepresentation::Enumeration(variants) => {
            let variants = variants
                .values()
                .map(|variant| {
                    Some((
                        variant.item.index,
                        variant
                            .item
                            .value_types
                            .iter()
                            .map(|r#type| type_descriptor(&r#type.item))
                            .collect::<Option<_>>()?,
                    ))
                })
                .collect::<Option<Vec<_>>>()?
                .into_iter()
                .sorted_by_key(|(index, _)| *index)
                .map(|(_, r#type)| r#type)
                .collect();

            Some(crate::ir::LayoutDescriptor::Enumeration(variants))
        }
        crate::typecheck::TypeRepresentation::Wrapper(r#type) => {
            let r#type = type_descriptor(&r#type.item)?;
            Some(crate::ir::LayoutDescriptor::Wrapper(Box::new(r#type)))
        }
    }
}

pub fn instance_descriptor(
    instance: &crate::typecheck::Instance,
) -> Option<crate::ir::InstanceDescriptor> {
    Some(crate::ir::InstanceDescriptor {
        trait_path: instance.r#trait.clone(),
        parameters: instance
            .parameters
            .iter()
            .map(|r#type| type_descriptor(&r#type.item))
            .collect::<Option<_>>()?,
    })
}
