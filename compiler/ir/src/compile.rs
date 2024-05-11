use derivative::Derivative;
use std::{collections::HashMap, mem};
use wipple_util::WithInfo;

pub fn compile<D: crate::Driver>(
    driver: &D,
    path: D::Path,
    expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
) -> Option<HashMap<D::Path, crate::Item<D>>> {
    let mut info = Info {
        driver,
        path: path.clone(),
        items: Default::default(),
        context: Default::default(),
    };

    compile_item_with_captures(path, &[], expression, &mut info, |expression, info| {
        compile_expression(expression, info)
    })?;

    Some(info.items)
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct Info<'a, D: crate::Driver> {
    #[derivative(Debug = "ignore")]
    driver: &'a D,

    path: D::Path,
    items: HashMap<D::Path, crate::Item<D>>,
    context: Context<D>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Default(bound = ""))]
struct Context<D: crate::Driver> {
    next_block_id: u32,
    block_stack: Vec<(u32, Vec<crate::Instruction<D>>)>,
    variables: Vec<D::Path>,
}

impl<D: crate::Driver> Info<'_, D> {
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

        self.push_instruction(crate::Instruction::Block(instructions));
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

        self.push_instruction(crate::Instruction::Break(relative));
    }

    fn break_out_of_block_if_not(&mut self, variant: u32, id: u32) {
        let relative = self
            .context
            .block_stack
            .iter()
            .rev()
            .take_while(|&&(block_id, _)| block_id != id)
            .count() as u32;

        self.push_instruction(crate::Instruction::BreakIfNot(variant, relative));
    }

    fn push_instruction(&mut self, instruction: crate::Instruction<D>) {
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
            wipple_typecheck::TypeRepresentation::Enumeration(variants) => variants
                .iter()
                .find_map(|(path, value)| (path == &true_variant).then_some(value.item.index)),
            _ => None,
        }
    }
}

#[must_use]
fn compile_item_with_captures<'a, D: crate::Driver>(
    path: D::Path,
    captures_paths: &[D::Path],
    expression: WithInfo<D::Info, &'a wipple_typecheck::TypedExpression<D>>,
    info: &mut Info<'_, D>,
    body: impl FnOnce(
        WithInfo<D::Info, &'a wipple_typecheck::TypedExpression<D>>,
        &mut Info<'_, D>,
    ) -> Option<()>,
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
        crate::Item {
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
    info.push_instruction(crate::Instruction::Return);

    info.context = prev_context;
    info.path = prev_path;

    Some(captures)
}

#[must_use]
fn compile_expression<D: crate::Driver>(
    expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
    info: &mut Info<'_, D>,
) -> Option<()> {
    match &expression.item.kind {
        wipple_typecheck::TypedExpressionKind::Unknown(_) => return None,
        wipple_typecheck::TypedExpressionKind::Variable(_, path) => {
            let variable = info.context.variables.iter().position(|p| p == path)? as u32;
            info.push_instruction(crate::Instruction::Variable(variable));
        }
        wipple_typecheck::TypedExpressionKind::Constant { path, parameters } => {
            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Constant(
                    path.clone(),
                    parameters
                        .iter()
                        .map(type_descriptor)
                        .collect::<Option<_>>()?,
                ),
            ));
        }
        wipple_typecheck::TypedExpressionKind::Trait(path) => {
            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Instance(path.clone()),
            ))
        }
        wipple_typecheck::TypedExpressionKind::Number(number) => {
            info.push_instruction(crate::Instruction::Typed(
                crate::TypeDescriptor::Named(info.driver.number_type()?, Vec::new()),
                crate::TypedInstruction::Number(number.clone()),
            ));
        }
        wipple_typecheck::TypedExpressionKind::Text(text) => {
            info.push_instruction(crate::Instruction::Typed(
                crate::TypeDescriptor::Named(info.driver.text_type()?, Vec::new()),
                crate::TypedInstruction::Text(text.clone()),
            ));
        }
        wipple_typecheck::TypedExpressionKind::Block {
            statements,
            captures,
        } => {
            let block_path = info
                .driver
                .item_path_in(&info.path, info.items.len() as u32);

            let captures = compile_item_with_captures(
                block_path.clone(),
                captures,
                expression.clone(),
                info,
                |_, info| {
                    if statements.is_empty() {
                        info.push_instruction(crate::Instruction::Tuple(0));
                    } else {
                        for (index, statement) in statements.iter().enumerate() {
                            compile_expression(statement.as_ref(), info)?;

                            if index + 1 != statements.len() {
                                info.push_instruction(crate::Instruction::Drop);
                            }
                        }
                    }

                    Some(())
                },
            )?;

            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Function(captures, block_path),
            ));
        }
        wipple_typecheck::TypedExpressionKind::Do(block) => {
            compile_expression(block.as_deref(), info)?;
            info.push_instruction(crate::Instruction::Do);
        }
        wipple_typecheck::TypedExpressionKind::Function {
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
                expression.clone(),
                info,
                |_, info| {
                    for pattern in inputs {
                        compile_exhaustive_pattern(pattern.as_ref(), info)?;
                    }

                    compile_expression(body.as_deref(), info)?;

                    Some(())
                },
            )?;

            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Function(captures, function_path),
            ));
        }
        wipple_typecheck::TypedExpressionKind::Call { function, inputs } => {
            compile_expression(function.as_deref(), info)?;

            for input in inputs {
                compile_expression(input.as_ref(), info)?;
            }

            info.push_instruction(crate::Instruction::Call(inputs.len() as u32));
        }
        wipple_typecheck::TypedExpressionKind::When { input, arms } => {
            compile_expression(input.as_deref(), info)?;
            compile_exhaustive_arms(arms.iter().map(|arm| arm.as_ref()), info)?;
        }
        wipple_typecheck::TypedExpressionKind::Intrinsic { name, inputs } => {
            for input in inputs {
                compile_expression(input.as_ref(), info)?;
            }

            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Intrinsic(name.clone(), inputs.len() as u32),
            ));
        }
        wipple_typecheck::TypedExpressionKind::Initialize { pattern, value } => {
            compile_expression(value.as_deref(), info)?;
            compile_exhaustive_pattern(pattern.as_ref(), info)?;
            info.push_instruction(crate::Instruction::Tuple(0));
        }
        wipple_typecheck::TypedExpressionKind::Mutate { path, value, .. } => {
            let variable = info
                .context
                .variables
                .iter()
                .position(|p| *p == path.item)? as u32;

            compile_expression(value.as_deref(), info)?;

            info.push_instruction(crate::Instruction::Mutate(variable));
            info.push_instruction(crate::Instruction::Tuple(0));
        }
        wipple_typecheck::TypedExpressionKind::Marker(_) => {
            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Marker,
            ));
        }
        wipple_typecheck::TypedExpressionKind::Structure { fields, .. } => {
            let fields = fields
                .iter()
                .map(|field| {
                    compile_expression(field.item.value.as_ref(), info)?;
                    field.item.index
                })
                .collect::<Option<_>>()?;

            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Structure(fields),
            ));
        }
        wipple_typecheck::TypedExpressionKind::Variant { variant, values } => {
            for value in values {
                compile_expression(value.as_ref(), info)?;
            }

            // TODO: Get variant index

            let enumeration_path = info.driver.get_enumeration_for_variant(&variant.item);
            let enumeration = info.driver.get_type_declaration(&enumeration_path);

            let variant_index = match enumeration.item.representation.item {
                wipple_typecheck::TypeRepresentation::Enumeration(variants) => {
                    variants.iter().find_map(|(path, value)| {
                        (path == &variant.item).then_some(value.item.index)
                    })?
                }
                _ => return None,
            };

            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Variant(variant_index, values.len() as u32),
            ));
        }
        wipple_typecheck::TypedExpressionKind::Wrapper(value) => {
            compile_expression(value.as_deref(), info)?;

            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Wrapper,
            ));
        }
        wipple_typecheck::TypedExpressionKind::Tuple(elements) => {
            for element in elements {
                compile_expression(element.as_ref(), info)?;
            }

            info.push_instruction(crate::Instruction::Tuple(elements.len() as u32));
        }
        wipple_typecheck::TypedExpressionKind::Format { segments, trailing } => {
            let segments = segments
                .iter()
                .map(|segment| {
                    compile_expression(segment.value.as_ref(), info)?;
                    Some(segment.text.clone())
                })
                .collect::<Option<_>>()?;

            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Format(segments, trailing.clone()),
            ));
        }
    }

    Some(())
}

#[must_use]
fn compile_exhaustive_pattern<'a, D: crate::Driver + 'a>(
    pattern: WithInfo<D::Info, &'a wipple_typecheck::Pattern<D>>,
    info: &mut Info<'_, D>,
) -> Option<()> {
    let entry_block = info.current_block();

    let continue_block = info.begin_block();
    let else_block_id = info.begin_block();

    compile_pattern(pattern, else_block_id, info)?;
    info.push_instruction(crate::Instruction::Drop);

    info.break_out_of_block(continue_block);

    info.end_block();

    info.push_instruction(crate::Instruction::Unreachable);
    info.end_block();

    assert_eq!(info.current_block(), entry_block);

    Some(())
}

#[must_use]
fn compile_exhaustive_arms<'a, D: crate::Driver + 'a>(
    mut arms: impl ExactSizeIterator<Item = WithInfo<D::Info, &'a wipple_typecheck::TypedArm<D>>>,
    info: &mut Info<'_, D>,
) -> Option<()> {
    let entry_block = info.current_block();

    let continue_block = info.begin_block();
    let else_block_ids = (0..arms.len())
        .map(|_| info.begin_block())
        .collect::<Vec<_>>();

    for (arm, else_block_id) in arms.by_ref().zip(else_block_ids.into_iter().rev()) {
        compile_pattern(arm.item.pattern.as_ref(), else_block_id, info)?;
        info.push_instruction(crate::Instruction::Drop);

        compile_expression(arm.item.body.as_ref(), info)?;
        info.break_out_of_block(continue_block);

        info.end_block();
    }

    info.push_instruction(crate::Instruction::Unreachable);
    info.end_block();

    assert_eq!(info.current_block(), entry_block);

    Some(())
}

#[must_use]
fn compile_pattern<D: crate::Driver>(
    pattern: WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
    else_block_id: u32,
    info: &mut Info<'_, D>,
) -> Option<()> {
    match &pattern.item {
        wipple_typecheck::Pattern::Unknown => return None,
        wipple_typecheck::Pattern::Wildcard => {}
        wipple_typecheck::Pattern::Number(number) => {
            info.push_instruction(crate::Instruction::Copy);

            info.push_instruction(crate::Instruction::Typed(
                crate::TypeDescriptor::Named(info.driver.number_type()?, Vec::new()),
                crate::TypedInstruction::Number(number.clone()),
            ));

            let continue_block_id = info.begin_block();
            let drop_block_id = info.begin_block();

            info.push_instruction(crate::Instruction::Typed(
                crate::TypeDescriptor::Named(info.driver.boolean_type()?, Vec::new()),
                crate::TypedInstruction::Intrinsic(info.driver.number_equality_intrinsic()?, 2),
            ));

            info.break_out_of_block_if_not(info.true_variant()?, drop_block_id);
            info.push_instruction(crate::Instruction::Drop);
            info.break_out_of_block(continue_block_id);
            info.end_block();

            info.push_instruction(crate::Instruction::Drop);
            info.break_out_of_block(else_block_id);
            info.end_block();
        }
        wipple_typecheck::Pattern::Text(text) => {
            info.push_instruction(crate::Instruction::Copy);

            info.push_instruction(crate::Instruction::Typed(
                crate::TypeDescriptor::Named(info.driver.text_type()?, Vec::new()),
                crate::TypedInstruction::Text(text.clone()),
            ));

            let continue_block_id = info.begin_block();
            let drop_block_id = info.begin_block();

            info.push_instruction(crate::Instruction::Typed(
                crate::TypeDescriptor::Named(info.driver.boolean_type()?, Vec::new()),
                crate::TypedInstruction::Intrinsic(info.driver.text_equality_intrinsic()?, 2),
            ));

            info.break_out_of_block_if_not(info.true_variant()?, drop_block_id);
            info.push_instruction(crate::Instruction::Drop);
            info.break_out_of_block(continue_block_id);
            info.end_block();

            info.push_instruction(crate::Instruction::Drop);
            info.break_out_of_block(else_block_id);
            info.end_block();
        }
        wipple_typecheck::Pattern::Variable(_, path) => {
            let variable = info.context.variables.len() as u32;
            info.context.variables.push(path.clone());
            info.push_instruction(crate::Instruction::Initialize(variable));
        }
        wipple_typecheck::Pattern::Destructure {
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
                wipple_typecheck::TypeRepresentation::Structure(fields) => fields
                    .into_iter()
                    .map(|(name, field)| (name, field.item.index))
                    .collect::<HashMap<_, _>>(),
                _ => return None,
            };

            for field in field_patterns {
                let field_index = field_indices.get(&field.item.name).copied()?;
                info.push_instruction(crate::Instruction::Field(field_index));
                compile_pattern(field.item.pattern.as_ref(), else_block_id, info)?;
                info.push_instruction(crate::Instruction::Drop);
            }
        }
        wipple_typecheck::Pattern::Variant {
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
                wipple_typecheck::TypeRepresentation::Enumeration(variants) => variants
                    .into_iter()
                    .find_map(|(path, value)| (path == variant.item).then_some(value.item.index))?,
                _ => return None,
            };

            info.break_out_of_block_if_not(variant_index, else_block_id);

            for (index, pattern) in value_patterns.iter().enumerate() {
                info.push_instruction(crate::Instruction::VariantElement(index as u32));
                compile_pattern(pattern.as_ref(), else_block_id, info)?;
                info.push_instruction(crate::Instruction::Drop);
            }
        }
        wipple_typecheck::Pattern::Wrapper { value_pattern, .. } => {
            info.push_instruction(crate::Instruction::Unwrap);
            compile_pattern(value_pattern.as_deref(), else_block_id, info)?;
            info.push_instruction(crate::Instruction::Drop);
        }
        wipple_typecheck::Pattern::Tuple(elements) => {
            for (index, pattern) in elements.iter().enumerate() {
                info.push_instruction(crate::Instruction::TupleElement(index as u32));
                compile_pattern(pattern.as_ref(), else_block_id, info)?;
                info.push_instruction(crate::Instruction::Drop);
            }
        }
        wipple_typecheck::Pattern::Or { left, right } => {
            let continue_block = info.begin_block();
            let else_block_id = info.begin_block();

            compile_pattern(left.as_deref(), else_block_id, info)?;
            info.break_out_of_block(continue_block);
            info.end_block();

            compile_pattern(right.as_deref(), else_block_id, info)?;
            info.break_out_of_block(continue_block);
            info.end_block();
        }
        wipple_typecheck::Pattern::Annotate { pattern, .. } => {
            compile_pattern(pattern.as_deref(), else_block_id, info)?;
        }
    }

    Some(())
}

#[must_use]
pub fn type_descriptor<D: crate::Driver>(
    r#type: &wipple_typecheck::Type<D>,
) -> Option<crate::TypeDescriptor<D>> {
    match r#type {
        wipple_typecheck::Type::Unknown => None,
        wipple_typecheck::Type::Parameter(parameter) => {
            Some(crate::TypeDescriptor::Parameter(parameter.clone()))
        }
        wipple_typecheck::Type::Declared { path, parameters } => {
            Some(crate::TypeDescriptor::Named(
                path.clone(),
                parameters
                    .iter()
                    .map(|r#type| type_descriptor(&r#type.item))
                    .collect::<Option<_>>()?,
            ))
        }
        wipple_typecheck::Type::Function { inputs, output } => {
            Some(crate::TypeDescriptor::Function(
                inputs
                    .iter()
                    .map(|r#type| type_descriptor(&r#type.item))
                    .collect::<Option<_>>()?,
                Box::new(type_descriptor(&output.item)?),
            ))
        }
        wipple_typecheck::Type::Tuple(elements) => Some(crate::TypeDescriptor::Tuple(
            elements
                .iter()
                .map(|r#type| type_descriptor(&r#type.item))
                .collect::<Option<_>>()?,
        )),
        wipple_typecheck::Type::Block(r#type) => Some(crate::TypeDescriptor::Block(Box::new(
            type_descriptor(&r#type.item)?,
        ))),
        wipple_typecheck::Type::Intrinsic => Some(crate::TypeDescriptor::Intrinsic),
        wipple_typecheck::Type::Message { .. } | wipple_typecheck::Type::Constant(_) => None,
    }
}
