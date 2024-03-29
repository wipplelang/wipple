use derivative::Derivative;
use std::mem;
use wipple_util::WithInfo;

pub fn compile<D: crate::Driver>(
    driver: &D,
    path: D::Path,
    expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
) -> Option<Vec<Vec<crate::Instruction<D>>>> {
    let mut info = Info {
        driver,
        path,
        labels: vec![Vec::new()],
        current_label: 0,
        variables: Vec::new(),
        captures: Vec::new(),
    };

    compile_expression(expression, &mut info)?;
    info.push_instruction(crate::Instruction::Return);

    Some(info.labels)
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct Info<'a, D: crate::Driver> {
    #[derivative(Debug = "ignore")]
    driver: &'a D,

    path: D::Path,
    labels: Vec<Vec<crate::Instruction<D>>>,
    current_label: crate::Label,
    variables: Vec<D::Path>,
    captures: Vec<u32>,
}

impl<D: crate::Driver> Info<'_, D> {
    #[must_use]
    fn push_label(&mut self) -> crate::Label {
        let label = self.labels.len();
        self.labels.push(Vec::new());
        label
    }

    fn push_instruction(&mut self, instruction: crate::Instruction<D>) {
        self.labels[self.current_label].push(instruction);
    }
}

#[must_use]
fn compile_expression<D: crate::Driver>(
    expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
    info: &mut Info<'_, D>,
) -> Option<()> {
    match &expression.item.kind {
        wipple_typecheck::TypedExpressionKind::Unknown(_) => return None,
        wipple_typecheck::TypedExpressionKind::Variable(_, path) => {
            let variable = info.variables.iter().position(|p| p == path)? as u32;

            info.captures.push(variable);

            info.push_instruction(crate::Instruction::Variable(variable));
        }
        wipple_typecheck::TypedExpressionKind::Constant { path, parameters } => info
            .push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Constant(
                    path.clone(),
                    parameters
                        .iter()
                        .map(type_descriptor)
                        .collect::<Option<_>>()?,
                ),
            )),
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
        wipple_typecheck::TypedExpressionKind::Block(statements) => {
            let lazy_label = info.push_label();
            let previous_label = mem::replace(&mut info.current_label, lazy_label);

            let prev_num_vars = info.variables.len() as u32;
            let prev_captures = mem::take(&mut info.captures);

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

            info.push_instruction(crate::Instruction::Return);
            let mut captures = mem::replace(&mut info.captures, prev_captures);
            captures.retain(|&var| var < prev_num_vars);
            info.current_label = previous_label;

            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Block(captures, info.path.clone(), lazy_label),
            ));
        }
        wipple_typecheck::TypedExpressionKind::Do(block) => {
            compile_expression(block.as_deref(), info)?;
            info.push_instruction(crate::Instruction::Do);
        }
        wipple_typecheck::TypedExpressionKind::Function { inputs, body } => {
            let function_label = info.push_label();
            let previous_label = mem::replace(&mut info.current_label, function_label);

            let prev_num_vars = info.variables.len() as u32;
            for pattern in inputs {
                compile_exhaustive_pattern(pattern.as_ref(), info)?;
            }

            let prev_captures = mem::take(&mut info.captures);
            compile_expression(body.as_deref(), info)?;
            info.push_instruction(crate::Instruction::Return);
            let mut captures = mem::replace(&mut info.captures, prev_captures);
            captures.retain(|&var| var < prev_num_vars);
            info.current_label = previous_label;

            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Function(captures, info.path.clone(), function_label),
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
            let variable = info.variables.iter().position(|p| *p == path.item)? as u32;
            info.captures.push(variable);

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
                    Some(field.item.name.clone())
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

            info.push_instruction(crate::Instruction::Typed(
                type_descriptor(&expression.item.r#type)?,
                crate::TypedInstruction::Variant(variant.item.clone(), values.len() as u32),
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
    let else_label = info.push_label();
    compile_pattern(pattern, else_label, info)?;
    info.push_instruction(crate::Instruction::Drop);

    let then_label = info.current_label;

    info.current_label = else_label;
    info.push_instruction(crate::Instruction::Unreachable);

    info.current_label = then_label;

    Some(())
}

#[must_use]
fn compile_exhaustive_arms<'a, D: crate::Driver + 'a>(
    arms: impl IntoIterator<Item = WithInfo<D::Info, &'a wipple_typecheck::TypedArm<D>>>,
    info: &mut Info<'_, D>,
) -> Option<()> {
    let continue_label = info.push_label();

    let mut else_label;
    for arm in arms {
        else_label = info.push_label();
        compile_pattern(arm.item.pattern.as_ref(), else_label, info)?;
        info.push_instruction(crate::Instruction::Drop);

        compile_expression(arm.item.body.as_ref(), info)?;
        info.push_instruction(crate::Instruction::Jump(continue_label));

        info.current_label = else_label;
    }

    info.push_instruction(crate::Instruction::Unreachable);

    info.current_label = continue_label;

    Some(())
}

#[must_use]
fn compile_pattern<D: crate::Driver>(
    pattern: WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
    break_label: crate::Label,
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

            info.push_instruction(crate::Instruction::Typed(
                crate::TypeDescriptor::Named(info.driver.boolean_type()?, Vec::new()),
                crate::TypedInstruction::Intrinsic(info.driver.number_equality_intrinsic()?, 2),
            ));

            let else_label = info.push_label();

            info.push_instruction(crate::Instruction::JumpIfNot(
                info.driver.true_variant()?,
                else_label,
            ));

            info.push_instruction(crate::Instruction::Drop);

            let prev_label = mem::replace(&mut info.current_label, else_label);
            info.push_instruction(crate::Instruction::Drop);
            info.push_instruction(crate::Instruction::Jump(break_label));
            info.current_label = prev_label;
        }
        wipple_typecheck::Pattern::Text(text) => {
            info.push_instruction(crate::Instruction::Copy);

            info.push_instruction(crate::Instruction::Typed(
                crate::TypeDescriptor::Named(info.driver.text_type()?, Vec::new()),
                crate::TypedInstruction::Text(text.clone()),
            ));

            info.push_instruction(crate::Instruction::Typed(
                crate::TypeDescriptor::Named(info.driver.boolean_type()?, Vec::new()),
                crate::TypedInstruction::Intrinsic(info.driver.text_equality_intrinsic()?, 2),
            ));

            let else_label = info.push_label();

            info.push_instruction(crate::Instruction::JumpIfNot(
                info.driver.true_variant()?,
                else_label,
            ));

            info.push_instruction(crate::Instruction::Drop);

            let prev_label = mem::replace(&mut info.current_label, else_label);
            info.push_instruction(crate::Instruction::Drop);
            info.push_instruction(crate::Instruction::Jump(break_label));
            info.current_label = prev_label;
        }
        wipple_typecheck::Pattern::Variable(_, path) => {
            let variable = info.variables.len() as u32;
            info.variables.push(path.clone());
            info.push_instruction(crate::Instruction::Initialize(variable));
        }
        wipple_typecheck::Pattern::Destructure(fields) => {
            for field in fields {
                info.push_instruction(crate::Instruction::Field(field.item.name.clone()));
                compile_pattern(field.item.pattern.as_ref(), break_label, info)?;
                info.push_instruction(crate::Instruction::Drop);
            }
        }
        wipple_typecheck::Pattern::Variant {
            variant,
            value_patterns,
        } => {
            info.push_instruction(crate::Instruction::JumpIfNot(
                variant.item.clone(),
                break_label,
            ));

            for (index, pattern) in value_patterns.iter().enumerate() {
                info.push_instruction(crate::Instruction::Element(index as u32));
                compile_pattern(pattern.as_ref(), break_label, info)?;
                info.push_instruction(crate::Instruction::Drop);
            }
        }
        wipple_typecheck::Pattern::Wrapper { value_pattern, .. } => {
            info.push_instruction(crate::Instruction::Unwrap);
            compile_pattern(value_pattern.as_deref(), break_label, info)?;
            info.push_instruction(crate::Instruction::Drop);
        }
        wipple_typecheck::Pattern::Tuple(elements) => {
            for (index, pattern) in elements.iter().enumerate() {
                info.push_instruction(crate::Instruction::Element(index as u32));
                compile_pattern(pattern.as_ref(), break_label, info)?;
                info.push_instruction(crate::Instruction::Drop);
            }
        }
        wipple_typecheck::Pattern::Or { left, right } => {
            let then_label = info.push_label();
            let else_label = info.push_label();
            compile_pattern(left.as_deref(), else_label, info)?;
            info.push_instruction(crate::Instruction::Jump(then_label));

            info.current_label = else_label;
            compile_pattern(right.as_deref(), break_label, info)?;
            info.push_instruction(crate::Instruction::Jump(then_label));

            info.current_label = then_label;
        }
    }

    Some(())
}

#[must_use]
pub fn type_descriptor<D: crate::Driver>(
    r#type: &wipple_typecheck::Type<D>,
) -> Option<crate::TypeDescriptor<D>> {
    match r#type {
        wipple_typecheck::Type::Unknown(_) => None,
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
    }
}
