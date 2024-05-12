#![allow(clippy::non_canonical_clone_impl)]

use derivative::Derivative;
use std::{collections::HashMap, marker::PhantomData, mem};
use wipple_ir as ir;
use wipple_linker as linker;

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
pub struct Module<D: crate::Driver> {
    pub types: Vec<Type<D>>,
    pub items: Vec<Item<D>>,
    pub code: Vec<ItemReference<D>>,
    type_map: HashMap<Type<D>, TypeReference<D>>,
    item_map: HashMap<ItemKey<D>, ItemReference<D>>,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    Copy(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct TypeReference<D: crate::Driver>(pub usize, PhantomData<D>);

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum Type<D: crate::Driver> {
    Marker(TypeParameterList<D>),
    Structure(TypeParameterList<D>, Vec<TypeReference<D>>),
    Enumeration(TypeParameterList<D>, Vec<Vec<TypeReference<D>>>),
    Wrapper(TypeParameterList<D>, TypeReference<D>),
    Function(Vec<TypeReference<D>>, TypeReference<D>),
    Tuple(Vec<TypeReference<D>>),
    Block(TypeReference<D>),
    Intrinsic,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct TypeParameterList<D: crate::Driver>(pub Vec<TypeReference<D>>);

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    Copy(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct ItemReference<D: crate::Driver>(pub usize, PhantomData<D>);

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Item<D: crate::Driver> {
    pub entrypoint: Vec<Instruction<D>>,
    pub captures: Vec<TypeReference<D>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub enum Instruction<D: crate::Driver> {
    Copy,
    Drop,
    Initialize(u32),
    Field(u32),
    TupleElement(u32),
    VariantElement(u32),
    Unwrap,
    Variable(u32),
    Call(u32),
    Do,
    Mutate(u32),
    Tuple(u32),
    Typed(TypeReference<D>, TypedInstruction<D>),
    Block(Vec<Instruction<D>>),
    BreakIfNot(u32, u32),
    Break(u32),
    Return,
    TailCall(u32),
    TailDo,
    Unreachable,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub enum TypedInstruction<D: crate::Driver> {
    Intrinsic(String, u32),
    Text(String),
    Number(String),
    Format(Vec<String>, String),
    Marker,
    Structure(Vec<u32>),
    Variant(u32, u32),
    Wrapper,
    Function(Vec<u32>, ItemReference<D>),
    Item(ItemReference<D>),
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
struct ItemKey<D: crate::Driver> {
    path: D::Path,
    parameters: Vec<TypeReference<D>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Default(bound = ""))]
struct Substitutions<'a, D: crate::Driver>(HashMap<D::Path, &'a ir::TypeDescriptor<D>>);

impl<'a, D: crate::Driver> Substitutions<'a, D> {
    fn get(&self, path: &D::Path) -> &'a ir::TypeDescriptor<D> {
        self.0
            .get(path)
            .copied()
            .unwrap_or_else(|| panic!("no substitution for parameter {:?}", path))
    }
}

impl<D: crate::Driver> Module<D> {
    pub fn from_executable(driver: &D, executable: &linker::Executable<D>) -> Self {
        let mut module = Module::default();

        let mut context = Context {
            driver,
            executable,
            substitutions: Default::default(),
            captures: Default::default(),
        };

        if let Some(entrypoint) = executable.entrypoint.as_ref() {
            let item = executable.items.get(entrypoint).unwrap();
            module.compile_item(None, item, &mut context);
        }

        module
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = "D: std::fmt::Debug"))]
struct Context<'a, D: crate::Driver> {
    driver: &'a D,
    executable: &'a linker::Executable<D>,
    substitutions: Substitutions<'a, D>,
    captures: HashMap<u32, u32>,
}

impl<D: crate::Driver> Module<D> {
    fn compile_item<'a>(
        &mut self,
        path: Option<&D::Path>,
        item: &'a linker::LinkedItem<D>,
        context: &mut Context<'a, D>,
    ) -> Option<ItemReference<D>> {
        let key = match path {
            Some(path) => Some(ItemKey {
                path: path.clone(),
                parameters: item
                    .parameters
                    .iter()
                    .map(|parameter| {
                        self.compile_type(context.substitutions.get(parameter), context)
                    })
                    .collect::<Option<_>>()?,
            }),
            None => None,
        };

        if let Some(key) = &key {
            if let Some(item_reference) = self.item_map.get(key) {
                return Some(*item_reference);
            }
        }

        let item_reference = ItemReference(self.items.len(), PhantomData);

        if let Some(key) = key {
            self.item_map.insert(key, item_reference);
        }

        let entrypoint = item
            .ir
            .iter()
            .map(|instruction| self.compile_instruction(instruction, context))
            .collect::<Option<_>>()?;

        let captures = item
            .captures
            .iter()
            .map(|capture| self.compile_type(capture, context))
            .collect::<Option<_>>()?;

        self.items.push(Item {
            entrypoint,
            captures,
        });

        Some(item_reference)
    }

    fn compile_instruction<'a>(
        &mut self,
        instruction: &'a ir::Instruction<D>,
        context: &mut Context<'a, D>,
    ) -> Option<Instruction<D>> {
        Some(match instruction {
            ir::Instruction::Copy => Instruction::Copy,
            ir::Instruction::Drop => Instruction::Drop,
            ir::Instruction::Initialize(variable) => Instruction::Initialize(*variable),
            ir::Instruction::Field(field) => Instruction::Field(*field),
            ir::Instruction::TupleElement(index) => Instruction::TupleElement(*index),
            ir::Instruction::VariantElement(index) => Instruction::VariantElement(*index),
            ir::Instruction::Unwrap => Instruction::Unwrap,
            ir::Instruction::Variable(variable) => Instruction::Variable(*variable),
            ir::Instruction::Call(inputs) => Instruction::Call(*inputs),
            ir::Instruction::Do => Instruction::Do,
            ir::Instruction::Mutate(variable) => Instruction::Mutate(*variable),
            ir::Instruction::Tuple(elements) => Instruction::Tuple(*elements),
            ir::Instruction::Typed(type_descriptor, instruction) => {
                let r#type = self.compile_type(type_descriptor, context)?;

                let instruction =
                    self.compile_typed_instruction(instruction, type_descriptor, context)?;

                Instruction::Typed(r#type, instruction)
            }
            ir::Instruction::Block(instructions) => {
                let instructions = instructions
                    .iter()
                    .map(|instruction| self.compile_instruction(instruction, context))
                    .collect::<Option<_>>()?;

                Instruction::Block(instructions)
            }
            ir::Instruction::BreakIfNot(variant, blocks) => {
                Instruction::BreakIfNot(*variant, *blocks)
            }
            ir::Instruction::Break(label) => Instruction::Break(*label),
            ir::Instruction::Return => Instruction::Return,
            ir::Instruction::TailCall(inputs) => Instruction::TailCall(*inputs),
            ir::Instruction::TailDo => Instruction::TailDo,
            ir::Instruction::Unreachable => Instruction::Unreachable,
        })
    }

    fn compile_typed_instruction<'a>(
        &mut self,
        instruction: &'a ir::TypedInstruction<D>,
        type_descriptor: &'a ir::TypeDescriptor<D>,
        context: &mut Context<'a, D>,
    ) -> Option<TypedInstruction<D>> {
        Some(match instruction {
            ir::TypedInstruction::Intrinsic(intrinsic, inputs) => {
                TypedInstruction::Intrinsic(intrinsic.clone(), *inputs)
            }
            ir::TypedInstruction::Text(text) => TypedInstruction::Text(text.clone()),
            ir::TypedInstruction::Number(number) => TypedInstruction::Number(number.clone()),
            ir::TypedInstruction::Format(segments, trailing) => {
                TypedInstruction::Format(segments.clone(), trailing.clone())
            }
            ir::TypedInstruction::Marker => TypedInstruction::Marker,
            ir::TypedInstruction::Structure(fields) => TypedInstruction::Structure(fields.clone()),
            ir::TypedInstruction::Variant(index, elements) => {
                TypedInstruction::Variant(*index, *elements)
            }
            ir::TypedInstruction::Wrapper => TypedInstruction::Wrapper,
            ir::TypedInstruction::Function(captures, path) => {
                let item = context.executable.items.get(path).unwrap();
                let item_reference = self.compile_item(Some(path), item, context)?;
                TypedInstruction::Function(captures.clone(), item_reference)
            }
            ir::TypedInstruction::Constant(path, parameters) => {
                let item = context.executable.items.get(path).unwrap();

                let substitutions = Substitutions(
                    item.parameters
                        .iter()
                        .zip(parameters)
                        .map(|(parameter, type_descriptor)| {
                            Some((parameter.clone(), type_descriptor))
                        })
                        .collect::<Option<_>>()?,
                );

                let prev_substitutions = mem::replace(&mut context.substitutions, substitutions);
                let item_reference = self.compile_item(Some(path), item, context)?;
                context.substitutions = prev_substitutions;

                TypedInstruction::Item(item_reference)
            }
            ir::TypedInstruction::Instance(path) => {
                let mut instances = context
                    .executable
                    .instances
                    .get(path)
                    .unwrap()
                    .iter()
                    .chain(context.executable.default_instances.get(path).unwrap());

                let (item, substitutions) = instances
                    .find_map(|instance| {
                        let item = context.executable.items.get(instance).unwrap();

                        let mut substitutions = Substitutions::default();
                        self.unify(type_descriptor, &item.type_descriptor, &mut substitutions)
                            .then_some((item, substitutions))
                    })
                    .unwrap_or_else(|| {
                        panic!(
                            "no instance found for trait {:?} with type {:#?}",
                            path, type_descriptor
                        )
                    });

                let prev_substitutions = mem::replace(&mut context.substitutions, substitutions);
                let item_reference = self.compile_item(Some(path), item, context)?;
                context.substitutions = prev_substitutions;

                TypedInstruction::Item(item_reference)
            }
        })
    }

    fn compile_type<'a>(
        &mut self,
        r#type: &'a ir::TypeDescriptor<D>,
        context: &mut Context<'a, D>,
    ) -> Option<TypeReference<D>> {
        let r#type = match r#type {
            ir::TypeDescriptor::Parameter(parameter) => {
                return self.compile_type(context.substitutions.get(parameter), context);
            }
            ir::TypeDescriptor::Named(path, parameters) => {
                let parameters = TypeParameterList(
                    parameters
                        .iter()
                        .map(|parameter| self.compile_type(parameter, context))
                        .collect::<Option<_>>()?,
                );

                let layout = context.executable.layouts.get(path)?;

                match layout {
                    ir::LayoutDescriptor::Marker => Type::Marker(parameters),
                    ir::LayoutDescriptor::Structure(fields) => {
                        let fields = fields
                            .iter()
                            .map(|field| self.compile_type(field, context))
                            .collect::<Option<_>>()?;

                        Type::Structure(parameters, fields)
                    }
                    ir::LayoutDescriptor::Enumeration(variants) => {
                        let variants = variants
                            .iter()
                            .map(|elements| {
                                elements
                                    .iter()
                                    .map(|element| self.compile_type(element, context))
                                    .collect::<Option<_>>()
                            })
                            .collect::<Option<_>>()?;

                        Type::Enumeration(parameters, variants)
                    }
                    ir::LayoutDescriptor::Wrapper(r#type) => {
                        let r#type = self.compile_type(r#type, context)?;
                        Type::Wrapper(parameters, r#type)
                    }
                    ir::LayoutDescriptor::Intrinsic => Type::Intrinsic,
                }
            }
            ir::TypeDescriptor::Function(inputs, output) => {
                let inputs = inputs
                    .iter()
                    .map(|input| self.compile_type(input, context))
                    .collect::<Option<_>>()?;

                let output = self.compile_type(output, context)?;

                Type::Function(inputs, output)
            }
            ir::TypeDescriptor::Tuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|element| self.compile_type(element, context))
                    .collect::<Option<_>>()?;

                Type::Tuple(elements)
            }
            ir::TypeDescriptor::Block(output) => {
                let output = self.compile_type(output, context)?;
                Type::Block(output)
            }
            ir::TypeDescriptor::Intrinsic => Type::Intrinsic,
        };

        if let Some(type_reference) = self.type_map.get(&r#type) {
            return Some(*type_reference);
        }

        let type_reference = TypeReference(self.types.len(), PhantomData);
        self.type_map.insert(r#type.clone(), type_reference);
        self.types.push(r#type);
        Some(type_reference)
    }

    #[allow(clippy::only_used_in_recursion)] // allow `self`
    fn unify<'a>(
        &mut self,
        left: &'a ir::TypeDescriptor<D>,
        right: &'a ir::TypeDescriptor<D>,
        substitutions: &mut Substitutions<'a, D>,
    ) -> bool {
        let left = match left {
            ir::TypeDescriptor::Parameter(parameter) => substitutions.get(parameter),
            _ => left,
        };

        match (left, right) {
            (_, ir::TypeDescriptor::Parameter(right)) => {
                use std::collections::hash_map::Entry;

                match substitutions.0.entry(right.clone()) {
                    Entry::Occupied(entry) => self.unify(left, entry.get(), substitutions),
                    Entry::Vacant(entry) => {
                        entry.insert(left);
                        true
                    }
                }
            }
            (
                ir::TypeDescriptor::Named(left_path, left_parameters),
                ir::TypeDescriptor::Named(right_path, right_parameters),
            ) => {
                left_path == right_path
                    && left_parameters
                        .iter()
                        .zip(right_parameters)
                        .all(|(left, right)| self.unify(left, right, substitutions))
            }
            (
                ir::TypeDescriptor::Function(left_inputs, left_output),
                ir::TypeDescriptor::Function(right_inputs, right_output),
            ) => {
                left_inputs.len() == right_inputs.len()
                    && left_inputs
                        .iter()
                        .zip(right_inputs)
                        .all(|(left, right)| self.unify(left, right, substitutions))
                    && self.unify(left_output, right_output, substitutions)
            }
            (
                ir::TypeDescriptor::Tuple(left_elements),
                ir::TypeDescriptor::Tuple(right_elements),
            ) => {
                left_elements.len() == right_elements.len()
                    && left_elements
                        .iter()
                        .zip(right_elements)
                        .all(|(left, right)| self.unify(left, right, substitutions))
            }
            (ir::TypeDescriptor::Block(left_output), ir::TypeDescriptor::Block(right_output)) => {
                self.unify(left_output, right_output, substitutions)
            }
            (ir::TypeDescriptor::Intrinsic, ir::TypeDescriptor::Intrinsic) => true,
            _ => false,
        }
    }
}
