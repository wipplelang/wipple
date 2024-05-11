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
    pub exports: HashMap<String, ItemReference<D>>,
    type_map: HashMap<Type<D>, TypeReference<D>>,
    item_map: HashMap<ItemKey<D>, ItemReference<D>>,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum Type<D: crate::Driver> {
    Named(TypeReference<D>),
    Function(Vec<Type<D>>, Box<Type<D>>),
    Tuple(Vec<Type<D>>),
    Block(Box<Type<D>>),
    Intrinsic,
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
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub enum TypeLayout<D: crate::Driver> {
    Marker,
    Structure(Vec<Type<D>>),
    Enumeration(Vec<Vec<Type<D>>>),
    Wrapper(Type<D>),
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
pub struct ItemReference<D: crate::Driver>(pub usize, PhantomData<D>);

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Item<D: crate::Driver> {
    pub entrypoint: Block<D>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Block<D: crate::Driver> {
    pub instructions: Vec<Instruction<D>>,
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
    JumpIfNot(u32, Box<Block<D>>),
    Return,
    Jump(Box<Block<D>>),
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
    Block(Vec<u32>, ItemReference<D>),
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
struct Substitutions<D: crate::Driver>(HashMap<D::Path, TypeReference<D>>);

impl<D: crate::Driver> Substitutions<D> {
    fn get(&self, path: &D::Path) -> TypeReference<D> {
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

        // TODO: Intrinsic variants -> exports

        module
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = "D: std::fmt::Debug"))]
struct Context<'a, D: crate::Driver> {
    driver: &'a D,
    executable: &'a linker::Executable<D>,
    substitutions: Substitutions<D>,
    captures: HashMap<u32, u32>,
}

impl<D: crate::Driver> Module<D> {
    fn compile_item(
        &mut self,
        path: Option<&D::Path>,
        item: &linker::LinkedItem<D>,
        context: &mut Context<'_, D>,
    ) -> ItemReference<D> {
        let key = path.map(|path| ItemKey {
            path: path.clone(),
            parameters: item
                .parameters
                .iter()
                .map(|parameter| context.substitutions.get(parameter))
                .collect(),
        });

        if let Some(key) = &key {
            if let Some(item_reference) = self.item_map.get(key) {
                return *item_reference;
            }
        }

        let item_reference = ItemReference(self.items.len(), PhantomData);

        if let Some(key) = key {
            self.item_map.insert(key, item_reference);
        }

        let entrypoint = todo!(); // self.compile_block(0, &item.ir, context);

        let item = Item { entrypoint };
        self.items.push(item);

        item_reference
    }

    // fn compile_block(
    //     &mut self,
    //     label: ir::Label,
    //     item: &[Vec<ir::Instruction<D>>],
    //     context: &mut Context<'_, D>,
    // ) -> Block<D> {
    //     let instructions = item[label]
    //         .iter()
    //         .map(|instruction| self.compile_instruction(instruction, item, context))
    //         .collect();

    //     Block { instructions }
    // }

    // fn compile_instruction(
    //     &mut self,
    //     instruction: &ir::Instruction<D>,
    //     item: &[Vec<ir::Instruction<D>>],
    //     context: &mut Context<'_, D>,
    // ) -> Instruction<D> {
    //     match instruction {
    //         ir::Instruction::Copy => Instruction::Copy,
    //         ir::Instruction::Drop => Instruction::Drop,
    //         ir::Instruction::Initialize(variable) => Instruction::Initialize(*variable),
    //         ir::Instruction::Field(field) => Instruction::Field(*field),
    //         ir::Instruction::TupleElement(index) => Instruction::TupleElement(*index),
    //         ir::Instruction::VariantElement(index) => Instruction::VariantElement(*index),
    //         ir::Instruction::Unwrap => Instruction::Unwrap,
    //         ir::Instruction::Variable(variable) => Instruction::Variable(*variable),
    //         ir::Instruction::Call(inputs) => Instruction::Call(*inputs),
    //         ir::Instruction::Do => Instruction::Do,
    //         ir::Instruction::Mutate(variable) => Instruction::Mutate(*variable),
    //         ir::Instruction::Tuple(elements) => Instruction::Tuple(*elements),
    //         ir::Instruction::Typed(type_descriptor, instruction) => {
    //             let r#type = self.compile_type(type_descriptor, context);

    //             let instruction = self.compile_typed_instruction(instruction, r#type, context);

    //             Instruction::Typed(r#type, instruction)
    //         }
    //         ir::Instruction::BreakIfNot(variant, label) => {
    //             let block = self.compile_block(*label, item, context);
    //             Instruction::JumpIfNot(*variant, Box::new(block))
    //         }
    //         ir::Instruction::Return => Instruction::Return,
    //         ir::Instruction::Break(label) => {
    //             let block = self.compile_block(*label, item, context);
    //             Instruction::Jump(Box::new(block))
    //         }
    //         ir::Instruction::TailCall(inputs) => Instruction::TailCall(*inputs),
    //         ir::Instruction::TailDo => Instruction::TailDo,
    //         ir::Instruction::Unreachable => Instruction::Unreachable,
    //     }
    // }

    fn compile_typed_instruction(
        &mut self,
        instruction: &ir::TypedInstruction<D>,
        r#type: TypeReference<D>,
        context: &mut Context<'_, D>,
    ) -> TypedInstruction<D> {
        match instruction {
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
                let item_reference = self.compile_item(Some(path), item, context);
                TypedInstruction::Function(captures.clone(), item_reference)
            }
            ir::TypedInstruction::Constant(path, parameters) => {
                let item = context.executable.items.get(path).unwrap();

                let substitutions = Substitutions(
                    item.parameters
                        .iter()
                        .zip(parameters)
                        .map(|(parameter, type_descriptor)| {
                            (
                                parameter.clone(),
                                self.compile_type(type_descriptor, context),
                            )
                        })
                        .collect(),
                );

                let prev_substitutions = mem::replace(&mut context.substitutions, substitutions);
                let item_reference = self.compile_item(Some(path), item, context);
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
                        let instance_type = self.compile_type(&item.type_descriptor, context);

                        let mut substitutions = Substitutions::default();
                        self.unify(r#type, instance_type, &mut substitutions)
                            .then_some((item, substitutions))
                    })
                    .unwrap_or_else(|| {
                        panic!(
                            "no instance found for trait {:?} with type {:#?}",
                            path, r#type
                        )
                    });

                let prev_substitutions = mem::replace(&mut context.substitutions, substitutions);
                let item_reference = self.compile_item(Some(path), item, context);
                context.substitutions = prev_substitutions;

                TypedInstruction::Item(item_reference)
            }
        }
    }

    fn compile_type(
        &mut self,
        r#type: &ir::TypeDescriptor<D>,
        context: &mut Context<'_, D>,
    ) -> TypeReference<D> {
        match r#type {
            ir::TypeDescriptor::Parameter(parameter) => context.substitutions.get(parameter),
            ir::TypeDescriptor::Named(path, parameters) => {
                todo!();
            }
            ir::TypeDescriptor::Function(inputs, output) => todo!(),
            ir::TypeDescriptor::Tuple(_) => todo!(),
            ir::TypeDescriptor::Block(_) => todo!(),
            ir::TypeDescriptor::Intrinsic => todo!(),
        }
    }

    fn unify(
        &mut self,
        left: TypeReference<D>,
        right: TypeReference<D>,
        substitutions: &mut Substitutions<D>,
    ) -> bool {
        todo!()
    }
}
