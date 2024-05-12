//! Generate a WebAssembly module from a linked executable.

mod monomorphize;

use std::{collections::HashMap, mem};
use wasm_encoder as wasm;
use wipple_linker as linker;

/// Provide the codegen with additional information about the program.
pub trait Driver: linker::Driver {
    /// Whether to import the runtime.
    fn should_import_runtime(&self) -> bool;
}

/// Generate a WebAssembly module from a linked executable.
pub fn to_wasm<D: Driver>(driver: &D, executable: &linker::Executable<D>) -> Vec<u8> {
    let module = monomorphize::Module::from_executable(driver, executable);

    let mut codegen = Codegen::new(driver, &module);

    let wasm = codegen.into_wasm();

    let bytes = wasm.finish();
    wasmparser::validate(&bytes).expect("invalid wasm module");

    bytes
}

struct Codegen<'a, D: Driver> {
    driver: &'a D,
    module: &'a monomorphize::Module<D>,
    types: wasm::TypeSection,
    functions: wasm::FunctionSection,
    datas: wasm::DataSection,
    imports: wasm::ImportSection,
    codes: wasm::CodeSection,
    item_map: HashMap<monomorphize::ItemReference<D>, u32>,
    type_map: HashMap<monomorphize::TypeReference<D>, u32>,
    local_map: HashMap<u32, (u32, monomorphize::TypeReference<D>, u32)>,
}

impl<'a, D: Driver> Codegen<'a, D> {
    fn new(driver: &'a D, module: &'a monomorphize::Module<D>) -> Self {
        Codegen {
            driver,
            module,
            types: wasm::TypeSection::new(),
            functions: wasm::FunctionSection::new(),
            datas: wasm::DataSection::new(),
            imports: wasm::ImportSection::new(),
            codes: wasm::CodeSection::new(),
            item_map: HashMap::new(),
            type_map: HashMap::new(),
            local_map: HashMap::new(),
        }
    }

    fn compile_item(&mut self, reference: monomorphize::ItemReference<D>) -> u32 {
        if let Some(index) = self.item_map.get(&reference) {
            return *index;
        }

        let index = self.codes.len();
        self.item_map.insert(reference, index);

        let item = &self.module.items[reference.0];

        let prev_local_map = mem::take(&mut self.local_map);

        let mut body = Vec::new();
        let mut locals = Vec::new();
        for instruction in &item.entrypoint {
            self.compile_instruction(instruction, &mut body, &mut locals, &mut Vec::new());
        }

        self.local_map = prev_local_map;

        let locals = locals.into_iter().map(|r#type| {
            (
                1,
                wasm::ValType::Ref(wasm::RefType {
                    nullable: false,
                    heap_type: wasm::HeapType::Concrete(r#type),
                }),
            )
        });

        let function = wasm::Function::new(locals);
        self.codes.function(&function);

        index
    }

    fn compile_type(&mut self, reference: monomorphize::TypeReference<D>) -> u32 {
        todo!()
    }

    fn compile_function_type(&mut self, reference: monomorphize::TypeReference<D>) -> u32 {
        todo!()
    }

    fn compile_instruction(
        &mut self,
        instruction: &monomorphize::Instruction<D>,
        body: &mut Vec<wasm::Instruction<'static>>,
        locals: &mut Vec<u32>,
        stack: &mut Vec<monomorphize::TypeReference<D>>,
    ) {
        match instruction {
            monomorphize::Instruction::Copy => {
                let r#type = *stack.last().unwrap();
                self.compile_copy(r#type, body, locals);
                stack.push(r#type);
            }
            monomorphize::Instruction::Drop => {
                body.push(wasm::Instruction::Drop);
                stack.pop().unwrap();
            }
            monomorphize::Instruction::Initialize(variable) => {
                let r#type = stack.pop().unwrap();

                let wrapper_index = self.wrap_local_type(r#type);

                let local = locals.len() as u32;
                locals.push(wrapper_index);

                self.local_map
                    .insert(*variable, (wrapper_index, r#type, local));

                body.push(wasm::Instruction::LocalSet(local));
            }
            monomorphize::Instruction::Field(_) => todo!(),
            monomorphize::Instruction::TupleElement(_) => todo!(),
            monomorphize::Instruction::VariantElement(_) => todo!(),
            monomorphize::Instruction::Unwrap => todo!(),
            monomorphize::Instruction::Variable(variable) => {
                let (wrapper_index, r#type, local) = *self.local_map.get(variable).unwrap();
                body.push(wasm::Instruction::LocalGet(local));
                body.push(wasm::Instruction::StructGet {
                    struct_type_index: wrapper_index,
                    field_index: 0,
                });

                stack.push(r#type);
            }
            monomorphize::Instruction::Call(inputs) => {
                self.compile_call(*inputs, false, body, locals, stack);
            }
            monomorphize::Instruction::Do => todo!(),
            monomorphize::Instruction::Mutate(_) => todo!(),
            monomorphize::Instruction::Tuple(_) => todo!(),
            monomorphize::Instruction::Typed(_, _) => todo!(),
            monomorphize::Instruction::Block(_) => todo!(),
            monomorphize::Instruction::BreakIfNot(_, _) => todo!(),
            monomorphize::Instruction::Break(_) => todo!(),
            monomorphize::Instruction::Return => {
                body.push(wasm::Instruction::Return);
            }
            monomorphize::Instruction::TailCall(inputs) => {
                self.compile_call(*inputs, true, body, locals, stack);
            }
            monomorphize::Instruction::TailDo => todo!(),
            monomorphize::Instruction::Unreachable => todo!(),
        }
    }

    fn compile_copy(
        &mut self,
        r#type: monomorphize::TypeReference<D>,
        body: &mut Vec<wasm::Instruction<'static>>,
        locals: &mut Vec<u32>,
    ) {
        let local = locals.len() as u32;
        locals.push(self.compile_type(r#type));

        body.push(wasm::Instruction::LocalTee(local));
        body.push(wasm::Instruction::LocalGet(local));
    }

    fn compile_call(
        &mut self,
        inputs: u32,
        tail: bool,
        body: &mut Vec<wasm::Instruction<'static>>,
        locals: &mut Vec<u32>,
        stack: &mut Vec<monomorphize::TypeReference<D>>,
    ) {
        for _ in 0..inputs {
            stack.pop().unwrap();
        }

        let r#type = stack.pop().unwrap();
        self.compile_copy(r#type, body, locals);
        let wrapper_index = self.compile_type(r#type);

        body.push(wasm::Instruction::StructGet {
            struct_type_index: wrapper_index,
            field_index: 0, // captures
        });

        body.push(wasm::Instruction::StructGet {
            struct_type_index: wrapper_index,
            field_index: 1, // function
        });

        let function_type = self.compile_function_type(r#type);
        body.push(wasm::Instruction::CallRef(function_type));
    }

    // TODO: In the future, detect variables that are never mutated and avoid
    // wrapping them... or maybe we can just use Heap2Local?
    // https://github.com/WebAssembly/binaryen/blob/main/src/passes/Heap2Local.cpp
    fn wrap_local_type(&mut self, r#type: monomorphize::TypeReference<D>) -> u32 {
        let index = self.types.len();

        let fields = [wasm::FieldType {
            element_type: wasm::StorageType::Val(wasm::ValType::Ref(wasm::RefType {
                nullable: false,
                heap_type: wasm::HeapType::Concrete(self.compile_type(r#type)),
            })),
            mutable: true,
        }];

        self.types.struct_(fields);

        index
    }

    fn wrap_function_type(
        &mut self,
        inputs: &[monomorphize::TypeReference<D>],
        output: monomorphize::TypeReference<D>,
    ) -> u32 {
        let index = self.types.len();

        let inputs = inputs.iter().map(|r#type| self.compile_type(*r#type));
        let output = self.compile_type(output);

        todo!();

        index
    }

    fn into_wasm(self) -> wasm::Module {
        let mut module = wasm::Module::new();
        module.section(&self.types);
        module.section(&self.functions);
        module.section(&self.datas);
        module.section(&self.imports);
        module.section(&self.codes);
        module
    }
}
