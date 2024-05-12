//! Generate a WebAssembly module from a linked executable.

mod monomorphize;

use std::collections::HashMap;
use wasm_encoder as wasm;
use wipple_ir as ir;
use wipple_linker as linker;

/// Provide the codegen with additional information about the program.
pub trait Driver: linker::Driver {
    /// Whether to import the runtime.
    fn should_import_runtime(&self) -> bool;
}

/// Generate a WebAssembly module from a linked executable.
pub fn to_wasm<D: Driver>(driver: &D, executable: &linker::Executable<D>) -> Vec<u8> {
    let module = monomorphize::Module::from_executable(driver, executable);

    let mut codegen = Codegen::new(driver);

    let wasm = codegen.into_wasm();

    let bytes = wasm.finish();
    wasmparser::validate(&bytes).expect("invalid wasm module");

    bytes
}

struct Codegen<'a, D: Driver> {
    driver: &'a D,
    items: HashMap<D::Path, u32>,
    parameters_type_index: u32,
    item_type_index: u32,
    context_type_index: u32,
    types: wasm::TypeSection,
    functions: wasm::FunctionSection,
    datas: wasm::DataSection,
    imports: wasm::ImportSection,
    codes: wasm::CodeSection,
}

impl<'a, D: Driver> Codegen<'a, D> {
    fn new(driver: &'a D) -> Self {
        let mut types = wasm::TypeSection::new();

        let parameters_type_index = types.len();
        types.array(&wasm::StorageType::Val(wasm::ValType::I32), false);

        let item_type_index = types.len();
        types.struct_([
            // Parameters
            wasm::FieldType {
                element_type: wasm::StorageType::Val(wasm::ValType::Ref(wasm::RefType {
                    nullable: false,
                    heap_type: wasm::HeapType::Concrete(parameters_type_index),
                })),
                mutable: false,
            },
            // Type descriptor
            wasm::FieldType {
                element_type: wasm::StorageType::Val(wasm::ValType::I32),
                mutable: false,
            },
            // Initializer
            wasm::FieldType {
                element_type: wasm::StorageType::Val(wasm::ValType::Ref(wasm::RefType {
                    nullable: false,
                    heap_type: wasm::HeapType::Func,
                })),
                mutable: false,
            },
        ]);

        let context_type_index = types.len();
        types.struct_([
            // Substitutions
            wasm::FieldType {
                element_type: wasm::StorageType::Val(wasm::ValType::Ref(wasm::RefType {
                    nullable: false,
                    heap_type: wasm::HeapType::Any,
                })),
                mutable: false,
            },
            // Task locals
            wasm::FieldType {
                element_type: wasm::StorageType::Val(wasm::ValType::Ref(wasm::RefType {
                    nullable: false,
                    heap_type: wasm::HeapType::Any,
                })),
                mutable: false,
            },
        ]);

        Codegen {
            driver,
            items: HashMap::new(),
            parameters_type_index,
            item_type_index,
            context_type_index,
            types,
            functions: wasm::FunctionSection::new(),
            datas: wasm::DataSection::new(),
            imports: wasm::ImportSection::new(),
            codes: wasm::CodeSection::new(),
        }
    }

    fn compile_item(&mut self, path: &D::Path, item: &linker::LinkedItem<D>) -> u32 {
        // The initializer accepts a context and produces an item
        let type_index = self.types.len();
        self.types.function(
            [wasm::ValType::Ref(wasm::RefType {
                nullable: false,
                heap_type: wasm::HeapType::Concrete(self.context_type_index),
            })],
            [wasm::ValType::Ref(wasm::RefType {
                nullable: false,
                heap_type: wasm::HeapType::Concrete(self.item_type_index),
            })],
        );

        let function_index = self.functions.len();
        self.functions.function(type_index);

        self.items.insert(path.clone(), function_index);

        let mut locals = Vec::new();
        let mut instructions = Vec::new();

        let mut function = wasm::Function::new(locals);
        for instruction in instructions {
            function.instruction(&instruction);
        }

        self.codes.function(&function);

        function_index
    }

    fn compile_value_type(&mut self, type_descriptor: &ir::TypeDescriptor<D>) -> u32 {
        todo!()
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
