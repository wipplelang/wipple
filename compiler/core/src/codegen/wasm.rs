#![allow(clippy::too_many_arguments)]

use crate::{
    codegen::{
        CodegenError, Options, TraceOptions, imports::import_is_wasm_instruction, ir,
        types::ir_named_type_representation,
    },
    db::{Db, Node},
    span::Span,
    visit::IsMutated,
};
use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
};

pub use wasmparser::validate;

#[derive(Debug, Default)]
struct Module {
    types: Types,
    imports: Imports,
    functions: Functions,
    strings: Strings,
    exports: Exports,
    implementations: Implementations,
}

pub fn to_bytes(
    db: &Db,
    program: &ir::Program,
    options: Options<'_>,
) -> Result<Vec<u8>, CodegenError> {
    let mut module = Module::default();
    collect_types(db, program, &mut module.types)?;
    collect_imports(
        db,
        program,
        &mut module.types,
        &mut module.imports,
        &mut module.functions,
    )?;
    collect_functions(db, program, &mut module.types, &mut module.functions)?;
    collect_strings(db, program, &mut module.strings)?;

    let main_index = module
        .functions
        .get(&ir::DefinitionKey::TopLevel, None)
        .ok_or_else(|| anyhow::format_err!("missing top level"))?;

    collect_exports(db, program, main_index, &mut module.exports)?;

    module.types.finish();
    module.strings.finish();

    for (key, node, function, _) in &module.functions.entries {
        let definition = program
            .definitions
            .get(key)
            .ok_or_else(|| anyhow::format_err!("missing definition"))?;

        let mut locals = Locals::default();
        collect_locals(
            db,
            program,
            key,
            definition,
            function,
            &module.types,
            &mut locals,
        )?;

        let mut wasm = wasm_encoder::Function::new(locals.locals.iter().map(|&ty| (1, ty)));

        let mut ctx = WriteContext {
            key,
            definition,
            module: &module,
            locals: &locals,
            options,
            wasm: &mut wasm,
        };

        ctx.write_function(function)?;
        module.implementations.insert(key, *node, wasm)?;
    }

    let mut wasm = wasm_encoder::Module::new();
    wasm.section(&module.types.type_section);
    wasm.section(&module.imports.import_section);
    wasm.section(&module.functions.function_section);
    wasm.section(&module.strings.memory_section);
    wasm.section(&module.exports.export_section);
    wasm.section(&module.functions.elem_section);
    wasm.section(&module.implementations.code_section);
    wasm.section(&module.strings.data_section);

    Ok(wasm.finish())
}

#[derive(Debug)]
struct WriteContext<'a> {
    key: &'a ir::DefinitionKey,
    definition: &'a ir::Definition,
    module: &'a Module,
    locals: &'a Locals,
    options: Options<'a>,
    wasm: &'a mut wasm_encoder::Function,
}

impl WriteContext<'_> {
    fn write_instruction(&mut self, instruction: &wasm_encoder::Instruction<'_>) {
        let _offset = self.wasm.byte_len(); // TODO
        self.wasm.instruction(instruction);
    }

    fn write_function(&mut self, function: &ir::Function) -> Result<(), CodegenError> {
        if let Some((closure, captures)) = &function.closure {
            for (index, &capture) in captures.iter().enumerate() {
                let env_index = 0;

                let env_ty_index = self
                    .module
                    .types
                    .get_env(self.key, *closure)
                    .ok_or_else(|| anyhow::format_err!("missing env"))?;

                self.write_instruction(&wasm_encoder::Instruction::LocalGet(env_index));

                self.write_instruction(&wasm_encoder::Instruction::RefCastNullable(
                    wasm_encoder::HeapType::Concrete(env_ty_index),
                ));

                self.write_instruction(&wasm_encoder::Instruction::StructGet {
                    struct_type_index: env_ty_index,
                    field_index: index as u32,
                });

                self.write_instruction(&wasm_encoder::Instruction::LocalSet(
                    self.locals
                        .get(capture)
                        .ok_or_else(|| anyhow::format_err!("missing capture"))?,
                ));
            }
        }

        self.write_instructions(&function.instructions)?;
        self.write_instruction(&wasm_encoder::Instruction::Return);
        self.write_instruction(&wasm_encoder::Instruction::End);

        Ok(())
    }

    fn write_instructions(&mut self, instructions: &[ir::Instruction]) -> Result<(), CodegenError> {
        for instruction in instructions {
            match instruction {
                ir::Instruction::If {
                    node,
                    branches,
                    else_branch,
                } => {
                    for (conditions, instructions, then_node) in branches {
                        self.write_conditions(conditions)?;

                        self.write_instruction(&wasm_encoder::Instruction::If(
                            wasm_encoder::BlockType::Empty,
                        ));

                        self.write_instructions(instructions)?;

                        if let Some(node) = *node
                            && let Some(then_node) = *then_node
                        {
                            self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                                self.locals
                                    .get(then_node)
                                    .ok_or_else(|| anyhow::format_err!("missing local"))?,
                            ));

                            self.write_instruction(&wasm_encoder::Instruction::LocalSet(
                                self.locals
                                    .get(node)
                                    .ok_or_else(|| anyhow::format_err!("missing local"))?,
                            ));
                        }

                        self.write_instruction(&wasm_encoder::Instruction::Else);
                    }

                    if let Some((instructions, else_node)) = else_branch {
                        self.write_instructions(instructions)?;

                        if let Some(node) = *node
                            && let Some(else_node) = *else_node
                        {
                            self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                                self.locals
                                    .get(else_node)
                                    .ok_or_else(|| anyhow::format_err!("missing local"))?,
                            ));

                            self.write_instruction(&wasm_encoder::Instruction::LocalSet(
                                self.locals
                                    .get(node)
                                    .ok_or_else(|| anyhow::format_err!("missing local"))?,
                            ));
                        }
                    } else {
                        self.write_instruction(&wasm_encoder::Instruction::Unreachable);
                    }

                    for _ in branches {
                        self.write_instruction(&wasm_encoder::Instruction::End);
                    }
                }
                ir::Instruction::Return { value } => {
                    self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                        self.locals
                            .get(*value)
                            .ok_or_else(|| anyhow::format_err!("missing local"))?,
                    ));

                    self.write_instruction(&wasm_encoder::Instruction::Return);
                }
                ir::Instruction::ReturnCall {
                    function,
                    inputs,
                    value: node,
                } => {
                    self.write_call(*function, inputs, *node, true)?;
                }
                ir::Instruction::Trace { span } => {
                    let can_trace = match self.options.trace {
                        TraceOptions::None => false,
                        TraceOptions::All => true,
                        TraceOptions::Files(files) => files.contains(&span.path.as_str()),
                    };

                    if can_trace {
                        self.write_string(&format_trace(span))?;

                        self.write_instruction(&wasm_encoder::Instruction::Call(
                            self.module
                                .imports
                                .get("trace", &[wasm_encoder::ValType::EXTERNREF], &[])
                                .ok_or_else(|| anyhow::format_err!("missing import"))?,
                        ));
                    }
                }
                ir::Instruction::Value { node, value } => {
                    self.write_value(Some(*node), value)?;

                    self.write_instruction(&wasm_encoder::Instruction::LocalSet(
                        self.locals
                            .get(*node)
                            .ok_or_else(|| anyhow::format_err!("missing local"))?,
                    ));
                }
            }
        }

        Ok(())
    }

    fn write_conditions(&mut self, conditions: &[ir::Condition]) -> Result<(), CodegenError> {
        if conditions.is_empty() {
            self.write_instruction(&wasm_encoder::Instruction::I32Const(1));
        } else {
            for (index, condition) in conditions.iter().enumerate() {
                match condition {
                    ir::Condition::Or(branches) => {
                        if branches.is_empty() {
                            self.write_instruction(&wasm_encoder::Instruction::I32Const(0));
                        } else {
                            for (index, conditions) in branches.iter().enumerate() {
                                self.write_conditions(conditions)?;

                                self.write_instruction(&wasm_encoder::Instruction::If(
                                    wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32),
                                ));
                                self.write_instruction(&wasm_encoder::Instruction::I32Const(1));
                                self.write_instruction(&wasm_encoder::Instruction::Else);

                                if index + 1 == branches.len() {
                                    self.write_instruction(&wasm_encoder::Instruction::I32Const(0));
                                }
                            }

                            for _ in branches {
                                self.write_instruction(&wasm_encoder::Instruction::End);
                            }
                        }
                    }
                    ir::Condition::EqualToNumber { input, value } => {
                        self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                            self.locals
                                .get(*input)
                                .ok_or_else(|| anyhow::format_err!("missing local"))?,
                        ));

                        self.write_instruction(&wasm_encoder::Instruction::F64Const(
                            value.parse::<f64>()?.into(),
                        ));

                        self.write_instruction(&wasm_encoder::Instruction::F64Eq);
                    }
                    ir::Condition::EqualToString { input, value } => {
                        self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                            self.locals
                                .get(*input)
                                .ok_or_else(|| anyhow::format_err!("missing local"))?,
                        ));

                        self.write_string(value)?;

                        self.write_instruction(&wasm_encoder::Instruction::Call(
                            self.module
                                .imports
                                .get(
                                    "string-equality",
                                    &[
                                        wasm_encoder::ValType::EXTERNREF,
                                        wasm_encoder::ValType::EXTERNREF,
                                    ],
                                    &[wasm_encoder::ValType::I32],
                                )
                                .ok_or_else(|| anyhow::format_err!("missing import"))?,
                        ));
                    }
                    ir::Condition::EqualToVariant { input, variant } => {
                        self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                            self.locals
                                .get(*input)
                                .ok_or_else(|| anyhow::format_err!("missing local"))?,
                        ));

                        self.write_instruction(&wasm_encoder::Instruction::StructGet {
                            struct_type_index: self
                                .module
                                .types
                                .get_as_index(
                                    self.definition
                                        .types
                                        .get(input)
                                        .ok_or_else(|| anyhow::format_err!("missing type"))?,
                                )
                                .ok_or_else(|| anyhow::format_err!("unresolved input type"))?,
                            field_index: 1,
                        });

                        self.write_instruction(&wasm_encoder::Instruction::I32Const(
                            *variant as i32,
                        ));

                        self.write_instruction(&wasm_encoder::Instruction::I32Eq);
                    }
                    ir::Condition::Initialize {
                        variable,
                        node,
                        value,
                        mutable,
                    } => {
                        self.write_instruction(&wasm_encoder::Instruction::Block(
                            wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32),
                        ));

                        self.write_value(*node, value)?;

                        if *mutable {
                            let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;

                            self.write_instruction(&wasm_encoder::Instruction::StructNew(
                                self.module
                                    .types
                                    .get_box(
                                        &self
                                            .module
                                            .types
                                            .get(self.definition.types.get(&node).ok_or_else(
                                                || anyhow::format_err!("missing type"),
                                            )?)
                                            .ok_or_else(|| {
                                                anyhow::format_err!("unresolved variable type")
                                            })?,
                                    )
                                    .ok_or_else(|| anyhow::format_err!("unresolved box type"))?,
                            ));
                        }

                        self.write_instruction(&wasm_encoder::Instruction::LocalSet(
                            self.locals
                                .get(*variable)
                                .ok_or_else(|| anyhow::format_err!("missing local"))?,
                        ));

                        self.write_instruction(&wasm_encoder::Instruction::I32Const(1));

                        self.write_instruction(&wasm_encoder::Instruction::End);
                    }
                    ir::Condition::Mutate { input, variable } => {
                        self.write_instruction(&wasm_encoder::Instruction::Block(
                            wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32),
                        ));

                        self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                            self.locals
                                .get(*variable)
                                .ok_or_else(|| anyhow::format_err!("missing local"))?,
                        ));

                        self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                            self.locals
                                .get(*input)
                                .ok_or_else(|| anyhow::format_err!("missing local"))?,
                        ));

                        self.write_instruction(&wasm_encoder::Instruction::StructSet {
                            struct_type_index: self
                                .module
                                .types
                                .get_box(
                                    &self
                                        .module
                                        .types
                                        .get(
                                            self.definition.types.get(variable).ok_or_else(
                                                || anyhow::format_err!("missing type"),
                                            )?,
                                        )
                                        .ok_or_else(|| {
                                            anyhow::format_err!("unresolved variable type")
                                        })?,
                                )
                                .ok_or_else(|| anyhow::format_err!("unresolved box type"))?,
                            field_index: 0,
                        });

                        self.write_instruction(&wasm_encoder::Instruction::I32Const(1));

                        self.write_instruction(&wasm_encoder::Instruction::End);
                    }
                }

                self.write_instruction(&wasm_encoder::Instruction::If(
                    wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32),
                ));

                if index + 1 == conditions.len() {
                    self.write_instruction(&wasm_encoder::Instruction::I32Const(1));
                }
            }

            for _ in conditions {
                self.write_instruction(&wasm_encoder::Instruction::Else);
                self.write_instruction(&wasm_encoder::Instruction::I32Const(0));
                self.write_instruction(&wasm_encoder::Instruction::End);
            }
        }

        Ok(())
    }

    fn write_value(&mut self, node: Option<Node>, value: &ir::Value) -> Result<(), CodegenError> {
        match value {
            ir::Value::Bound(node) => {
                return Err(anyhow::format_err!("bound {node:?} not resolved"));
            }
            ir::Value::Call { function, inputs } => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;

                self.write_call(*function, inputs, node, false)?;
            }
            ir::Value::Constant(key) => {
                self.write_instruction(&wasm_encoder::Instruction::Call(
                    self.module
                        .functions
                        .get(key, None)
                        .ok_or_else(|| anyhow::format_err!("missing function"))?,
                ));
            }
            ir::Value::Function(function) => {
                let (closure, captures) = function
                    .closure
                    .as_ref()
                    .ok_or_else(|| anyhow::format_err!("missing closure"))?;

                self.write_instruction(&wasm_encoder::Instruction::RefFunc(
                    self.module
                        .functions
                        .get(self.key, Some(*closure))
                        .ok_or_else(|| anyhow::format_err!("missing function"))?,
                ));

                let env_ty_index = self
                    .module
                    .types
                    .get_env(self.key, *closure)
                    .ok_or_else(|| anyhow::format_err!("missing env"))?;

                for &capture in captures {
                    self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                        self.locals
                            .get(capture)
                            .ok_or_else(|| anyhow::format_err!("missing capture"))?,
                    ));
                }

                self.write_instruction(&wasm_encoder::Instruction::StructNew(env_ty_index));

                self.write_instruction(&wasm_encoder::Instruction::StructNew(
                    self.module
                        .types
                        .get_as_index(
                            self.definition
                                .types
                                .get(closure)
                                .ok_or_else(|| anyhow::format_err!("missing type"))?,
                        )
                        .ok_or_else(|| anyhow::format_err!("missing closure type"))?,
                ));
            }
            ir::Value::Field {
                input, field_index, ..
            } => {
                self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                    self.locals
                        .get(*input)
                        .ok_or_else(|| anyhow::format_err!("missing local"))?,
                ));

                self.write_instruction(&wasm_encoder::Instruction::StructGet {
                    struct_type_index: self
                        .module
                        .types
                        .get_as_index(
                            self.definition
                                .types
                                .get(input)
                                .ok_or_else(|| anyhow::format_err!("missing type"))?,
                        )
                        .ok_or_else(|| anyhow::format_err!("unresolved input type"))?,
                    field_index: *field_index as u32,
                });
            }
            ir::Value::Tuple(elements) => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;

                for &element in elements {
                    self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                        self.locals
                            .get(element)
                            .ok_or_else(|| anyhow::format_err!("missing local"))?,
                    ));
                }

                self.write_instruction(&wasm_encoder::Instruction::StructNew(
                    self.module
                        .types
                        .get_as_index(
                            self.definition
                                .types
                                .get(&node)
                                .ok_or_else(|| anyhow::format_err!("missing type"))?,
                        )
                        .ok_or_else(|| anyhow::format_err!("unresolved tuple type"))?,
                ));
            }
            ir::Value::Marker => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;

                self.write_instruction(&wasm_encoder::Instruction::StructNew(
                    self.module
                        .types
                        .get_as_index(
                            self.definition
                                .types
                                .get(&node)
                                .ok_or_else(|| anyhow::format_err!("missing type"))?,
                        )
                        .ok_or_else(|| anyhow::format_err!("unresolved marker type"))?,
                ));
            }
            ir::Value::MutableVariable(node) => {
                self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                    self.locals
                        .get(*node)
                        .ok_or_else(|| anyhow::format_err!("missing local"))?,
                ));

                self.write_instruction(&wasm_encoder::Instruction::StructGet {
                    struct_type_index: self
                        .module
                        .types
                        .get_box(
                            &self
                                .module
                                .types
                                .get(
                                    self.definition
                                        .types
                                        .get(node)
                                        .ok_or_else(|| anyhow::format_err!("missing type"))?,
                                )
                                .ok_or_else(|| anyhow::format_err!("unresolved variable type"))?,
                        )
                        .ok_or_else(|| anyhow::format_err!("unresolved box type"))?,
                    field_index: 0,
                });
            }
            ir::Value::Number(number) => {
                self.write_instruction(&wasm_encoder::Instruction::F64Const(
                    number.parse::<f64>()?.into(),
                ));
            }
            ir::Value::Runtime { name, inputs } => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;

                for input in inputs {
                    self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                        self.locals
                            .get(*input)
                            .ok_or_else(|| anyhow::format_err!("missing local"))?,
                    ));
                }

                if import_is_wasm_instruction(name) {
                    self.write_wasm_instruction(name)?;
                } else {
                    let input_tys = inputs
                        .iter()
                        .map(|input| {
                            self.module
                                .types
                                .get(
                                    self.definition
                                        .types
                                        .get(input)
                                        .ok_or_else(|| anyhow::format_err!("missing type"))?,
                                )
                                .ok_or_else(|| anyhow::format_err!("unresolved input type"))
                        })
                        .collect::<Result<Vec<_>, CodegenError>>()?;

                    let output_ty = self
                        .module
                        .types
                        .get(
                            self.definition
                                .types
                                .get(&node)
                                .ok_or_else(|| anyhow::format_err!("missing type"))?,
                        )
                        .ok_or_else(|| anyhow::format_err!("unresolved output type"))?;

                    self.write_instruction(&wasm_encoder::Instruction::Call(
                        self.module
                            .imports
                            .get(name, &input_tys, &[output_ty])
                            .ok_or_else(|| anyhow::format_err!("missing import"))?,
                    ));
                }
            }
            ir::Value::String(string) => {
                self.write_string(string)?;
            }
            ir::Value::Structure(fields) => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;

                for (_, value) in fields {
                    self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                        self.locals
                            .get(*value)
                            .ok_or_else(|| anyhow::format_err!("missing local"))?,
                    ));
                }

                self.write_instruction(&wasm_encoder::Instruction::StructNew(
                    self.module
                        .types
                        .get_as_index(
                            self.definition
                                .types
                                .get(&node)
                                .ok_or_else(|| anyhow::format_err!("missing type"))?,
                        )
                        .ok_or_else(|| anyhow::format_err!("unresolved structure type"))?,
                ));
            }
            ir::Value::TupleElement { input, index } => {
                self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                    self.locals
                        .get(*input)
                        .ok_or_else(|| anyhow::format_err!("missing local"))?,
                ));

                self.write_instruction(&wasm_encoder::Instruction::StructGet {
                    struct_type_index: self
                        .module
                        .types
                        .get_as_index(
                            self.definition
                                .types
                                .get(input)
                                .ok_or_else(|| anyhow::format_err!("missing type"))?,
                        )
                        .ok_or_else(|| anyhow::format_err!("unresolved input type"))?,
                    field_index: *index as u32,
                });
            }
            ir::Value::Unreachable => {
                self.write_instruction(&wasm_encoder::Instruction::Unreachable);
            }
            ir::Value::Variable(node) => {
                self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                    self.locals
                        .get(*node)
                        .ok_or_else(|| anyhow::format_err!("missing local"))?,
                ));
            }
            ir::Value::Variant { index, elements } => {
                let node = node.ok_or_else(|| anyhow::format_err!("missing node"))?;

                let enumeration_ty_index = self
                    .module
                    .types
                    .get_as_index(
                        self.definition
                            .types
                            .get(&node)
                            .ok_or_else(|| anyhow::format_err!("missing type"))?,
                    )
                    .ok_or_else(|| anyhow::format_err!("missing enumeration type"))?;

                for &element in elements {
                    self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                        self.locals
                            .get(element)
                            .ok_or_else(|| anyhow::format_err!("missing local"))?,
                    ));
                }

                self.write_instruction(&wasm_encoder::Instruction::StructNew(
                    self.module
                        .types
                        .get_variant(enumeration_ty_index, *index)
                        .ok_or_else(|| anyhow::format_err!("missing variant type"))?,
                ));

                self.write_instruction(&wasm_encoder::Instruction::I32Const(*index as i32));

                self.write_instruction(&wasm_encoder::Instruction::StructNew(enumeration_ty_index));
            }
            ir::Value::VariantElement {
                input,
                variant,
                index,
            } => {
                let enumeration_ty_index = self
                    .module
                    .types
                    .get_as_index(
                        self.definition
                            .types
                            .get(input)
                            .ok_or_else(|| anyhow::format_err!("missing type"))?,
                    )
                    .ok_or_else(|| anyhow::format_err!("missing enumeration type"))?;

                let variant_ty_index = self
                    .module
                    .types
                    .get_variant(enumeration_ty_index, *variant)
                    .ok_or_else(|| anyhow::format_err!("unresolved variant type"))?;

                self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                    self.locals
                        .get(*input)
                        .ok_or_else(|| anyhow::format_err!("missing local"))?,
                ));

                self.write_instruction(&wasm_encoder::Instruction::StructGet {
                    struct_type_index: enumeration_ty_index,
                    field_index: 0,
                });

                self.write_instruction(&wasm_encoder::Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(variant_ty_index),
                ));

                self.write_instruction(&wasm_encoder::Instruction::StructGet {
                    struct_type_index: variant_ty_index,
                    field_index: *index as u32,
                });
            }
        }

        Ok(())
    }

    fn write_call(
        &mut self,
        function: Node,
        inputs: &[Node],
        output: Node,
        tail: bool,
    ) -> Result<(), CodegenError> {
        let closure_ty_index = self
            .module
            .types
            .get_as_index(
                self.definition
                    .types
                    .get(&function)
                    .ok_or_else(|| anyhow::format_err!("missing type"))?,
            )
            .ok_or_else(|| anyhow::format_err!("unresolved closure type"))?;

        let mut input_tys = inputs
            .iter()
            .map(|input| {
                self.module
                    .types
                    .get(
                        self.definition
                            .types
                            .get(input)
                            .ok_or_else(|| anyhow::format_err!("missing type"))?,
                    )
                    .ok_or_else(|| anyhow::format_err!("unresolved input type"))
            })
            .collect::<Result<Vec<_>, CodegenError>>()?;

        let output_ty = self
            .module
            .types
            .get(
                self.definition
                    .types
                    .get(&output)
                    .ok_or_else(|| anyhow::format_err!("missing type"))?,
            )
            .ok_or_else(|| anyhow::format_err!("unresolved output type"))?;

        let env_ty = wasm_encoder::ValType::Ref(wasm_encoder::RefType::ANYREF);
        input_tys.insert(0, env_ty);

        let func_ty_index = self
            .module
            .types
            .get_function(&wasm_encoder::FuncType::new(input_tys, vec![output_ty]))
            .ok_or_else(|| anyhow::format_err!("missing function type"))?;

        self.write_instruction(&wasm_encoder::Instruction::LocalGet(
            self.locals
                .get(function)
                .ok_or_else(|| anyhow::format_err!("missing local"))?,
        ));

        self.write_instruction(&wasm_encoder::Instruction::StructGet {
            struct_type_index: closure_ty_index,
            field_index: 1,
        });

        for input in inputs {
            self.write_instruction(&wasm_encoder::Instruction::LocalGet(
                self.locals
                    .get(*input)
                    .ok_or_else(|| anyhow::format_err!("missing local"))?,
            ));
        }

        self.write_instruction(&wasm_encoder::Instruction::LocalGet(
            self.locals
                .get(function)
                .ok_or_else(|| anyhow::format_err!("missing local"))?,
        ));

        self.write_instruction(&wasm_encoder::Instruction::StructGet {
            struct_type_index: closure_ty_index,
            field_index: 0,
        });

        if tail {
            self.write_instruction(&wasm_encoder::Instruction::ReturnCallRef(func_ty_index));
        } else {
            self.write_instruction(&wasm_encoder::Instruction::CallRef(func_ty_index));
        }

        Ok(())
    }

    fn write_wasm_instruction(&mut self, name: &str) -> Result<(), CodegenError> {
        let instruction = match name {
            "f64.add" => wasm_encoder::Instruction::F64Add,
            "f64.sub" => wasm_encoder::Instruction::F64Sub,
            "f64.mul" => wasm_encoder::Instruction::F64Mul,
            "f64.div" => wasm_encoder::Instruction::F64Div,
            "f64.floor" => wasm_encoder::Instruction::F64Floor,
            "f64.ceil" => wasm_encoder::Instruction::F64Ceil,
            "f64.sqrt" => wasm_encoder::Instruction::F64Sqrt,
            "f64.neg" => wasm_encoder::Instruction::F64Neg,
            _ => return Err(anyhow::format_err!("unsupported wasm instruction {name:?}")),
        };

        self.write_instruction(&instruction);

        Ok(())
    }

    fn write_string(&mut self, string: &str) -> Result<(), CodegenError> {
        let offset = self
            .module
            .strings
            .get(string)
            .ok_or_else(|| anyhow::format_err!("missing trace"))?;

        self.write_instruction(&wasm_encoder::Instruction::I32Const(offset));
        self.write_instruction(&wasm_encoder::Instruction::I32Const(string.len() as i32));

        self.write_instruction(&wasm_encoder::Instruction::Call(
            self.module
                .imports
                .get(
                    "make-string",
                    &[wasm_encoder::ValType::I32, wasm_encoder::ValType::I32],
                    &[wasm_encoder::ValType::EXTERNREF],
                )
                .ok_or_else(|| anyhow::format_err!("missing import"))?,
        ));

        Ok(())
    }
}

#[derive(Debug, Default)]
struct Types {
    next_index: u32,
    type_section: wasm_encoder::TypeSection,
    type_entries: HashMap<ir::Type, wasm_encoder::ValType>,
    func_type_entries: HashMap<wasm_encoder::FuncType, u32>,
    variant_type_entries: HashMap<(u32, usize), u32>,
    box_type_entries: HashMap<wasm_encoder::ValType, u32>,
    env_type_entries: HashMap<(ir::DefinitionKey, Node), u32>,
    queue: Vec<(u32, wasm_encoder::SubType)>,
}

fn subtype(ty: wasm_encoder::CompositeInnerType) -> wasm_encoder::SubType {
    wasm_encoder::SubType {
        is_final: false,
        supertype_idx: None,
        composite_type: wasm_encoder::CompositeType {
            inner: ty,
            shared: false,
            descriptor: None,
            describes: None,
        },
    }
}

impl Types {
    fn insert_function(&mut self, func_type: wasm_encoder::FuncType) -> Result<u32, CodegenError> {
        if let Some(&index) = self.func_type_entries.get(&func_type) {
            return Ok(index);
        }

        let index = self.next_index;
        self.next_index += 1;

        self.func_type_entries.insert(func_type.clone(), index);

        self.queue.push((
            index,
            subtype(wasm_encoder::CompositeInnerType::Func(func_type)),
        ));

        Ok(index)
    }

    fn insert_box(
        &mut self,
        box_type: wasm_encoder::ValType,
    ) -> Result<wasm_encoder::ValType, CodegenError> {
        if let Some(&index) = self.box_type_entries.get(&box_type) {
            return Ok(wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(index),
            }));
        }

        let index = self.next_index;
        self.next_index += 1;

        self.queue.push((
            index,
            subtype(wasm_encoder::CompositeInnerType::Struct(
                wasm_encoder::StructType {
                    fields: Box::new([wasm_encoder::FieldType {
                        element_type: wasm_encoder::StorageType::Val(box_type),
                        mutable: true,
                    }]),
                },
            )),
        ));

        self.box_type_entries.insert(box_type, index);

        Ok(wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(index),
        }))
    }

    fn insert_env(
        &mut self,
        key: &ir::DefinitionKey,
        node: Node,
        captures: Vec<wasm_encoder::ValType>,
    ) -> Result<wasm_encoder::ValType, CodegenError> {
        if let Some(&index) = self.env_type_entries.get(&(key.clone(), node)) {
            return Ok(wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(index),
            }));
        }

        let index = self.next_index;
        self.next_index += 1;

        self.queue.push((
            index,
            subtype(wasm_encoder::CompositeInnerType::Struct(
                wasm_encoder::StructType {
                    fields: captures
                        .into_iter()
                        .map(|ty| wasm_encoder::FieldType {
                            element_type: wasm_encoder::StorageType::Val(ty),
                            mutable: false,
                        })
                        .collect(),
                },
            )),
        ));

        self.env_type_entries.insert((key.clone(), node), index);

        Ok(wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(index),
        }))
    }

    fn insert(&mut self, db: &Db, ty: &ir::Type) -> Result<wasm_encoder::ValType, CodegenError> {
        if let Some(existing) = self.type_entries.get(ty) {
            return Ok(*existing);
        }

        let encoded = match ty {
            ir::Type::Named {
                definition,
                parameters,
            } => {
                let representation =
                    ir_named_type_representation(db, *definition, parameters.clone())?;

                if let ir::TypeRepresentation::Intrinsic { representation } = representation {
                    match representation.as_deref() {
                        Some("f64") => wasm_encoder::ValType::F64,
                        Some(_) => {
                            return Err(anyhow::format_err!("unsupported intrinsic type"));
                        }
                        None => wasm_encoder::ValType::Ref(wasm_encoder::RefType::EXTERNREF),
                    }
                } else {
                    let index = self.next_index;
                    self.next_index += 1;

                    let encoded = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(index),
                    });

                    // Insert before traversing members to prevent infinite recursion
                    self.type_entries.insert(ty.clone(), encoded);

                    match representation {
                        ir::TypeRepresentation::Intrinsic { .. } => unreachable!(),
                        ir::TypeRepresentation::Marker => {
                            self.queue.push((
                                index,
                                subtype(wasm_encoder::CompositeInnerType::Struct(
                                    wasm_encoder::StructType {
                                        fields: Box::new([]),
                                    },
                                )),
                            ));
                        }
                        ir::TypeRepresentation::Structure(fields) => {
                            let fields = fields
                                .iter()
                                .map(|field| {
                                    let ty = self.insert(db, field)?;

                                    Ok(wasm_encoder::FieldType {
                                        element_type: wasm_encoder::StorageType::Val(ty),
                                        mutable: false,
                                    })
                                })
                                .collect::<Result<Vec<_>, CodegenError>>()?;

                            self.queue.push((
                                index,
                                subtype(wasm_encoder::CompositeInnerType::Struct(
                                    wasm_encoder::StructType {
                                        fields: fields.into_boxed_slice(),
                                    },
                                )),
                            ));
                        }
                        ir::TypeRepresentation::Enumeration(variants) => {
                            self.queue.push((
                                index,
                                subtype(wasm_encoder::CompositeInnerType::Struct(
                                    wasm_encoder::StructType {
                                        fields: Box::new([
                                            wasm_encoder::FieldType {
                                                element_type: wasm_encoder::StorageType::Val(
                                                    wasm_encoder::ValType::Ref(
                                                        wasm_encoder::RefType::ANYREF,
                                                    ),
                                                ),
                                                mutable: false,
                                            },
                                            wasm_encoder::FieldType {
                                                element_type: wasm_encoder::StorageType::Val(
                                                    wasm_encoder::ValType::I32,
                                                ),
                                                mutable: false,
                                            },
                                        ]),
                                    },
                                )),
                            ));

                            for (variant, elements) in variants.into_iter().enumerate() {
                                let elements = elements
                                    .iter()
                                    .map(|element| self.insert(db, element))
                                    .collect::<Result<Vec<_>, CodegenError>>()?;

                                let variant_index = self.next_index;
                                self.next_index += 1;

                                self.queue.push((
                                    variant_index,
                                    subtype(wasm_encoder::CompositeInnerType::Struct(
                                        wasm_encoder::StructType {
                                            fields: elements
                                                .into_iter()
                                                .map(|ty| wasm_encoder::FieldType {
                                                    element_type: wasm_encoder::StorageType::Val(
                                                        ty,
                                                    ),
                                                    mutable: false,
                                                })
                                                .collect::<Vec<_>>()
                                                .into_boxed_slice(),
                                        },
                                    )),
                                ));

                                self.variant_type_entries
                                    .insert((index, variant), variant_index);
                            }
                        }
                    }

                    encoded
                }
            }
            ir::Type::Tuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|element| self.insert(db, element))
                    .collect::<Result<Vec<_>, CodegenError>>()?;

                let index = self.next_index;
                self.next_index += 1;

                self.queue.push((
                    index,
                    subtype(wasm_encoder::CompositeInnerType::Struct(
                        wasm_encoder::StructType {
                            fields: elements
                                .into_iter()
                                .map(|element| wasm_encoder::FieldType {
                                    element_type: wasm_encoder::StorageType::Val(element),
                                    mutable: false,
                                })
                                .collect::<Vec<_>>()
                                .into_boxed_slice(),
                        },
                    )),
                ));

                wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(index),
                })
            }
            ir::Type::Function(inputs, output) => {
                let mut inputs = inputs
                    .iter()
                    .map(|input| self.insert(db, input))
                    .collect::<Result<Vec<_>, CodegenError>>()?;

                let output = self.insert(db, output)?;

                let env_ty = wasm_encoder::ValType::Ref(wasm_encoder::RefType::ANYREF);
                inputs.insert(0, env_ty);

                let func_ty_index =
                    self.insert_function(wasm_encoder::FuncType::new(inputs, [output]))?;

                let index = self.next_index;
                self.next_index += 1;

                self.queue.push((
                    index,
                    subtype(wasm_encoder::CompositeInnerType::Struct(
                        wasm_encoder::StructType {
                            fields: Box::new([
                                wasm_encoder::FieldType {
                                    element_type: wasm_encoder::StorageType::Val(
                                        wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                                            nullable: true,
                                            heap_type: wasm_encoder::HeapType::Concrete(
                                                func_ty_index,
                                            ),
                                        }),
                                    ),
                                    mutable: false,
                                },
                                wasm_encoder::FieldType {
                                    element_type: wasm_encoder::StorageType::Val(env_ty),
                                    mutable: false,
                                },
                            ]),
                        },
                    )),
                ));

                wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(index),
                })
            }
            ir::Type::Parameter(node) => {
                return Err(anyhow::format_err!("parameter {node:?} not resolved"));
            }
        };

        self.type_entries.insert(ty.clone(), encoded);

        Ok(encoded)
    }

    fn get(&self, ty: &ir::Type) -> Option<wasm_encoder::ValType> {
        self.type_entries.get(ty).cloned()
    }

    fn get_function(&self, func_type: &wasm_encoder::FuncType) -> Option<u32> {
        self.func_type_entries.get(func_type).copied()
    }

    fn get_variant(&self, ty_index: u32, variant: usize) -> Option<u32> {
        self.variant_type_entries.get(&(ty_index, variant)).copied()
    }

    fn get_box(&self, ty: &wasm_encoder::ValType) -> Option<u32> {
        self.box_type_entries.get(ty).copied()
    }

    fn get_env(&self, key: &ir::DefinitionKey, node: Node) -> Option<u32> {
        self.env_type_entries.get(&(key.clone(), node)).copied()
    }

    fn get_as_index(&self, ty: &ir::Type) -> Option<u32> {
        self.get(ty).and_then(|encoded| match encoded {
            wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                heap_type: wasm_encoder::HeapType::Concrete(index),
                ..
            }) => Some(index),
            _ => None,
        })
    }

    fn finish(&mut self) {
        self.queue.sort_by_key(|(index, _)| *index);

        self.type_section
            .ty()
            .rec(self.queue.drain(..).map(|(_, subtype)| subtype));
    }
}

fn collect_types(db: &Db, program: &ir::Program, types: &mut Types) -> Result<(), CodegenError> {
    for definition in program.definitions.values() {
        for ty in definition.types.values().chain(&definition.ty) {
            ty.clone().traverse_mut(&mut |ty| {
                types.insert(db, ty)?;
                Ok(())
            })?;
        }

        for instruction in &definition.instructions {
            instruction.clone().traverse_mut(&mut |instruction| {
                if let ir::Instruction::If { branches, .. } = instruction {
                    for (conditions, _, _) in branches {
                        for condition in conditions {
                            if let ir::Condition::Initialize {
                                node: Some(node),
                                mutable,
                                ..
                            } = *condition
                                && mutable
                            {
                                let ty = types.insert(
                                    db,
                                    definition
                                        .types
                                        .get(&node)
                                        .ok_or_else(|| anyhow::format_err!("missing type"))?,
                                )?;
                                types.insert_box(ty)?;
                            }
                        }
                    }
                }

                Ok(())
            })?;
        }
    }

    Ok(())
}

#[derive(Debug, Default)]
struct Functions {
    imports_offset: u32,
    function_section: wasm_encoder::FunctionSection,
    elem_section: wasm_encoder::ElementSection,
    entries: Vec<(ir::DefinitionKey, Option<Node>, ir::Function, u32)>,
}

impl Functions {
    fn insert(
        &mut self,
        key: &ir::DefinitionKey,
        node: Option<Node>,
        inputs: &[wasm_encoder::ValType],
        outputs: &[wasm_encoder::ValType],
        function: ir::Function,
        types: &mut Types,
    ) -> Result<u32, CodegenError> {
        if let Some(existing) = self.get(key, node) {
            return Ok(existing);
        }

        let ty_index = types.insert_function(wasm_encoder::FuncType::new(
            inputs.to_vec(),
            outputs.to_vec(),
        ))?;

        let index = self.imports_offset + self.function_section.len();
        self.function_section.function(ty_index);
        self.elem_section
            .declared(wasm_encoder::Elements::Functions(Cow::Borrowed(&[index])));
        self.entries.push((key.clone(), node, function, index));

        Ok(index)
    }

    fn get(&self, key: &ir::DefinitionKey, node: Option<Node>) -> Option<u32> {
        self.entries
            .iter()
            .position(|(other_key, other_node, _, _)| other_key == key && *other_node == node)
            .map(|index| self.imports_offset + index as u32)
    }
}

fn collect_functions(
    db: &Db,
    program: &ir::Program,
    types: &mut Types,
    functions: &mut Functions,
) -> Result<(), CodegenError> {
    for (key, definition) in &program.definitions {
        functions.insert(
            key,
            None,
            &[],
            &Vec::from_iter(
                definition
                    .ty
                    .as_ref()
                    .map(|ty| {
                        types
                            .get(ty)
                            .ok_or_else(|| anyhow::format_err!("unresolved definition type"))
                    })
                    .transpose()?,
            ),
            ir::Function {
                inputs: Vec::new(),
                instructions: definition.instructions.clone(),
                closure: None,
            },
            types,
        )?;

        for instruction in &definition.instructions {
            instruction.clone().traverse_mut(&mut |instruction| {
                if let ir::Instruction::Value {
                    node,
                    value: ir::Value::Function(function),
                } = instruction
                {
                    let ir::Type::Function(inputs, output) = definition
                        .types
                        .get(node)
                        .ok_or_else(|| anyhow::format_err!("missing type"))?
                    else {
                        return Err(anyhow::format_err!("not a function"));
                    };

                    let mut input_tys = inputs
                        .iter()
                        .map(|input| {
                            types
                                .get(input)
                                .ok_or_else(|| anyhow::format_err!("unresolved input type"))
                        })
                        .collect::<Result<Vec<_>, CodegenError>>()?;

                    let output_ty = types
                        .get(output)
                        .ok_or_else(|| anyhow::format_err!("unresolved output type"))?;

                    let mut node = *node;
                    if let Some((closure, captures)) = &function.closure {
                        let captures = captures
                            .iter()
                            .map(|&capture| {
                                let mut ty = definition
                                    .types
                                    .get(&capture)
                                    .ok_or_else(|| anyhow::format_err!("missing type"))
                                    .and_then(|ty| types.insert(db, ty))?;

                                if db.get::<IsMutated>(capture).is_some() {
                                    ty = types.insert_box(ty)?;
                                }

                                Ok(ty)
                            })
                            .collect::<Result<Vec<_>, CodegenError>>()?;

                        types.insert_env(key, *closure, captures)?;

                        input_tys
                            .insert(0, wasm_encoder::ValType::Ref(wasm_encoder::RefType::ANYREF));

                        node = *closure;
                    }

                    functions.insert(
                        key,
                        Some(node),
                        &input_tys,
                        &[output_ty],
                        function.clone(),
                        types,
                    )?;
                }

                Ok(())
            })?;
        }
    }

    Ok(())
}

#[derive(Debug, Default)]
struct Imports {
    import_section: wasm_encoder::ImportSection,
    entries: HashMap<
        (
            String,
            Vec<wasm_encoder::ValType>,
            Vec<wasm_encoder::ValType>,
        ),
        u32,
    >,
}

impl Imports {
    fn insert(
        &mut self,
        name: &str,
        inputs: Vec<wasm_encoder::ValType>,
        outputs: Vec<wasm_encoder::ValType>,
        types: &mut Types,
        functions: &mut Functions,
    ) -> Result<u32, CodegenError> {
        if let Some(existing) = self.get(name, &inputs, &outputs) {
            return Ok(existing);
        }

        let func_ty_index =
            types.insert_function(wasm_encoder::FuncType::new(inputs.clone(), outputs.clone()))?;

        let index = self.import_section.len();

        self.import_section.import(
            "runtime",
            name,
            wasm_encoder::EntityType::Function(func_ty_index),
        );

        self.entries
            .insert((name.to_string(), inputs, outputs), index);

        functions.imports_offset += 1;

        Ok(index)
    }

    fn get(
        &self,
        name: &str,
        inputs: &[wasm_encoder::ValType],
        outputs: &[wasm_encoder::ValType],
    ) -> Option<u32> {
        self.entries
            .get(&(name.to_string(), inputs.to_vec(), outputs.to_vec()))
            .copied()
    }
}

fn collect_imports(
    db: &Db,
    program: &ir::Program,
    types: &mut Types,
    imports: &mut Imports,
    functions: &mut Functions,
) -> Result<(), CodegenError> {
    imports.insert(
        "trace",
        vec![wasm_encoder::ValType::EXTERNREF],
        vec![],
        types,
        functions,
    )?;

    imports.insert(
        "make-string",
        vec![wasm_encoder::ValType::I32, wasm_encoder::ValType::I32],
        vec![wasm_encoder::ValType::EXTERNREF],
        types,
        functions,
    )?;

    imports.insert(
        "string-equality",
        vec![
            wasm_encoder::ValType::EXTERNREF,
            wasm_encoder::ValType::EXTERNREF,
        ],
        vec![wasm_encoder::ValType::I32],
        types,
        functions,
    )?;

    for definition in program.definitions.values() {
        for instruction in &definition.instructions {
            instruction.clone().traverse_mut(&mut |instruction| {
                if let ir::Instruction::Value {
                    node,
                    value: ir::Value::Runtime { name, inputs },
                } = instruction
                {
                    if import_is_wasm_instruction(name) {
                        return Ok(());
                    }

                    let inputs = inputs
                        .iter()
                        .map(|&input| {
                            definition
                                .types
                                .get(&input)
                                .ok_or_else(|| anyhow::format_err!("missing type"))
                                .and_then(|ty| types.insert(db, ty))
                        })
                        .collect::<Result<Vec<_>, CodegenError>>()?;

                    let output = definition
                        .types
                        .get(node)
                        .ok_or_else(|| anyhow::format_err!("missing type"))
                        .and_then(|ty| types.insert(db, ty))?;

                    imports.insert(name, inputs, vec![output], types, functions)?;
                }

                Ok(())
            })?;
        }
    }

    Ok(())
}

#[derive(Debug, Default)]
struct Exports {
    export_section: wasm_encoder::ExportSection,
}

impl Exports {
    fn insert_func(&mut self, name: &str, index: u32) {
        self.export_section
            .export(name, wasm_encoder::ExportKind::Func, index);
    }

    fn insert_memory(&mut self, name: &str, index: u32) {
        self.export_section
            .export(name, wasm_encoder::ExportKind::Memory, index);
    }
}

fn collect_exports(
    _db: &Db,
    _program: &ir::Program,
    main_index: u32,
    exports: &mut Exports,
) -> Result<(), CodegenError> {
    exports.insert_func("main", main_index);
    exports.insert_memory("memory", 0);

    Ok(())
}

#[derive(Debug, Default)]
struct Strings {
    memory_section: wasm_encoder::MemorySection,
    data_section: wasm_encoder::DataSection,
    entries: Vec<(u64, String)>,
}

impl Strings {
    fn insert(&mut self, string: &str) -> Result<i32, CodegenError> {
        if let Some(existing) = self.get(string) {
            return Ok(existing);
        }

        let offset = self
            .entries
            .last()
            .map_or(0, |(offset, s)| *offset + s.len() as u64);

        self.entries.push((offset, string.to_string()));

        self.data_section.active(
            0,
            &wasm_encoder::ConstExpr::i32_const(offset as i32),
            string.bytes(),
        );

        Ok(offset as i32)
    }

    fn get(&self, string: &str) -> Option<i32> {
        self.entries
            .iter()
            .find(|(_, s)| s == string)
            .map(|(offset, _)| *offset as i32)
    }

    fn finish(&mut self) {
        const PAGE_SIZE: u64 = 64 * 1024;

        let pages = self.entries.last().map_or(1, |(offset, s)| {
            (*offset + s.len() as u64).div_ceil(PAGE_SIZE)
        });

        self.memory_section.memory(wasm_encoder::MemoryType {
            minimum: pages,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
    }
}

fn collect_strings(
    _db: &Db,
    program: &ir::Program,
    strings: &mut Strings,
) -> Result<(), CodegenError> {
    for definition in program.definitions.values() {
        for instruction in &definition.instructions {
            instruction.clone().traverse_mut(&mut |instruction| {
                match instruction {
                    ir::Instruction::Trace { span } => {
                        strings.insert(&format_trace(span))?;
                    }
                    ir::Instruction::Value {
                        value: ir::Value::String(string),
                        ..
                    } => {
                        strings.insert(string)?;
                    }
                    _ => {}
                }

                Ok(())
            })?;
        }
    }

    Ok(())
}

#[derive(Debug, Default)]
struct Implementations {
    code_section: wasm_encoder::CodeSection,
    entries: HashMap<(ir::DefinitionKey, Option<Node>), u32>,
}

impl Implementations {
    fn insert(
        &mut self,
        key: &ir::DefinitionKey,
        node: Option<Node>,
        function: wasm_encoder::Function,
    ) -> Result<u32, CodegenError> {
        let index = self.code_section.len();
        self.code_section.function(&function);
        self.entries.insert((key.clone(), node), index);

        Ok(index)
    }
}

#[derive(Debug, Default)]
struct Locals {
    locals: Vec<wasm_encoder::ValType>,
    entries: BTreeMap<Node, u32>,
}

impl Locals {
    fn insert(&mut self, node: Node, ty: wasm_encoder::ValType) -> Result<u32, CodegenError> {
        if let Some(existing) = self.get(node) {
            return Ok(existing);
        }

        let index = self.entries.len() as u32;
        self.locals.push(ty);
        self.entries.insert(node, index);

        Ok(index)
    }

    fn insert_param(&mut self, node: Node, index: u32) -> Result<(), CodegenError> {
        self.entries.insert(node, index);
        Ok(())
    }

    fn get(&self, node: Node) -> Option<u32> {
        self.entries.get(&node).copied()
    }
}

fn collect_locals(
    db: &Db,
    _program: &ir::Program,
    _key: &ir::DefinitionKey,
    definition: &ir::Definition,
    function: &ir::Function,
    types: &Types,
    locals: &mut Locals,
) -> Result<(), CodegenError> {
    for (index, &node) in function
        .closure
        .iter()
        .map(|(node, _)| node)
        .chain(&function.inputs)
        .enumerate()
    {
        locals.insert_param(node, index as u32)?;
    }

    let mut nodes = Vec::new();

    if let Some((_, captures)) = &function.closure {
        for &capture in captures {
            nodes.push(capture);
        }
    }

    for instruction in &function.instructions {
        instruction.clone().for_each_node(false, &mut |node| {
            nodes.push(*node);
            Ok(())
        })?;
    }

    for node in nodes {
        if locals.get(node).is_some() {
            continue;
        }

        let mut ty = types
            .get(
                definition
                    .types
                    .get(&node)
                    .ok_or_else(|| anyhow::format_err!("missing type"))?,
            )
            .ok_or_else(|| anyhow::format_err!("missing type"))?;

        if db.get::<IsMutated>(node).is_some() {
            ty = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(
                    types
                        .get_box(&ty)
                        .ok_or_else(|| anyhow::format_err!("missing type"))?,
                ),
            });
        }

        locals.insert(node, ty)?;
    }

    Ok(())
}

fn format_trace(span: &Span) -> String {
    serde_json::json!({
        "path": span.path,
        "start": span.start,
        "end": span.end,
    })
    .to_string()
}
