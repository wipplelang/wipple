use crate::codegen::{CodegenError, ir};

pub fn collect_imports(definition: &mut ir::Definition) -> Result<(), CodegenError> {
    for instruction in &mut definition.instructions {
        instruction.traverse_mut(&mut |instruction| {
            if let ir::Instruction::Value { node, value } = instruction
                && let ir::Value::Runtime { name, inputs } = value
                && import_as_wasm_instruction(name).is_none()
            {
                let inputs = inputs
                    .iter()
                    .map(|input| {
                        definition.types.get(input).cloned().ok_or_else(|| {
                            anyhow::format_err!("missing type for import input {input:?}")
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let output = definition.types.get(node).cloned().ok_or_else(|| {
                    anyhow::format_err!("missing type for import output {node:?}")
                })?;

                definition.imports.push(ir::Import {
                    name: name.clone(),
                    inputs,
                    output,
                });
            }

            Ok(())
        })?;
    }

    Ok(())
}

pub fn import_as_wasm_instruction(name: &str) -> Option<wasm_encoder::Instruction<'static>> {
    match name {
        "f64.add" => Some(wasm_encoder::Instruction::F64Add),
        "f64.sub" => Some(wasm_encoder::Instruction::F64Sub),
        "f64.mul" => Some(wasm_encoder::Instruction::F64Mul),
        "f64.div" => Some(wasm_encoder::Instruction::F64Div),
        "f64.floor" => Some(wasm_encoder::Instruction::F64Floor),
        "f64.ceil" => Some(wasm_encoder::Instruction::F64Ceil),
        "f64.sqrt" => Some(wasm_encoder::Instruction::F64Sqrt),
        "f64.neg" => Some(wasm_encoder::Instruction::F64Neg),
        "unreachable" => Some(wasm_encoder::Instruction::Unreachable),
        _ => None,
    }
}
