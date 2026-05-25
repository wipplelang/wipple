use crate::codegen::{CodegenError, ir};

pub fn collect_imports(definition: &mut ir::Definition) -> Result<(), CodegenError> {
    for instruction in &mut definition.instructions {
        instruction.traverse_mut(&mut |instruction| {
            if let ir::Instruction::Value { node, value } = instruction
                && let ir::Value::Runtime { name, inputs } = value
                && !import_is_wasm_instruction(name)
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

pub fn import_is_wasm_instruction(name: &str) -> bool {
    name.contains(".")
}
