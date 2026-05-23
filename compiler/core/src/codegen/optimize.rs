use crate::codegen::ir::{self, traverse_instructions};
use itertools::Itertools;
use std::mem;

pub fn optimize(program: &mut ir::Program) {
    let mut ctx = OptimizeCtx::default();

    for definition in program.definitions.values_mut() {
        ctx.optimize_definition(definition);
    }

    ctx.optimize_definition(&mut program.top_level);
}

#[derive(Debug, Default)]
struct OptimizeCtx {
    progress: bool,
}

impl OptimizeCtx {
    fn optimize_definition(&mut self, definition: &mut ir::Definition) {
        loop {
            self.progress = false;

            traverse_instructions(&mut definition.instructions, &mut |statements| {
                self.optimize_statements(statements);
                Ok(())
            })
            .unwrap();

            if !self.progress {
                break;
            }
        }
    }

    fn optimize_statements(&mut self, statements: &mut Vec<ir::Instruction>) {
        // Propagate values
        if statements.len() > 1 {
            let mut i = statements.len() - 1;
            while i > 0 {
                let [first, second] = statements.get_disjoint_mut([i - 1, i]).unwrap();

                if let &mut ir::Instruction::Value {
                    node: first_node,
                    value: ref first_value,
                } = first
                    && let &mut ir::Instruction::Value {
                        value: ref mut second_value @ ir::Value::Variable(second_node),
                        ..
                    } = second
                    && first_node == second_node
                {
                    *second_value = first_value.clone();
                    statements.remove(i - 1);

                    // Update references to `second_node`
                    for statement in statements.iter_mut() {
                        statement.for_each_node(true, &mut |node| {
                            if *node == second_node {
                                *node = first_node;
                            }
                        });
                    }

                    self.progress = true;
                }

                i -= 1;
            }
        }

        // Inline returns into conditions
        if statements.len() > 1 {
            let mut i = statements.len() - 1;
            while i > 0 {
                let [first, second] = statements.get_disjoint_mut([i - 1, i]).unwrap();

                if let ir::Instruction::Return { value } = *second
                    && let ir::Instruction::If {
                        node: ref mut if_node @ Some(node),
                        ref mut branches,
                        ref mut else_branch,
                    } = *first
                    && node == value
                {
                    *if_node = None;

                    for (statements, branch_node) in branches
                        .iter_mut()
                        .map(|(_, statements, then_node)| (statements, then_node))
                        .chain(
                            else_branch
                                .as_mut()
                                .map(|(statements, else_node)| (statements, else_node)),
                        )
                    {
                        if let Some(then_node) = *branch_node
                            && let ir::Instruction::Value { node, .. } = *statements.last().unwrap()
                            && node == then_node
                        {
                            statements.push(ir::Instruction::Return { value: then_node });
                            *branch_node = None;
                        }
                    }

                    // Replace `return` with `unreachable`
                    // statements.remove(i);
                    statements[i] = ir::Instruction::If {
                        node: None,
                        branches: vec![],
                        else_branch: None,
                    };

                    self.progress = true;
                }

                i -= 1;
            }
        }

        // Inline immediately-called functions (e.g. `do {...}`)
        if statements.len() > 1 {
            let mut i = statements.len() - 1;
            while i > 0 {
                let [first, second] = statements.get_disjoint_mut([i - 1, i]).unwrap();

                let &mut ir::Instruction::Value {
                    node: function,
                    value:
                        ir::Value::Function {
                            ref inputs,
                            captures: _,
                            ref mut instructions,
                        },
                } = first
                else {
                    break;
                };

                if let &mut ir::Instruction::Value {
                    value:
                        ir::Value::Call {
                            function: called_function,
                            inputs: ref called_inputs,
                        },
                    ..
                }
                | &mut ir::Instruction::ReturnCall {
                    function: called_function,
                    inputs: ref called_inputs,
                } = second
                    && called_function == function
                {
                    for instruction in instructions.iter_mut() {
                        // Update references to inputs
                        instruction.for_each_node(true, &mut |node| {
                            if let Some(index) = inputs.iter().position(|input| input == node) {
                                *node = called_inputs[index];
                            }
                        });

                        // Update references to return value
                        if let ir::Instruction::Value { node, .. } = *second
                            && let ir::Instruction::Return { value } = *instruction
                        {
                            // NOTE: This won't work for early returns
                            *instruction = ir::Instruction::Value {
                                node,
                                value: ir::Value::Variable(value),
                            };
                        }
                    }

                    // Inline function body
                    let instructions = mem::take(instructions);
                    statements.splice(i - 1..=i, instructions);

                    statements.remove(i - 1);
                    self.progress = true;
                }

                i -= 1;
            }
        }

        // Tail call optimization
        if let Some((return_instruction, call_instruction)) =
            statements.iter().rev().take(2).cloned().collect_tuple()
            && let ir::Instruction::Return {
                value: return_value,
            } = return_instruction
            && let ir::Instruction::Value {
                node: value,
                value: ir::Value::Call { function, inputs },
            } = call_instruction
            && value == return_value
        {
            statements.pop();
            statements.pop();
            statements.push(ir::Instruction::ReturnCall { function, inputs });
            self.progress = true;
        }
    }
}
