pub fn apply<D: crate::Driver>(instructions: &mut Vec<crate::Instruction<D>>) {
    let mut i = 0;
    while i < instructions.len() {
        match instructions[i] {
            crate::Instruction::Call(inputs) => {
                if let Some(crate::Instruction::Return) = instructions.get(i + 1) {
                    instructions[i] = crate::Instruction::TailCall(inputs);
                    instructions.remove(i + 1);
                }
            }
            crate::Instruction::Do => {
                if let Some(crate::Instruction::Return) = instructions.get(i + 1) {
                    instructions[i] = crate::Instruction::TailDo;
                    instructions.remove(i + 1);
                }
            }
            _ => {}
        }

        i += 1;
    }
}
