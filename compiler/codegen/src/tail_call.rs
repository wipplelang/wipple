pub fn apply<D: crate::Driver>(instructions: &mut Vec<crate::Instruction<D>>) {
    let mut i = 0;
    while i < instructions.len() {
        if let crate::Instruction::Call { .. } = instructions[i] {
            if let Some(crate::Instruction::Return) = instructions.get(i + 1) {
                instructions[i] = crate::Instruction::TailCall;
                instructions.remove(i + 1);
            }
        }

        i += 1;
    }
}
