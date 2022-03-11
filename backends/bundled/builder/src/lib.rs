use wipple_compiler::compile::Program;

pub fn bundle(program: Program, mut runner: Vec<u8>) -> Vec<u8> {
    let mut data = bincode::serialize(&program).unwrap();
    let size = data.len() as u64;

    runner.append(&mut data);
    runner.append(&mut size.to_ne_bytes().to_vec());
    runner
}
