use wipple_frontend::typecheck::*;

const BUNDLED_RUNNER: &[u8] = include_bytes!(env!("BUNDLED_RUNNER_PATH"));

pub fn bundle(_item: Item) -> Vec<u8> {
    let mut bin = BUNDLED_RUNNER.to_vec();

    bin.append(&mut Vec::new()); // TODO
    bin
}
