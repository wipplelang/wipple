use wipple_frontend::typecheck::*;

pub fn bundle(item: Item, mut runner: Vec<u8>) -> Vec<u8> {
    let size = runner.len();
    runner.append(&mut serde_cbor::to_vec(&item).unwrap());
    runner.append(&mut size.to_ne_bytes().to_vec());
    runner
}
