use wipple_frontend::typecheck::*;

pub fn bundle(item: Item, mut runner: Vec<u8>) -> Vec<u8> {
    let mut data = serde_json::to_vec(&item).unwrap();
    let size = data.len();

    runner.append(&mut data);
    runner.append(&mut size.to_ne_bytes().to_vec());
    runner
}
