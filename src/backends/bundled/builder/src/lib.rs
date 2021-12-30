use wipple_frontend::typecheck::*;

pub fn bundle(item: Item, mut runner: Vec<u8>) -> Vec<u8> {
    let mut data = bincode::serialize(&item).unwrap();
    let size = data.len() as u64;

    runner.append(&mut data);
    runner.append(&mut size.to_ne_bytes().to_vec());
    runner
}
