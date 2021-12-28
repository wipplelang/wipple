use std::{env, fs, io::Read, mem};

fn main() {
    let bin_path = env::current_exe().expect("Could not locate current executable");

    let bin = fs::File::open(bin_path)
        .and_then(|file| file.bytes().collect::<Result<Vec<_>, _>>())
        .expect("Could not open current executable");

    let item_size_index = bin.len() - mem::size_of::<usize>();
    let item_index = usize::from_ne_bytes(bin[item_size_index..].try_into().unwrap());

    let item = &bin[item_index..item_size_index];
    let item = serde_cbor::from_slice(item).expect("Invalid binary");

    wipple_interpreter_backend::set_output(|text| println!("{}", text));

    if let Err(error) = wipple_interpreter_backend::eval(&item) {
        eprintln!("Fatal error: {:?}", error)
    }
}
