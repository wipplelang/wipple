use std::{
    env, fs,
    io::{self, Read, Seek},
    mem,
};

fn main() {
    let bin_path = env::current_exe().expect("could not locate current executable");

    let mut file = fs::File::open(bin_path).expect("could not open current executable");

    let item_size = (|| {
        file.seek(io::SeekFrom::End(-(mem::size_of::<u64>() as i64)))?;

        let mut buf = [0u8; mem::size_of::<u64>()];
        file.read_exact(&mut buf)?;

        io::Result::Ok(u64::from_ne_bytes(buf))
    })()
    .expect("could not determine bundle location in executable");

    let bundle = (|| {
        file.seek(io::SeekFrom::End(
            -(item_size as i64) - mem::size_of::<u64>() as i64,
        ))?;

        let mut buf = vec![0; item_size as usize];
        file.read_exact(&mut buf)?;

        io::Result::Ok(buf)
    })()
    .expect("could not read bundle");

    let program = bincode::deserialize_from(bundle.as_slice()).expect("invalid bundle");

    let interpreter =
        wipple_interpreter_backend::Interpreter::handling_output(|text| println!("{}", text));

    if let Err((error, _)) = interpreter.eval(program) {
        eprintln!("fatal error: {}", error);
    }
}
