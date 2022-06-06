use std::{
    env,
    error::Error,
    fs::{self, File},
    io::Write,
    path::Path,
};

fn main() -> Result<(), Box<dyn Error>> {
    let mut lib = File::create(Path::new(&env::var("OUT_DIR")?).join("prelude.rs"))?;

    write!(lib, "pub const PRELUDE: &str = concat!(")?;

    let mut entries = fs::read_dir("contents")?.collect::<Result<Vec<_>, _>>()?;
    entries.sort_by_key(|entry| entry.path());

    for entry in entries {
        let path = entry.path();

        write!(
            lib,
            r##"include_str!(concat!(env!("CARGO_MANIFEST_DIR"), r#"/{}"#)), "##,
            path.to_string_lossy()
        )?;
    }

    writeln!(lib, r#");"#,)?;

    Ok(())
}
