use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=src/lib.rs");
    println!("cargo:rerun-if-changed=src/main.rs");

    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());

    let mut build_dir = out_dir
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();

    build_dir.push("wipple_bundled_backend_runner");

    println!(
        "cargo:rustc-env=BUNDLED_RUNNER_PATH={}",
        build_dir.to_string_lossy()
    );
}
