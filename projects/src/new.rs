use rust_embed::RustEmbed;
use std::{
    fs,
    path::{Path, PathBuf},
};

#[derive(RustEmbed)]
#[folder = "template"]
struct Template;

pub fn new_project(folder: &Path) -> std::io::Result<()> {
    fs::create_dir_all(folder)?;

    for path in Template::iter() {
        let path = path.as_ref();
        let file = Template::get(path).unwrap();
        let path = PathBuf::from(path);

        if let Some(parent) = path.parent() {
            fs::create_dir_all(folder.join(parent))?;
        }

        fs::write(folder.join(path), file)?;
    }

    Ok(())
}
