use crate::{
    database::{Db, NodeRef},
    syntax::parse,
};
use std::{fs, io, path::Path};

#[derive(Debug, Clone)]
pub struct Layer {
    pub name: String,
    pub files: Vec<NodeRef>,
}

pub fn read_file(db: &mut Db, path: impl AsRef<Path>) -> io::Result<NodeRef> {
    let source = fs::read_to_string(path.as_ref())?;
    let file = parse(db, path.as_ref().to_string_lossy(), source);
    Ok(file)
}

pub fn read_layer(db: &mut Db, path: impl AsRef<Path>) -> anyhow::Result<Layer> {
    let path = path.as_ref();

    let files = fs::read_dir(path)?
        .map(|entry| -> anyhow::Result<_> {
            let entry = entry?;

            if !entry.file_type()?.is_file() {
                return Ok(None);
            }

            let path = entry.path();

            if entry
                .path()
                .extension()
                .and_then(|ext| ext.to_str())
                .is_none_or(|ext| ext != "wipple")
            {
                return Ok(None);
            }

            let file = read_file(db, path)?;

            Ok(Some(file))
        })
        .collect::<anyhow::Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();

    Ok(Layer {
        name: path.to_string_lossy().to_string(),
        files,
    })
}
