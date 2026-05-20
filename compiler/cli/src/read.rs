use std::{fs, io, path::Path};
use wipple_core::visit::Visit;
use wipple_syntax::parse;

pub fn read_dir(path: impl AsRef<Path>) -> io::Result<Vec<Box<dyn Visit>>> {
    fs::read_dir(path)?.try_fold(Vec::new(), |mut files, entry| {
        let entry = entry?;

        let path = entry.path();

        if !path.is_file() || path.extension().and_then(|ext| ext.to_str()) != Some("wipple") {
            return Ok(files);
        }

        let Some(path) = path.to_str() else {
            return Ok(files);
        };

        let source = fs::read_to_string(entry.path())?;

        files.push(parse(path, source));

        Ok(files)
    })
}
