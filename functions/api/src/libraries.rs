use rust_embed::RustEmbed;
use serde::Deserialize;
use std::{collections::HashMap, path::Path};
use wipple_compiler::File;

#[derive(RustEmbed)]
#[folder = "../../library"]
#[include = "*/metadata.json"]
#[include = "*/src/*.wipple"]
struct Library;

#[derive(Default)]
pub struct LibraryEntry {
    pub metadata: LibraryMetadata,
    pub files: Vec<File>,
}

#[derive(Default, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LibraryMetadata {
    pub library: Option<String>,
    pub ide: Option<serde_json::Value>,
}

pub fn load_libraries() -> HashMap<String, LibraryEntry> {
    let mut libraries = HashMap::<String, LibraryEntry>::new();
    for path in Library::iter() {
        let contents = String::from_utf8(Library::get(&path).unwrap().data.to_vec()).unwrap();

        let path = Path::new(path.as_ref());

        let mut components = path.components().flat_map(|c| c.as_os_str().to_str());

        let Some(library_name) = components.next() else {
            continue;
        };

        let Some(next) = components.next() else {
            continue;
        };

        match next {
            "metadata.json" => {
                libraries
                    .entry(library_name.to_string())
                    .or_default()
                    .metadata = serde_json::from_str(&contents)
                    .unwrap_or_else(|_| panic!("invalid metadata: {path:?}"));
            }
            "src" => {
                let Some(file) = components.next() else {
                    continue;
                };

                if components.next().is_some() {
                    continue;
                };

                let file = File {
                    path: file.to_string(),
                    code: contents,
                };

                libraries
                    .entry(library_name.to_string())
                    .or_default()
                    .files
                    .push(file);
            }
            _ => continue,
        }
    }

    libraries
}
