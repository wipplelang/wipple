use std::path::Path;

/// Convert a full path to a string of the form `dir/file.ext`, where `dir` is
/// the parent directory.
pub fn get_visible_path(path: &Path) -> String {
    let file_name = |path: &Path| {
        path.file_name().map_or_else(
            || String::from("<unknown>"),
            |file_name| file_name.to_string_lossy().into_owned(),
        )
    };

    match path.canonicalize().as_deref().unwrap_or(path).parent() {
        Some(parent) => format!("{}/{}", file_name(parent), file_name(path)),
        None => file_name(path),
    }
}
