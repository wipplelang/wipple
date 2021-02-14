use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
pub struct FilePath(String);

#[derive(Clone, Serialize, Deserialize)]
pub struct Location {
    pub file: Option<FilePath>,
    pub line: usize,
    pub column: usize,
}
