mod diagnostics;
mod executable;
mod graph;
mod ide;

use std::{cell::RefCell, collections::HashMap, sync::Arc};
use wasm_bindgen::prelude::*;
use wipple_core::{
    LibraryArtifact,
    db::{Db, DbRef, Node},
};
use wipple_syntax::{GroupOrder, checks::run_checks, parse};

#[wasm_bindgen]
pub struct CompileResult {
    db: Db,
    path: String,
    root: Node,
    statements: Vec<Node>,
    lib_statements: Vec<Node>,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
pub struct File {
    pub path: String,
    pub code: String,
}

#[wasm_bindgen]
impl File {
    #[wasm_bindgen(constructor)]
    pub fn new(path: String, code: String) -> Self {
        File { path, code }
    }
}

thread_local! {
    static LIBRARIES: RefCell<HashMap<String, Arc<LibraryArtifact<DbRef>>>> = Default::default();
}

#[wasm_bindgen]
pub fn register_library(name: String, bin: Box<[u8]>) -> Result<(), JsError> {
    let artifact = rmp_serde::from_slice::<LibraryArtifact<DbRef>>(&bin)?;
    LIBRARIES.with(|libraries| libraries.borrow_mut().insert(name, Arc::new(artifact)));
    Ok(())
}

#[wasm_bindgen]
pub fn compile(files: Vec<File>, library_name: Option<String>) -> Option<CompileResult> {
    let library = library_name
        .and_then(|name| LIBRARIES.with(|libraries| libraries.borrow().get(&name).cloned()))?;

    let mut db = Db::new();
    db.set_parent(library.db.clone());

    let path = files[0].path.to_string();

    let files = files
        .into_iter()
        .map(|file| parse(file.path, file.code))
        .collect::<Vec<_>>();

    let (root, statements) = wipple_core::compile(
        &mut db,
        &mut library.top_level.clone(),
        files,
        run_checks,
        GroupOrder::new,
    );

    Some(CompileResult {
        db,
        path,
        root,
        statements,
        lib_statements: library.statements.clone(),
    })
}

#[wasm_bindgen]
pub fn format(code: &str) -> Option<String> {
    wipple_parse::format(code)
}
