use crate::{
    database::{Db, NodeRef},
    typecheck::Bounds,
    visit::{Defined, Definition},
};
use parcel_sourcemap::SourceMap;
use std::{
    collections::{BTreeSet, HashSet},
    fmt::{Display, Write as _},
};

#[derive(Debug, Clone)]
pub struct CodegenError(String);

impl CodegenError {
    pub fn new(message: impl Into<String>) -> Self {
        CodegenError(message.into())
    }
}

impl Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for CodegenError {}

pub trait Codegen {
    fn codegen(&self, codegen: &mut CodegenCtx<'_>) -> Result<(), CodegenError>;

    fn identifier(&self) -> Option<String> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Options<'a> {
    pub core: &'a str,
    pub runtime: &'a str,
    pub module: bool,
    pub sourcemap: bool,
}

pub struct CodegenCtx<'a> {
    pub db: &'a mut Db,
    options: Options<'a>,
    output: String,
    line: usize,
    column: usize,
    sourcemap: Option<CodegenSourcemap>,
    node: Option<NodeRef>,
    identifier: Option<String>,
    reachable: BTreeSet<NodeRef>,
    reachable_intrinsics: HashSet<String>,
}

#[derive(Debug)]
struct CodegenSourcemap {
    map: SourceMap,
}

pub fn codegen(
    db: &mut Db,
    files: &[NodeRef],
    lib_files: &[NodeRef],
    options: Options<'_>,
) -> Result<String, CodegenError> {
    CodegenCtx::new(db, options).to_string(files, lib_files)
}

impl<'a> CodegenCtx<'a> {
    fn new(db: &'a mut Db, options: Options<'a>) -> Self {
        let sourcemap = options.sourcemap.then(|| CodegenSourcemap {
            map: SourceMap::new(""),
        });

        CodegenCtx {
            db,
            options,
            output: String::new(),
            line: 0,
            column: 0,
            sourcemap,
            identifier: Default::default(),
            node: Default::default(),
            reachable: Default::default(),
            reachable_intrinsics: Default::default(),
        }
    }

    pub fn current_node(&self) -> &NodeRef {
        self.node.as_ref().unwrap()
    }

    pub fn node(&mut self, node: &NodeRef) -> String {
        format!("_{}", node.id())
    }

    pub fn mark_reachable(&mut self, definition: &NodeRef) {
        self.reachable.insert(definition.clone());

        if let Some(Bounds(items)) = self.db.get(self.current_node()) {
            for (_, item) in items {
                self.reachable.insert(item.bound.trait_node);

                if let Some(instance) = item.instance {
                    self.reachable.insert(instance.instance_node);
                }
            }
        }
    }

    pub fn mark_reachable_intrinsic(&mut self, name: &str) {
        self.reachable_intrinsics.insert(name.to_string());
    }

    pub fn write_node(&mut self, node: &NodeRef) {
        let name = self.node(node);
        self.write_string(&name);
    }

    pub fn write_line(&mut self) {
        self.output.push('\n');
        self.line += 1;
        self.column = 0;
    }

    pub fn write_string(&mut self, s: impl AsRef<str>) {
        let s = s.as_ref();

        if let Some(node) = &self.node
            && let Some(sourcemap) = &mut self.sourcemap
        {
            let span = self.db.span(node);

            let source = sourcemap
                .map
                .get_sources()
                .iter()
                .position(|p| p == &span.path)
                .map_or_else(
                    || sourcemap.map.add_source(&span.path),
                    |index| index as u32,
                );

            let name = self.identifier.as_deref().map(|identifier| {
                sourcemap
                    .map
                    .get_name_index(identifier)
                    .unwrap_or_else(|| sourcemap.map.add_name(identifier))
            });

            sourcemap.map.add_mapping(
                self.line as u32,
                self.column as u32,
                Some(parcel_sourcemap::OriginalLocation {
                    original_line: span.start.line as u32 - 1,
                    original_column: span.start.column as u32 - 1,
                    source,
                    name,
                }),
            );
        }

        self.output.push_str(s);
        self.column += s.len();
    }

    pub fn write(&mut self, node: &NodeRef) -> Result<(), CodegenError> {
        let prev_node = self.node.clone();
        let prev_identifier = self.identifier.clone();
        self.node = Some(node.clone());
        self.identifier = node.identifier();

        let result = node.codegen(self);

        self.identifier = prev_identifier;
        self.node = prev_node;

        result
    }

    pub fn error(&self) -> CodegenError {
        CodegenError::new(format!("cannot codegen {:?}", self.node))
    }

    fn write_definitions(&mut self) -> Result<(), CodegenError> {
        let definitions = self
            .db
            .iter()
            .map(|(_, Defined(definition))| definition)
            .collect::<Vec<_>>();

        let mut written = BTreeSet::new();
        loop {
            let mut progress = false;

            for definition in &definitions {
                let node = definition.node();

                if written.contains(&node) || !self.reachable.contains(&node) {
                    continue;
                }

                written.insert(node.clone());
                progress = true;

                let body = match definition {
                    Definition::Constant(definition) => definition.value.as_ref(),
                    Definition::Instance(definition) => definition.value.as_ref(),
                    _ => continue,
                };

                let Some(body) = body else {
                    continue;
                };

                self.write_string(format!("/**! {} */ ", self.db.span(&node)));
                self.write_string("async function ");
                self.write_node(&node);
                self.write_string("(__wipple_bounds) {");
                self.write_line();
                self.write_string("return ");
                self.write(body)?;
                self.write_string(";");
                self.write_line();
                self.write_string("}");
                self.write_line();
            }

            if !progress {
                break;
            }
        }

        Ok(())
    }

    fn write_intrinsics(&mut self, runtime: &str) -> Result<(), CodegenError> {
        let prefix = "const __wipple_runtime_";

        let mut indices = runtime.match_indices(prefix).peekable();
        while let Some(start) = indices.next() {
            let end = indices.peek();

            let function = match end {
                Some(end) => &runtime[start.0..end.0],
                None => &runtime[start.0..],
            };

            let function = function.strip_prefix(prefix).unwrap();

            if self
                .reachable_intrinsics
                .iter()
                .any(|f| function.starts_with(f))
            {
                self.write_string(prefix);
                self.write_string(function);
            }
        }

        Ok(())
    }

    pub fn to_string(
        mut self,
        files: &[NodeRef],
        lib_files: &[NodeRef],
    ) -> Result<String, CodegenError> {
        colored::control::set_override(false);

        if self.options.module {
            self.write_string("let __wipple_env, __wipple_proxy;");
            self.write_line();
            self.write_string("async function __wipple_main(env, proxy) {");
            self.write_line();
            self.write_string("__wipple_env = env;");
            self.write_line();
            self.write_string("__wipple_proxy = proxy;");
            self.write_line();
        } else {
            self.write_string("async function __wipple_main() {");
            self.write_line();
        }

        for file in files {
            self.write(file)?;
        }

        self.write_string("};");
        self.write_line();

        self.write_definitions()?;

        self.output.push_str(self.options.core);
        self.write_intrinsics(self.options.runtime)?;

        if self.options.module {
            self.write_string("export default __wipple_main;");
            self.write_line();
        } else {
            self.write_string("__wipple_main();");
            self.write_line();
        }

        if let Some(mut sourcemap) = self.sourcemap {
            for file in files.iter().chain(lib_files) {
                let span = self.db.span(file);

                if let Some(source_index) = sourcemap
                    .map
                    .get_sources()
                    .iter()
                    .position(|path| path == &span.path)
                {
                    sourcemap
                        .map
                        .set_source_content(source_index, &span.source)
                        .unwrap();
                }
            }

            let mut vlq = Vec::new();
            sourcemap.map.write_vlq(&mut vlq).unwrap();
            let vlq = String::from_utf8(vlq).unwrap();

            let json = serde_json::json!({
                "version": 3,
                "sources": sourcemap.map.get_sources(),
                "sourcesContent": sourcemap.map.get_sources_content(),
                "names": sourcemap.map.get_names(),
                "mappings": vlq,
            });

            let base64 =
                base64::Engine::encode(&base64::prelude::BASE64_STANDARD, json.to_string());

            write!(
                &mut self.output,
                "\n//# sourceMappingURL=data:application/json;base64,{base64}"
            )
            .unwrap();
        }

        colored::control::unset_override();

        Ok(self.output)
    }
}
