use crate::{
    compile::{self, Program},
    diagnostics::*,
    parser::{self, Span},
    Compiler, FilePath, Loader,
};
use petgraph::{algo::toposort, graph::NodeIndex, stable_graph::StableGraph, EdgeDirection};
use std::{collections::HashMap, sync::Arc};

type FileGraph = StableGraph<parser::File, ()>;

impl<L: Loader> Compiler<L> {
    pub fn build(&mut self, path: FilePath) -> Option<Program> {
        #[derive(Default)]
        struct Info {
            graph: FileGraph,
            cache: HashMap<FilePath, NodeIndex>,
        }

        let mut info = Info::default();

        fn load<L: Loader>(
            compiler: &mut Compiler<L>,
            path: FilePath,
            source_span: Option<Span>,
            info: &mut Info,
        ) -> Option<NodeIndex> {
            if let Some(index) = info.cache.get(&path) {
                return Some(*index);
            }

            let code = match compiler.loader.load(path) {
                Ok(code) => code,
                Err(error) => {
                    compiler.diagnostics.add(Diagnostic::error(
                        format!("cannot load file `{}`: {}", path, error),
                        source_span
                            .map(|span| Note::primary(span, "try fixing this import"))
                            .into_iter()
                            .collect(),
                    ));

                    return None;
                }
            };

            compiler
                .diagnostics
                .add_file(path, Arc::from(code.as_ref()));

            compiler.parse(path, &code).map(|file| {
                let index = info.graph.add_node(file.clone());
                info.cache.insert(path, index);

                if !matches!(path, FilePath::Prelude) {
                    let prelude_index = load(compiler, FilePath::Prelude, None, info)
                        .expect("failed to load prelude");

                    info.graph.add_edge(prelude_index, index, ());
                }

                for dependency in file.dependencies {
                    if let Some(dependency_index) = load(
                        compiler,
                        FilePath::Path(dependency.name),
                        Some(dependency.span),
                        info,
                    ) {
                        info.graph.add_edge(dependency_index, index, ());
                    }
                }

                index
            })
        }

        load(self, path, None, &mut info)?;

        let files = match toposort(&info.graph, None) {
            Ok(sorted) => sorted
                .into_iter()
                .rev()
                .map(|index| {
                    let dependencies = info
                        .graph
                        .neighbors_directed(index, EdgeDirection::Incoming)
                        .map(|index| info.graph[index].path)
                        .collect::<Vec<_>>();

                    let file = info.graph.remove_node(index).unwrap();

                    (file, dependencies)
                })
                .collect::<Vec<_>>(),
            Err(error) => {
                let file = info.graph.remove_node(error.node_id()).unwrap();

                self.diagnostics.add(Diagnostic::error(
                    format!("import cycle found: {}", file.path),
                    Vec::new(),
                ));

                return None;
            }
        };

        let mut lowered_files = Vec::new();
        let mut lowered_files_by_path = HashMap::new();
        let last_index = files.len() - 1;
        let mut success = true;
        for (index, (file, dependencies)) in files.into_iter().rev().enumerate() {
            let dependencies = dependencies
                .into_iter()
                .map(|path| {
                    // Unwrapping is OK because the dependency graph is sorted,
                    // so 'dependencies' only includes files already added to
                    // 'files_by_path'
                    &lowered_files[*lowered_files_by_path.get(&path).unwrap()]
                })
                .collect::<Vec<&compile::lower::File>>();

            let is_entrypoint = index == last_index;
            if is_entrypoint {
                for dependency in &dependencies {
                    if !dependency.block.is_empty() {
                        self.diagnostics.add(Diagnostic::error(
                            format!("cannot import `{}`", dependency.path),
                            vec![Note::primary(
                                dependency.span,
                                "dependencies must not contain executable statements",
                            )],
                        ));

                        success = false;
                    }
                }
            }

            let file = self.lower(file, dependencies);
            success &= file.complete;

            let index = lowered_files.len();
            lowered_files_by_path.insert(file.path, index);
            lowered_files.push(file);
        }

        success.then(|| self.typecheck(lowered_files)).flatten()
    }
}
