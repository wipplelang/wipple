//! The Wipple CLI.

#![allow(clippy::print_stdout, clippy::print_stderr)]

mod lsp;

use clap::Parser;
use futures::{future, FutureExt};
use serde::Serialize;
use std::{fs, os::unix::fs::PermissionsExt, path::PathBuf, process, sync::Arc};
use wipple_driver::util::lazy_static::lazy_static;

#[derive(Parser)]
#[command(name = "Wipple", bin_name = "wipple")]
enum Args {
    Compile {
        #[clap(long = "dependency")]
        dependency_path: Option<PathBuf>,

        #[clap(long = "interface")]
        output_interface_path: Option<PathBuf>,

        #[clap(long = "library")]
        output_library_path: Option<PathBuf>,

        source_paths: Vec<PathBuf>,
    },
    Link {
        #[clap(short = 'o', long = "output")]
        output_executable_path: PathBuf,

        library_paths: Vec<PathBuf>,
    },
    Run {
        executable_path: PathBuf,
    },
    BundleForPlayground {
        #[clap(long = "dependency")]
        dependency_interface_path: Option<PathBuf>,

        #[clap(long = "link")]
        dependency_libraries_paths: Vec<PathBuf>,

        #[clap(short = 'o', long = "output")]
        output_path: PathBuf,

        source_paths: Vec<PathBuf>,
    },
    Lsp,
}

const SHEBANG: &str = "#!/usr/bin/env wipple run\n";

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    env_logger::init();

    let args = Args::parse();

    match args {
        Args::Compile {
            dependency_path,
            output_interface_path,
            output_library_path,
            source_paths,
        } => {
            let sources = source_paths
                .into_iter()
                .map(|source_path| {
                    fs::read_to_string(&source_path).map(|code| wipple_driver::File {
                        path: source_path.to_string_lossy().into_owned(),
                        visible_path: wipple_driver::util::get_visible_path(&source_path),
                        code,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            let dependencies = match dependency_path {
                Some(path) => Some(serde_json::from_str(&fs::read_to_string(path)?)?),
                None => None,
            };

            let result = wipple_driver::compile(sources, dependencies);

            if !result.diagnostics.is_empty() {
                print_diagnostics(&result.diagnostics, result.interface).await;
                process::exit(1);
            }

            if let Some(output_interface_path) = output_interface_path {
                if let Some(parent) = output_interface_path.parent() {
                    fs::create_dir_all(parent)?;
                }

                fs::write(
                    output_interface_path,
                    serde_json::to_string_pretty(&result.interface)?,
                )?;
            }

            if let Some(output_library_path) = output_library_path {
                if let Some(parent) = output_library_path.parent() {
                    fs::create_dir_all(parent)?;
                }

                fs::write(
                    output_library_path,
                    serde_json::to_string_pretty(&result.library)?,
                )?;
            }

            Ok(())
        }
        Args::Link {
            output_executable_path,
            library_paths,
        } => {
            let libraries = library_paths
                .into_iter()
                .map(|library_path| {
                    let string = fs::read_to_string(library_path)?;
                    let library = serde_json::from_str::<wipple_driver::Library>(&string)?;
                    Ok(library)
                })
                .collect::<anyhow::Result<Vec<_>>>()?;

            let executable = match wipple_driver::link(libraries) {
                Some(executable) => executable,
                None => {
                    eprintln!("linking failed");
                    process::exit(1);
                }
            };

            let output = format!("{}{}", SHEBANG, serde_json::to_string_pretty(&executable)?);

            if let Some(parent) = output_executable_path.parent() {
                fs::create_dir_all(parent)?;
            }

            fs::write(&output_executable_path, output)?;

            let mut permissions = fs::metadata(&output_executable_path)?.permissions();
            permissions.set_mode(0o755);
            fs::set_permissions(output_executable_path, permissions)?;

            Ok(())
        }
        Args::Run { executable_path } => {
            let executable = serde_json::from_str::<wipple_driver::Executable>(
                fs::read_to_string(executable_path)?
                    .strip_prefix(SHEBANG)
                    .expect("missing shebang"),
            )?;

            run_executable(executable).await;

            Ok(())
        }
        Args::BundleForPlayground {
            dependency_interface_path,
            dependency_libraries_paths,
            output_path,
            source_paths,
        } => {
            let sources = source_paths
                .into_iter()
                .map(|source_path| {
                    fs::read_to_string(&source_path).map(|code| wipple_driver::File {
                        path: source_path.to_string_lossy().into_owned(),
                        visible_path: wipple_driver::util::get_visible_path(&source_path),
                        code,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            let dependencies = match dependency_interface_path {
                Some(path) => Some(serde_json::from_str(&fs::read_to_string(path)?)?),
                None => None,
            };

            let libraries = dependency_libraries_paths
                .into_iter()
                .map(|library_path| {
                    let string = fs::read_to_string(library_path)?;
                    let library = serde_json::from_str::<wipple_driver::Library>(&string)?;
                    Ok(library)
                })
                .collect::<anyhow::Result<Vec<_>>>()?;

            let result = wipple_driver::compile(sources, dependencies);

            if !result.diagnostics.is_empty() {
                print_diagnostics(&result.diagnostics, result.interface).await;
                process::exit(1);
            }

            let output = PlaygroundBundle {
                interface: result.interface,
                libraries: libraries.into_iter().chain([result.library]).collect(),
            };

            if let Some(parent) = output_path.parent() {
                fs::create_dir_all(parent)?;
            }

            fs::write(&output_path, serde_json::to_string_pretty(&output)?)?;

            Ok(())
        }
        Args::Lsp => {
            eprintln!("Starting LSP server...");

            lsp::start().await;
            Ok(())
        }
    }
}

async fn print_diagnostics(
    diagnostics: &[wipple_driver::util::WithInfo<wipple_driver::Info, wipple_driver::Diagnostic>],
    interface: wipple_driver::Interface,
) {
    let render = wipple_render::Render::new();
    render.update(interface, Vec::new(), None).await;

    for diagnostic in diagnostics {
        if let Some(rendered_diagnostic) = render.render_diagnostic(diagnostic).await {
            eprintln!("{}", rendered_diagnostic.raw);
        }
    }
}

async fn run_executable(executable: wipple_driver::Executable) {
    #[derive(Clone)]
    struct Value;

    struct Runtime;

    lazy_static! {
        static ref RUNTIME: tokio::runtime::Runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(1)
            .enable_all()
            .build()
            .unwrap();
    }

    impl wipple_interpreter::Runtime for Runtime {
        type Value = Value;
        type JoinHandle = tokio::task::JoinHandle<()>;

        fn run(future: impl future::Future<Output = ()> + Send + 'static) -> Self::JoinHandle {
            RUNTIME.spawn(future)
        }

        async fn from_value(
            _value: wipple_interpreter::Value<Self>,
            _task: &wipple_interpreter::TaskLocals<Self>,
            _context: &wipple_interpreter::Context<Self>,
        ) -> Self::Value {
            unimplemented!()
        }

        async fn to_value(_value: Self::Value) -> wipple_interpreter::Value<Self> {
            unimplemented!()
        }

        async fn with_functions<T>(
            _f: impl FnOnce(&mut Vec<wipple_interpreter::StoredFunction<Self>>) -> T + Send,
        ) -> T {
            unimplemented!()
        }
    }

    let options = wipple_interpreter::Options::<Runtime>::with_io(wipple_interpreter::Io {
        display: Arc::new(|message| {
            async move {
                println!("{}", message);
                Ok(())
            }
            .boxed()
        }),
        prompt: Arc::new(|_, _| todo!()),
        choice: Arc::new(|_, _| todo!()),
        ui: Arc::new(|_, _| todo!()),
        sleep: Arc::new(|_| todo!()),
    })
    .with_debug(|s| {
        async move {
            if std::env::var("WIPPLE_DEBUG_INTERPRETER").is_ok() {
                eprintln!("{}\n", s);
            }
        }
        .boxed()
    });

    if let Err(error) = wipple_interpreter::evaluate(executable, options).await {
        eprintln!("error: {}", error.0);
        process::exit(1);
    }
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct PlaygroundBundle {
    interface: wipple_driver::Interface,
    libraries: Vec<wipple_driver::Library>,
}
