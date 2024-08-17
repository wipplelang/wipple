//! The Wipple CLI.

#![allow(clippy::print_stdout, clippy::print_stderr)]

mod doc;
mod lsp;

use clap::Parser;
use futures::{future, FutureExt};
use serde::{de::DeserializeOwned, Serialize};
use std::{
    fs, io,
    path::{Path, PathBuf},
    process,
    sync::Arc,
};
use wipple_driver::util::lazy_static::lazy_static;

#[derive(Parser)]
#[command(name = "Wipple", bin_name = "wipple")]
enum Args {
    Compile {
        #[clap(long = "dependency")]
        dependency_paths: Vec<PathBuf>,

        #[clap(long = "interface")]
        output_interface_path: Option<PathBuf>,

        #[clap(long = "library")]
        output_library_path: Option<PathBuf>,

        #[clap(long = "release")]
        release: bool,

        source_paths: Vec<PathBuf>,
    },
    Link {
        #[clap(short = 'o', long = "output")]
        output_executable_path: PathBuf,

        #[clap(long = "release")]
        release: bool,

        library_paths: Vec<PathBuf>,
    },
    Run {
        executable_path: PathBuf,
    },
    BundleForPlayground {
        #[clap(long = "dependency")]
        dependency_interfaces_paths: Vec<PathBuf>,

        #[clap(long = "link")]
        dependency_libraries_paths: Vec<PathBuf>,

        #[clap(short = 'o', long = "output")]
        output_path: PathBuf,

        source_paths: Vec<PathBuf>,
    },
    Format {
        source_paths: Vec<PathBuf>,
    },
    Lsp,
    Doc {
        #[clap(
            long = "template-url",
            default_value = "https://wipple.dev/doc-template.html"
        )]
        template_url: String,

        #[clap(long = "template-path")]
        template_path: Option<PathBuf>,

        #[clap(long = "json")]
        json: bool,

        #[clap(long = "title")]
        title: String,

        #[clap(long = "dependency")]
        dependency_interfaces_paths: Vec<PathBuf>,

        #[clap(short = 'o', long = "output")]
        output_path: PathBuf,

        interface_path: PathBuf,
    },
}

fn read_binary<T: DeserializeOwned>(path: impl AsRef<Path>) -> anyhow::Result<T> {
    wipple_driver::util::read_binary(io::BufReader::new(fs::File::open(path)?))
}

fn write_binary(
    path: impl AsRef<Path>,
    value: &impl Serialize,
    release: bool,
) -> anyhow::Result<()> {
    let compression_level = if release {
        wipple_driver::util::CompressionLevel::Max
    } else {
        wipple_driver::util::CompressionLevel::Min
    };

    wipple_driver::util::write_binary(
        io::BufWriter::new(fs::File::create(&path)?),
        value,
        compression_level,
    )
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    env_logger::init();

    let args = Args::parse();

    match args {
        Args::Compile {
            dependency_paths,
            output_interface_path,
            output_library_path,
            release,
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

            let dependencies = dependency_paths
                .into_iter()
                .map(read_binary)
                .collect::<anyhow::Result<Vec<_>>>()?;

            let result = wipple_driver::compile(sources, dependencies.clone());

            if !result.diagnostics.is_empty() {
                let contains_errors =
                    print_diagnostics(&result.diagnostics, dependencies, result.interface.clone());

                if contains_errors {
                    process::exit(1);
                }
            }

            if let Some(output_interface_path) = output_interface_path {
                if let Some(parent) = output_interface_path.parent() {
                    fs::create_dir_all(parent)?;
                }

                write_binary(output_interface_path, &result.interface, release)?;
            }

            if let Some(output_library_path) = output_library_path {
                if let Some(parent) = output_library_path.parent() {
                    fs::create_dir_all(parent)?;
                }

                write_binary(output_library_path, &result.library, release)?;
            }

            Ok(())
        }
        Args::Link {
            output_executable_path,
            release,
            library_paths,
        } => {
            let libraries = library_paths
                .into_iter()
                .map(read_binary)
                .collect::<anyhow::Result<Vec<wipple_driver::Library>>>()?;

            let executable = match wipple_driver::link(libraries) {
                Some(executable) => executable,
                None => {
                    eprintln!("linking failed");
                    process::exit(1);
                }
            };

            if let Some(parent) = output_executable_path.parent() {
                fs::create_dir_all(parent)?;
            }

            write_binary(&output_executable_path, &executable, release)?;

            Ok(())
        }
        Args::Run { executable_path } => {
            let executable = read_binary::<wipple_driver::Executable>(executable_path)?;
            run_executable(executable).await;

            Ok(())
        }
        Args::BundleForPlayground {
            dependency_interfaces_paths,
            dependency_libraries_paths,
            output_path,
            source_paths,
        } => {
            let release = true;

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

            let dependencies = dependency_interfaces_paths
                .into_iter()
                .map(read_binary)
                .collect::<anyhow::Result<Vec<_>>>()?;

            let libraries = dependency_libraries_paths
                .into_iter()
                .map(read_binary)
                .collect::<anyhow::Result<Vec<wipple_driver::Library>>>()?;

            let result = wipple_driver::compile(sources, dependencies.clone());

            if !result.diagnostics.is_empty() {
                let contains_error = print_diagnostics(
                    &result.diagnostics,
                    dependencies.clone(),
                    result.interface.clone(),
                );

                if contains_error {
                    process::exit(1);
                }
            }

            let output = PlaygroundBundle {
                interfaces: dependencies.into_iter().chain([result.interface]).collect(),
                libraries: libraries.into_iter().chain([result.library]).collect(),
            };

            if let Some(parent) = output_path.parent() {
                fs::create_dir_all(parent)?;
            }

            write_binary(&output_path, &output, release)?;

            Ok(())
        }
        Args::Format { source_paths } => {
            for path in source_paths {
                if let Err(error) = fs::read_to_string(&path)
                    .map(|code| wipple_driver::format(&code) + "\n")
                    .and_then(|code| fs::write(&path, code))
                {
                    eprintln!("could not format {}: {}", path.to_string_lossy(), error);
                } else {
                    eprintln!("formatted {}", path.to_string_lossy());
                }
            }

            Ok(())
        }
        Args::Lsp => {
            eprintln!("Starting LSP server...");

            lsp::start().await;
            Ok(())
        }
        Args::Doc {
            template_url,
            template_path,
            json,
            title,
            dependency_interfaces_paths,
            output_path,
            interface_path,
        } => {
            let interface = read_binary(interface_path)?;

            let dependencies = dependency_interfaces_paths
                .into_iter()
                .map(read_binary)
                .collect::<anyhow::Result<Vec<_>>>()?;

            let output = if json {
                eprintln!("Generating documentation...");
                serde_json::to_string_pretty(&doc::json(&title, interface, dependencies))?
            } else {
                let template = match template_path {
                    Some(path) => fs::read_to_string(path)?,
                    None => {
                        eprintln!("Fetching template...");
                        reqwest::get(&template_url).await?.text().await?
                    }
                };

                eprintln!("Generating documentation...");
                doc::html(&title, interface, dependencies, &template)?
            };

            if let Some(parent) = output_path.parent() {
                fs::create_dir_all(parent)?;
            }

            fs::write(&output_path, output)?;

            eprintln!("Documentation written to {}", output_path.display());

            Ok(())
        }
    }
}

#[must_use]
fn print_diagnostics(
    diagnostics: &[wipple_driver::util::WithInfo<wipple_driver::Info, wipple_driver::Diagnostic>],
    dependencies: Vec<wipple_driver::Interface>,
    interface: wipple_driver::Interface,
) -> bool {
    let render = wipple_render::Render::new();
    render.update(
        dependencies.into_iter().chain([interface]).collect(),
        None,
        None,
    );

    let mut contains_error = false;
    for diagnostic in diagnostics {
        if let Some(rendered_diagnostic) = render.render_diagnostic(diagnostic) {
            if matches!(
                rendered_diagnostic.severity,
                wipple_render::RenderedDiagnosticSeverity::Error
            ) {
                contains_error = true;
            }

            eprintln!("{}", rendered_diagnostic.raw);
        }
    }

    contains_error
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

    let mut options = wipple_interpreter::Options::<Runtime>::with_io(wipple_interpreter::Io {
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
    });

    if std::env::var("WIPPLE_DEBUG_INTERPRETER").is_ok() {
        options = options.with_debug(|s| {
            async move {
                if std::env::var("WIPPLE_DEBUG_INTERPRETER").is_ok() {
                    eprintln!("{}\n", s);
                }
            }
            .boxed()
        });
    }

    if let Err(error) = wipple_interpreter::evaluate(executable, options).await {
        eprintln!("error: {}", error.0);
        process::exit(1);
    }
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct PlaygroundBundle {
    interfaces: Vec<wipple_driver::Interface>,
    libraries: Vec<wipple_driver::Library>,
}
