use anyhow::Context;
use clap::Parser;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label, Severity},
    files::SimpleFiles,
    term::{
        self, Config,
        termcolor::{ColorChoice, StandardStream},
    },
};
use indicatif::{ProgressBar, ProgressStyle};
use std::{
    io::Write,
    path::{Path, PathBuf},
    time::Duration,
};
use wipple_api::{
    routes::{
        Handle, InputMetadata,
        compile::{CompileRequest, CompileResponse},
    },
    wipple_compiler::{
        codegen,
        render::{RenderedDiagnostic, RenderedDiagnosticSeverity},
    },
};

#[derive(Parser)]
enum Args {
    Compile {
        path: PathBuf,

        #[clap(short, long)]
        output: PathBuf,
    },
    Run {
        path: PathBuf,

        #[clap(long, default_value = "node")]
        runtime: String,
    },
}

const PROGRESS_TICKS: &[&str] = &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

const RUNTIME: &str = include_str!("../../runtime/runtime.js");
const NODE_PRELUDE: &str = include_str!("../../runtime/node-prelude.js");

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let progress = ProgressBar::new_spinner();
    progress.set_style(ProgressStyle::default_spinner().tick_strings(PROGRESS_TICKS));
    progress.enable_steady_tick(Duration::from_millis(80));

    let args = Args::parse();

    match args {
        Args::Compile { path, output } => {
            let code = std::fs::read_to_string(&path)
                .with_context(|| format!("could not read {}", path.display()))?;

            progress.set_message(format!("Compiling {}", path.display()));

            let response = compile(&path, &code).await?;

            progress.finish_and_clear();

            match response {
                CompileResponse::Success { executable } => {
                    let mut output = std::fs::File::options()
                        .create(true)
                        .write(true)
                        .truncate(true)
                        .open(&output)
                        .with_context(|| format!("could not open {}", output.display()))?;

                    output.write_all(executable.as_bytes())?;
                }
                CompileResponse::Failure { diagnostics } => {
                    print_diagnostics(&path, &code, diagnostics)?;
                    return Err(anyhow::anyhow!("could not compile {}", path.display()));
                }
            }
        }
        Args::Run { path, runtime } => {
            let code = std::fs::read_to_string(&path)
                .with_context(|| format!("could not read {}", path.display()))?;

            progress.set_message(format!("Compiling {}", path.display()));

            let response = compile(&path, &code).await?;

            progress.finish_and_clear();

            match response {
                CompileResponse::Success { executable } => {
                    let status = std::process::Command::new(&runtime)
                        .arg("-e")
                        .arg(executable)
                        .status()
                        .with_context(|| format!("could not launch runtime '{runtime}'"))?;

                    if !status.success() {
                        return Err(anyhow::anyhow!("'{runtime}' exited with status {status}"));
                    }
                }
                CompileResponse::Failure { diagnostics } => {
                    print_diagnostics(&path, &code, diagnostics)?;
                    return Err(anyhow::anyhow!("could not compile {}", path.display()));
                }
            }
        }
    }

    Ok(())
}

async fn compile(path: &Path, code: &str) -> anyhow::Result<CompileResponse> {
    let req = CompileRequest {
        metadata: InputMetadata {
            library: Some(String::from("foundation")),
        },
        code: code.to_string(),
        js_options: codegen::js::Options {
            mode: codegen::js::Mode::Iife(String::from("buildRuntime(env)")),
            debug: true,
        },
    };

    let mut response = req
        .response()
        .await
        .with_context(|| format!("could not compile {}", path.display()))?;

    if let CompileResponse::Success { executable } = &mut response {
        let mut bundled = String::new();
        bundled.push_str(NODE_PRELUDE);
        bundled.push_str(RUNTIME);
        bundled.push_str(executable);
        *executable = bundled;
    }

    Ok(response)
}

fn print_diagnostics(
    path: &Path,
    code: &str,
    diagnostics: impl IntoIterator<Item = RenderedDiagnostic>,
) -> anyhow::Result<()> {
    let mut files = SimpleFiles::new();
    let file = files.add(path.display().to_string(), code);

    let mut writer = StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();

    for diagnostic in diagnostics {
        let start = diagnostic.location.start.index as usize;
        let end = diagnostic.location.end.index as usize;

        let severity = match diagnostic.severity {
            RenderedDiagnosticSeverity::Warning => Severity::Warning,
            RenderedDiagnosticSeverity::Error => Severity::Error,
        };

        let mut emitted_diagnostic = Diagnostic::new(severity);

        emitted_diagnostic = emitted_diagnostic.with_labels(vec![
            Label::primary(file, start..end).with_message(diagnostic.message),
        ]);

        if let Some(description) = diagnostic.description {
            emitted_diagnostic = emitted_diagnostic.with_notes(vec![description]);
        }

        term::emit(&mut writer, &config, &files, &emitted_diagnostic)?;
    }

    Ok(())
}
