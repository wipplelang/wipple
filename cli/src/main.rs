use anyhow::{Context, anyhow};
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
use std::{collections::HashMap, io::Write, path::PathBuf, time::Duration};
use wipple_api::{
    routes::{
        Handle, InputMetadata,
        compile::{CompileRequest, CompileRequestInput, CompileResponse},
    },
    wipple_compiler::{
        File, codegen,
        render::{RenderedDiagnostic, RenderedDiagnosticSeverity},
    },
};

#[derive(Parser)]
enum Args {
    Compile {
        inputs: Vec<PathBuf>,

        #[clap(short, long)]
        output: Option<PathBuf>,
    },
    Run {
        inputs: Vec<PathBuf>,

        #[clap(long, default_value = "node")]
        runtime: String,
    },
}

const PROGRESS_TICKS: &[&str] = &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

const RUNTIME: &str = include_str!("../../runtime/runtime.js");
const NODE_PRELUDE: &str = include_str!("../../runtime/node-prelude.js");

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let progress = ProgressBar::new_spinner();
    progress.set_style(ProgressStyle::default_spinner().tick_strings(PROGRESS_TICKS));
    progress.enable_steady_tick(Duration::from_millis(80));

    match args {
        Args::Compile { inputs, output } => {
            let files = read_inputs(&inputs)?;

            progress.set_message("Compiling");
            let response = compile(files.iter().cloned()).await?;
            progress.finish_and_clear();

            match response {
                CompileResponse::Success { executable } => {
                    if let Some(output) = output {
                        let mut output = std::fs::File::options()
                            .create(true)
                            .write(true)
                            .truncate(true)
                            .open(&output)
                            .with_context(|| format!("could not open {}", output.display()))?;

                        output.write_all(executable.as_bytes())?;
                    }
                }
                CompileResponse::Failure { diagnostics } => {
                    print_diagnostics(&files, diagnostics)?;
                    return Err(anyhow!("could not compile"));
                }
            }
        }
        Args::Run { inputs, runtime } => {
            let files = read_inputs(&inputs)?;

            progress.set_message("Compiling");
            let response = compile(files.iter().cloned()).await?;
            progress.finish_and_clear();

            match response {
                CompileResponse::Success { executable } => {
                    let status = std::process::Command::new(&runtime)
                        .arg("-e")
                        .arg(executable)
                        .status()
                        .with_context(|| format!("could not launch runtime '{runtime}'"))?;

                    if !status.success() {
                        return Err(anyhow!("'{runtime}' exited with status {status}"));
                    }
                }
                CompileResponse::Failure { diagnostics } => {
                    print_diagnostics(&files, diagnostics)?;
                    return Err(anyhow!("could not compile"));
                }
            }
        }
    }

    Ok(())
}

fn read_inputs(inputs: &[PathBuf]) -> anyhow::Result<Vec<File>> {
    inputs
        .iter()
        .map(|path| {
            let code = std::fs::read_to_string(path)
                .with_context(|| format!("could not read {}", path.display()))?;

            Ok(File {
                path: path.display().to_string(),
                code,
            })
        })
        .collect()
}

async fn compile(files: impl IntoIterator<Item = File>) -> anyhow::Result<CompileResponse> {
    let req = CompileRequest {
        metadata: InputMetadata {
            library: Some(String::from("foundation")),
        },
        input: CompileRequestInput::Files(Vec::from_iter(files)),
        js_options: codegen::js::Options {
            mode: codegen::js::Mode::Iife(String::from("buildRuntime(env)")),
            debug: true,
        },
    };

    let mut response = req
        .response()
        .await
        .with_context(|| "internal compiler error")?;

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
    files: &[File],
    diagnostics: impl IntoIterator<Item = RenderedDiagnostic>,
) -> anyhow::Result<()> {
    let mut indices = HashMap::new();
    let files = files.iter().fold(SimpleFiles::new(), |mut files, file| {
        let index = files.add(file.path.as_str(), file.code.as_str());
        indices.insert(file.path.as_str(), index);
        files
    });

    let mut writer = StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();

    for diagnostic in diagnostics {
        let Some(&file) = indices.get(diagnostic.location.path.as_str()) else {
            continue;
        };

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
