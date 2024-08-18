#![allow(missing_docs)]

mod util;

use anyhow::Context;
use clap::Parser;
use indicatif::{ProgressBar, ProgressStyle};
use serde::Serialize;
use std::{
    borrow::Cow,
    io,
    path::{Path, PathBuf},
    process::Command,
};
use url::Url;
use util::{dependency_name, glob, BuildDir, RunExt};
use which::which;

static BASE_INTERFACE: &[u8] = include_bytes!("../../artifacts/base.wippleinterface");
static BASE_LIBRARY: &[u8] = include_bytes!("../../artifacts/base.wipplelibrary");
static PROJECT_INTERFACE: &[u8] = include_bytes!("../../artifacts/project.wippleinterface");
static PROJECT_LIBRARY: &[u8] = include_bytes!("../../artifacts/project.wipplelibrary");

#[derive(Parser)]
struct Options {
    /// Path to 'wipplec'; defaults to searching for it in your PATH
    #[clap(long = "compiler")]
    compiler_path: Option<PathBuf>,

    /// Path to the build directory; defaults to a temporary directory
    #[clap(long = "build-dir")]
    build_dir: Option<PathBuf>,

    /// Path to the project directory; defaults to the current directory
    project_path: Option<PathBuf>,
}

#[derive(Parser)]
enum Args {
    /// Compile a Wipple project and retrieve its build configuration
    Config {
        #[clap(flatten)]
        options: Options,
    },

    /// Compile a Wipple project into an executable
    Build {
        #[clap(flatten)]
        options: Options,

        /// Where Wipple should write the executable
        #[clap(long = "output")]
        output: PathBuf,
    },

    /// Compile and run a Wipple project
    Run {
        #[clap(flatten)]
        options: Options,
    },
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let progress = ProgressBar::new(25).with_style(
        ProgressStyle::with_template("[{bar:15}] {msg}")
            .unwrap()
            .progress_chars("=> "),
    );

    progress.set_message("Initializing");

    match args {
        Args::Config { options } => {
            let build_dir = BuildDir::from_options(&options)?;
            let build_result = build(&options, &build_dir, &progress)?;
            progress.finish_and_clear();

            #[derive(Default, Serialize)]
            #[serde(rename_all = "camelCase")]
            struct Configuration {
                project_dependencies: Vec<PathBuf>,
                project_file: PathBuf,
                source_dependencies: Vec<PathBuf>,
                source_files: Vec<PathBuf>,
            }

            let configuration = Configuration {
                project_dependencies: vec![
                    build_dir.join("_base.wippleinterface"),
                    build_dir.join("_project.wippleinterface"),
                ],
                project_file: build_result.project_file,
                source_dependencies: build_result.main_compile_options.dependencies,
                source_files: build_result.main_compile_options.sources,
            };

            serde_json::to_writer_pretty(io::stdout(), &configuration)?;
            println!();
        }
        Args::Build { options, output } => {
            let build_dir = BuildDir::from_options(&options)?;
            let build_result = build(&options, &build_dir, &progress)?;
            progress.finish_and_clear();

            std::fs::copy(&build_result.binary_path, &output)
                .with_context(|| "copying executable to output path")?;
        }
        Args::Run { options } => {
            let build_dir = BuildDir::from_options(&options)?;
            let build_result = build(&options, &build_dir, &progress)?;
            progress.finish_and_clear();

            wipplec(
                options.compiler_path.as_deref(),
                WipplecOptions::Run(WipplecRunOptions {
                    executable: build_result.binary_path,
                }),
            )
            .and_then(|mut command| command.spawn()?.run())
            .with_context(|| "running compiled executable")?;
        }
    }

    Ok(())
}

struct BuildResult {
    project_file: PathBuf,
    main_compile_options: WipplecCompileOptions,
    binary_path: PathBuf,
}

fn build(
    options: &Options,
    build_dir: &Path,
    progress: &ProgressBar,
) -> anyhow::Result<BuildResult> {
    let project_path = match options.project_path.as_deref() {
        Some(path) => Cow::Borrowed(path),
        None => Cow::Owned(std::env::current_dir().with_context(|| "resolving current directory")?),
    };

    let binary_path = build_dir.join("_main.wipplebinary");

    // Copy built-in libraries

    std::fs::write(build_dir.join("_base.wippleinterface"), BASE_INTERFACE)
        .with_context(|| "copying built-in libraries")?;

    std::fs::write(build_dir.join("_base.wipplelibrary"), BASE_LIBRARY)
        .with_context(|| "copying built-in libraries")?;

    std::fs::write(
        build_dir.join("_project.wippleinterface"),
        PROJECT_INTERFACE,
    )
    .with_context(|| "copying built-in libraries")?;

    std::fs::write(build_dir.join("_project.wipplelibrary"), PROJECT_LIBRARY)
        .with_context(|| "copying built-in libraries")?;

    // Build project

    let mut queue = Vec::new();
    let main_project_result = build_project(
        "_main",
        &project_path,
        options.compiler_path.as_deref(),
        build_dir,
        &mut queue,
        progress,
    )?;

    let main_compile_options = queue.last().unwrap().1.clone();

    progress.set_length(queue.len() as u64);
    for (project_path, compile_options) in queue {
        progress.set_message(format!("Building {}", project_path.display()));

        wipplec(
            options.compiler_path.as_deref(),
            WipplecOptions::Compile(compile_options),
        )
        .and_then(|mut command| command.spawn()?.run())
        .with_context(|| format!("compiling dependency {}", project_path.display()))?;

        progress.inc(1);
    }

    // Link libraries

    progress.set_message("Linking");

    let libraries = glob([build_dir.join("*.wipplelibrary")])?;

    wipplec(
        options.compiler_path.as_deref(),
        WipplecOptions::Link(WipplecLinkOptions {
            libraries,
            output_executable: binary_path.clone(),
        }),
    )
    .and_then(|mut command| command.spawn()?.run())
    .with_context(|| "linking libraries")?;

    Ok(BuildResult {
        project_file: main_project_result.project_file,
        main_compile_options,
        binary_path,
    })
}

struct BuildProjectResult {
    project_file: PathBuf,
}

fn build_project(
    name: &str,
    project_path: &Path,
    compiler_path: Option<&Path>,
    build_dir: &Path,
    queue: &mut Vec<(PathBuf, WipplecCompileOptions)>,
    progress: &ProgressBar,
) -> anyhow::Result<BuildProjectResult> {
    progress.set_message(format!("Scanning project {}", project_path.display()));

    let mut project = Project {
        path: project_path
            .canonicalize()
            .with_context(|| format!("resolving project {}", project_path.display()))?,
        include: Vec::new(),
        dependencies: Vec::new(),
    };

    let project_file = project
        .path
        .join("project.wipple")
        .canonicalize()
        .with_context(|| format!("resolving project.wipple in {}", project.path.display()))?;

    wipplec(
        compiler_path,
        WipplecOptions::Compile(WipplecCompileOptions {
            sources: vec![project_file.clone()],
            dependencies: vec![
                build_dir.join("_base.wippleinterface"),
                build_dir.join("_project.wippleinterface"),
            ],
            output_interface: None,
            output_library: Some(build_dir.join(format!("_projects/{name}.wipplelibrary"))),
        }),
    )
    .and_then(|mut command| command.spawn()?.run())
    .with_context(|| format!("compiling {}", project_file.display()))?;

    wipplec(
        compiler_path,
        WipplecOptions::Link(WipplecLinkOptions {
            libraries: vec![
                build_dir.join(format!("_projects/{name}.wipplelibrary")),
                build_dir.join("_base.wipplelibrary"),
                build_dir.join("_project.wipplelibrary"),
            ],
            output_executable: build_dir.join(format!("_projects/{name}.wipplebinary")),
        }),
    )
    .and_then(|mut command| command.spawn()?.run())
    .with_context(|| format!("linking {}", project_file.display()))?;

    let output = wipplec(
        compiler_path,
        WipplecOptions::Run(WipplecRunOptions {
            executable: build_dir.join(format!("_projects/{name}.wipplebinary")),
        }),
    )
    .and_then(|mut command| Ok(command.output()?))
    .with_context(|| format!("evaluating {}", project_file.display()))?;

    if !output.status.success() {
        anyhow::bail!("failed to evaluate {}", project_file.display());
    }

    for line in String::from_utf8(output.stdout)
        .with_context(|| format!("parsing output of {}", project_file.display()))?
        .lines()
    {
        parse_project_config(&mut project, line);
    }

    let sources = glob(project.include.iter().flat_map(|path| {
        let path = project.path.join(path);

        if path.is_file() {
            path.extension()
                .is_some_and(|ext| ext == "wipple")
                .then_some(path)
        } else {
            Some(path.join("**/*.wipple"))
        }
    }))
    .with_context(|| format!("resolving source files in {}", project.path.display()))?;

    // Make sure the hashes of dependencies are stable by resolving paths. This
    // must be done before calling `dependency_name`
    for dependency in &mut project.dependencies {
        match dependency {
            Dependency::Git { .. } => {
                // Keep as-is
            }
            Dependency::Local { path } => {
                let full_path = project.path.join(path.as_path());

                *path = full_path.canonicalize().with_context(|| {
                    format!("resolving local dependency {}", full_path.display())
                })?;
            }
        }
    }

    for dependency in &project.dependencies {
        let dependency_name = dependency_name(dependency);

        match dependency {
            Dependency::Git { .. } => {
                anyhow::bail!("git dependencies are not yet supported");
            }
            Dependency::Local { path } => {
                build_project(
                    &dependency_name,
                    path,
                    compiler_path,
                    build_dir,
                    queue,
                    progress,
                )?;
            }
        }
    }

    let dependencies = project
        .dependencies
        .into_iter()
        .map(|dependency| {
            build_dir.join(format!("{}.wippleinterface", dependency_name(&dependency)))
        })
        .chain([build_dir.join("_base.wippleinterface")])
        .collect::<Vec<_>>();

    if queue.iter().all(|(path, _)| path != &project.path) {
        queue.push((
            project.path,
            WipplecCompileOptions {
                sources,
                dependencies,
                output_interface: Some(build_dir.join(format!("{name}.wippleinterface"))),
                output_library: Some(build_dir.join(format!("{name}.wipplelibrary"))),
            },
        ));
    }

    Ok(BuildProjectResult { project_file })
}

#[derive(Debug, Clone)]
struct Project {
    path: PathBuf,
    include: Vec<PathBuf>,
    dependencies: Vec<Dependency>,
}

#[derive(Debug, Clone, Hash)]
enum Dependency {
    Git {
        url: Url,
        r#ref: String,
        dir: Option<String>,
    },
    Local {
        path: PathBuf,
    },
}

fn parse_project_config(project: &mut Project, line: &str) {
    if let Some(path) = line.strip_prefix("project:include:") {
        project.include.push(path.into());
    } else if let Some(dep) = line.strip_prefix("project:dependency:") {
        if let Some(git) = dep.strip_prefix("git:") {
            if let Some((url, r#ref)) = git.split_once(' ') {
                if let Ok(url) = url.parse() {
                    project.dependencies.push(Dependency::Git {
                        url,
                        r#ref: String::from(r#ref),
                        dir: None,
                    });
                }
            }
        } else if let Some(git_dir) = dep.strip_prefix("git-dir:") {
            if let Some((url, git_dir)) = git_dir.split_once(' ') {
                if let Some((r#ref, dir)) = git_dir.split_once(' ') {
                    if let Ok(url) = url.parse() {
                        project.dependencies.push(Dependency::Git {
                            url,
                            r#ref: String::from(r#ref),
                            dir: Some(String::from(dir)),
                        });
                    }
                }
            }
        } else if let Some(path) = dep.strip_prefix("local:") {
            project
                .dependencies
                .push(Dependency::Local { path: path.into() });
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum WipplecOptions {
    Compile(WipplecCompileOptions),
    Link(WipplecLinkOptions),
    Run(WipplecRunOptions),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct WipplecCompileOptions {
    sources: Vec<PathBuf>,
    dependencies: Vec<PathBuf>,
    output_interface: Option<PathBuf>,
    output_library: Option<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct WipplecLinkOptions {
    libraries: Vec<PathBuf>,
    output_executable: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct WipplecRunOptions {
    executable: PathBuf,
}

fn wipplec(path: Option<&Path>, options: WipplecOptions) -> anyhow::Result<Command> {
    let wipplec = path
        .map(|path| Ok(path.to_path_buf()))
        .unwrap_or_else(|| which("wipplec").with_context(|| "searching for 'wipplec' in PATH"))?;

    let path_str = |path: &Path| path.to_string_lossy().to_string();

    let mut args = Vec::new();
    match options {
        WipplecOptions::Compile(options) => {
            args.push(String::from("compile"));

            for path in options.sources {
                args.push(path_str(&path));
            }

            for path in options.dependencies {
                args.push(String::from("--dependency"));
                args.push(path_str(&path));
            }

            if let Some(path) = options.output_interface {
                args.push(String::from("--interface"));
                args.push(path_str(&path));
            }

            if let Some(path) = options.output_library {
                args.push(String::from("--library"));
                args.push(path_str(&path));
            }
        }
        WipplecOptions::Link(options) => {
            args.push(String::from("link"));

            for path in options.libraries {
                args.push(path_str(&path));
            }

            args.push(String::from("--output"));
            args.push(path_str(&options.output_executable));
        }
        WipplecOptions::Run(options) => {
            args.push(String::from("run"));
            args.push(path_str(&options.executable));
        }
    }

    let mut command = Command::new(wipplec);
    command.args(args);

    Ok(command)
}
