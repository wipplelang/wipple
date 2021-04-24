use std::path::{Path, PathBuf};
use structopt::StructOpt;
use zip::ZipWriter;
use zip_extensions::write::ZipWriterExtensions;

const TARGET: &str = env!("TARGET");

/// Bundle a Wipple project into an executable
#[derive(StructOpt)]
pub struct Bundle {
    /// URL of interpreter to use
    #[structopt(long = "interpreter")]
    pub interpreter_url: Option<String>,

    /// Path of interpreter to use
    #[structopt(long = "interpreter-path")]
    pub interpreter_path: Option<PathBuf>,

    /// Path to project, defaults to current directory
    #[structopt(long = "project")]
    pub project_path: Option<PathBuf>,

    /// Name of the output executable
    #[structopt(long = "bin")]
    pub output_path: PathBuf,
}

impl Bundle {
    pub fn run(&self) -> Result<(), String> {
        let path = self
            .project_path
            .clone()
            .unwrap_or_else(|| std::env::current_dir().unwrap());

        if !path.exists() {
            return Err(format!(
                "Error: '{}' does not exist",
                path.to_string_lossy()
            ));
        }

        println!("Preparing project for bundling");

        // SAFETY: This is the only location where this value is set, and it
        // always happens before it is read in 'wipple_projects::load_project'
        unsafe { wipple_projects::IS_BUNDLING = true };

        (|| -> wipple::Result<()> {
            let stack = wipple_bundled_interpreter::setup()?;
            wipple_projects::load_project(&path.join("project.wpl"), &stack)?;
            Ok(())
        })()
        .map_err(|e| {
            format!(
                "Error preparing project: {}",
                e.into_error(&wipple::Stack::new())
            )
        })?;

        let mut zip_data = Vec::new();

        let mut zip = ZipWriter::new(std::io::Cursor::new(&mut zip_data));
        zip.create_from_directory(&path)
            .map_err(|e| format!("Error: Could not bundle project: {}", e))?;

        drop(zip);

        let mut interpreter = if let Some(path) = &self.interpreter_path {
            println!(
                "Using locally-installed interpreter at '{}'",
                path.to_string_lossy()
            );

            std::fs::read(path).map_err(|e| format!("Error: Could not load interpreter: {}", e))?
        } else {
            let interpreter_url = self.interpreter_url.clone().unwrap_or_else(|| {
                format!(
                    "https://github.com/wipplelang/wipple/releases/latest/download/wipple-bundled-{}",
                    TARGET
                )
            });

            println!("Downloading interpreter from '{}'", interpreter_url);

            download_url(&interpreter_url)
                .map_err(|e| format!("Error: Could not download interpreter: {}", e))?
        };

        println!("Building executable");

        // We can just append the zip data to the end of the executable because
        // executables are loaded from the beginning and zip files are loaded
        // from the end
        interpreter.append(&mut zip_data);

        std::fs::write(&self.output_path, interpreter)
            .map_err(|e| format!("Error: Could not write executable to disk: {}", e))?;

        let ok = chmod(&self.output_path, 0o755);
        if !ok {
            return Err("Error: Could not set mode of executable".to_string());
        }

        Ok(())
    }
}

fn download_url(url: &str) -> Result<Vec<u8>, String> {
    let client = reqwest::blocking::Client::new();
    let response = client.get(url).send().map_err(|e| format!("{}", e))?;

    if !response.status().is_success() {
        return Err("Server sent error response".to_string());
    }

    let data = response.bytes().map_err(|e| format!("{}", e))?;
    Ok(data.to_vec())
}

fn chmod(path: &Path, mode: libc::mode_t) -> bool {
    let path = std::ffi::CString::new(path.to_str().unwrap()).unwrap();
    let result = unsafe { libc::chmod(path.as_ptr(), mode) };
    result == 0
}
