use colored::Colorize;
use std::path::PathBuf;
use structopt::StructOpt;
use wipple_projects::new_project;

/// Create a new Wipple project
#[derive(StructOpt)]
pub struct New {
    /// Path to the project folder
    pub path: PathBuf,
}

impl New {
    pub fn run(&self) -> Result<(), String> {
        if self.path.exists() {
            return Err(format!(
                "Error: '{}' already exists",
                self.path.to_string_lossy()
            ));
        }

        new_project(&self.path).map_err(|e| e.to_string())?;

        println!(
            "{}",
            &format!("Created new project at {}", self.path.to_string_lossy()).green()
        );

        Ok(())
    }
}
