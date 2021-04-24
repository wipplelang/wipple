use std::{
    collections::hash_map::DefaultHasher,
    collections::HashMap,
    hash::{Hash, Hasher},
    io::Write,
    path::{Path, PathBuf},
};
use wipple::{dynamic, primitive};

pub fn update_dependencies(
    dependencies: HashMap<String, Dependency>,
    install_dir: Option<&Path>,
    on_install: impl Fn(),
) -> Result<HashMap<String, PathBuf>, Box<dyn std::error::Error>> {
    let mut result = HashMap::new();

    let mut already_downloading = false;

    for (name, dependency) in dependencies {
        let path = dependency.update(install_dir, || {
            if already_downloading {
                return;
            }

            on_install();
            already_downloading = true;
        })?;

        result.insert(name.clone(), path);
    }

    Ok(result)
}

#[derive(Debug, Clone, Hash)]
pub enum DependencyType {
    /// Location points to a .wpl project file
    Project,

    /// Location points to a .wplplugin file
    Plugin,
}

#[derive(dynamic::TypeInfo, Debug, Clone, Hash)]
pub struct Dependency {
    pub r#type: DependencyType,
    pub location: DependencyLocation,
}

primitive!(dependency for Dependency);

impl Dependency {
    pub fn project(location: DependencyLocation) -> Self {
        Dependency {
            r#type: DependencyType::Project,
            location,
        }
    }

    pub fn plugin(location: DependencyLocation) -> Self {
        Dependency {
            r#type: DependencyType::Plugin,
            location,
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub enum DependencyLocation {
    Path(PathBuf),
    Url(String),
    Git {
        location: String,
        branch: Option<String>,
    },
}

impl Dependency {
    pub fn update(
        &self,
        install_dir: Option<&Path>,
        on_install: impl FnOnce(),
    ) -> Result<PathBuf, Box<dyn std::error::Error>> {
        use DependencyLocation::*;

        let dir = match install_dir {
            Some(install_dir) => {
                let dir = self.location.cache_dir(install_dir, true);

                if dir.exists() {
                    std::fs::remove_dir_all(&dir)?;
                }

                dir
            }
            None => {
                let cache_dir = dirs::cache_dir()
                    .expect("Could not resolve cache directory")
                    .join("wipple");

                let dir = self.location.cache_dir(&cache_dir, false);

                // TODO: Option to ignore cache
                if dir.exists() {
                    return Ok(dir);
                }

                dir
            }
        };

        match &self.location {
            Path(path) => {
                if install_dir.is_some() {
                    on_install();
                    copy_dir(path, &dir)?;
                }
            }
            Url(url) => {
                on_install();
                download_url(url, &dir, matches!(self.r#type, DependencyType::Project))?
            }
            Git { location, branch } => {
                on_install();
                download_git(location, branch.as_ref(), &dir)?
            }
        }

        Ok(dir)
    }
}

impl DependencyLocation {
    pub fn cache_dir(&self, base: &Path, installing: bool) -> PathBuf {
        use DependencyLocation::*;

        match self {
            Path(path) if !installing => path.clone(),
            _ => {
                let mut hasher = DefaultHasher::new();
                self.hash(&mut hasher);
                let hash = hasher.finish();

                base.join(&hash.to_string())
            }
        }
    }
}

/// https://stackoverflow.com/a/65192210/5569234
fn copy_dir(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> std::io::Result<()> {
    std::fs::create_dir_all(&dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        if ty.is_dir() {
            copy_dir(entry.path(), dst.as_ref().join(entry.file_name()))?;
        } else {
            std::fs::copy(entry.path(), dst.as_ref().join(entry.file_name()))?;
        }
    }
    Ok(())
}

fn download_url(url: &str, path: &Path, extract: bool) -> Result<(), String> {
    let client = reqwest::blocking::Client::new();
    let response = client.get(url).send().map_err(|e| format!("{}", e))?;

    if !response.status().is_success() {
        return Err("Server sent error response".to_string());
    }

    (|| -> Result<(), Box<dyn std::error::Error>> {
        let data = response.bytes()?;

        if extract {
            let mut file = tempfile::tempfile()?;
            file.write_all(&data)?;

            let mut zip = zip::read::ZipArchive::new(file)?;
            std::fs::create_dir_all(path)?;
            zip.extract(path)?;
        } else {
            std::fs::write(path, data)?;
        }

        Ok(())
    })()
    .map_err(|e| format!("{}", e))?;

    Ok(())
}

fn download_git(
    location: &str,
    branch: Option<&String>,
    dir: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut repo = git2::build::RepoBuilder::new();

    if let Some(branch) = branch {
        repo.branch(branch);
    }

    repo.clone(location, dir)?;

    Ok(())
}
