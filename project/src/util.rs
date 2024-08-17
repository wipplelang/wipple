use anyhow::Context;
use std::{
    hash::{DefaultHasher, Hash, Hasher},
    ops::Deref,
    path::{Path, PathBuf},
};
use tempdir::TempDir;

pub trait RunExt {
    fn run(self) -> anyhow::Result<()>;
}

impl RunExt for std::process::Child {
    fn run(mut self) -> anyhow::Result<()> {
        let status = self.wait()?;
        if !status.success() {
            anyhow::bail!("command failed: {:?}", status);
        }

        Ok(())
    }
}

pub enum BuildDir<'a> {
    Temporary(TempDir),
    Path(&'a Path),
}

impl<'a> BuildDir<'a> {
    pub fn from_options(options: &'a crate::Options) -> anyhow::Result<Self> {
        let path = match &options.build_dir {
            Some(build_dir) => {
                std::fs::create_dir_all(build_dir)
                    .with_context(|| "creating temporary build directory")?;

                BuildDir::Path(build_dir)
            }
            None => BuildDir::Temporary(TempDir::new("wipple")?),
        };

        Ok(path)
    }
}

impl Deref for BuildDir<'_> {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        match self {
            BuildDir::Temporary(tempdir) => tempdir.path(),
            BuildDir::Path(path) => path,
        }
    }
}

pub fn glob(paths: impl IntoIterator<Item = PathBuf>) -> anyhow::Result<Vec<PathBuf>> {
    paths
        .into_iter()
        .map(|path| Ok(glob::glob(&path.to_string_lossy())?))
        .collect::<anyhow::Result<Vec<_>>>()?
        .into_iter()
        .flat_map(|paths| paths.map(|path| Ok(path?)))
        .collect::<anyhow::Result<Vec<_>>>()
}

pub fn dependency_name(dependency: &crate::Dependency) -> String {
    let mut hasher = DefaultHasher::new();
    dependency.hash(&mut hasher);
    format!("{:x}", hasher.finish())
}
