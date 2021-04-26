use crate::*;
use serde::{Deserialize, Serialize};
use std::{
    collections::hash_map::DefaultHasher,
    collections::HashMap,
    error::Error,
    hash::{Hash, Hasher},
    io::Write,
    path::{Path, PathBuf},
    result::Result,
};
use wipple::*;

const TARGET: &str = env!("TARGET");

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct ProjectRoot(pub Option<PathBuf>);

stack_key!(pub project_root for ProjectRoot);

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct DependencyPath(pub Option<PathBuf>);

stack_key!(_dependency_path for DependencyPath);

pub fn dependency_path_in(stack: &Stack) -> wipple::Result<PathBuf> {
    _dependency_path_in(stack)
        .0
        .ok_or_else(|| Return::error("Dependency path not set", stack))
}

pub fn set_dependency_path_in(stack: &mut Stack, path: &Path) {
    *_dependency_path_mut_in(stack) = DependencyPath(Some(path.to_path_buf()))
}

ref_thread_local! {
    pub static managed PROJECT_ENVS: HashMap<PathBuf, EnvironmentRef> = HashMap::new();
    pub static managed DEPENDENCIES: HashMap<PathBuf, ParsedProject> = HashMap::new();
}

pub fn project_env_for_path(path: &Path) -> EnvironmentRef {
    PROJECT_ENVS
        .borrow_mut()
        .entry(path.to_path_buf())
        .or_insert_with(|| Environment::child_of(&Environment::global()).into_ref())
        .clone()
}

pub fn project_env_in(stack: &Stack) -> EnvironmentRef {
    match project_root_in(stack).0 {
        Some(path) => project_env_for_path(&path),
        None => Environment::global(),
    }
}

#[derive(Clone)]
pub struct Project {
    pub path: PathBuf,
    pub dependencies: HashMap<String, Dependency>,
    pub main: String,
}

impl Project {
    pub fn from_file(path: &Path, stack: &Stack) -> wipple::Result<Project> {
        let mut stack = stack.clone();
        stack
            .evaluation_mut()
            .add(|| format!("Loading project '{}'", path.to_string_lossy()));

        let env = Environment::child_of(&project_env_for_path(path)).into_ref();
        setup_project_env(path.parent().unwrap(), &env);

        let module = wipple_loading::import_file_with_parent_env(path, &env, &stack)?;

        parse_project_env(path, &module.env, &stack)
    }

    pub fn update_dependencies(
        &self,
        install_path: &Path,
        on_install: &dyn Fn(),
        stack: &Stack,
    ) -> Result<HashMap<String, ParsedProject>, Box<dyn Error>> {
        let mut dependencies = HashMap::new();

        for (name, dependency) in &self.dependencies {
            let project = dependency.update(install_path, &on_install, stack)?;
            let subdependencies = project.update_dependencies(install_path, &on_install, stack)?;
            let parsed_project = project.parse(subdependencies);

            dependencies.insert(name.clone(), parsed_project);
        }

        Ok(dependencies)
    }

    pub fn parse(self, dependencies: HashMap<String, ParsedProject>) -> ParsedProject {
        ParsedProject {
            path: self.path,
            dependencies,
            main: self.main,
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ParsedProject {
    pub path: PathBuf,
    pub dependencies: HashMap<String, ParsedProject>,
    pub main: String,
}

impl ParsedProject {
    pub fn root_path(&self) -> &Path {
        self.path.parent().unwrap()
    }

    pub fn register(&self) {
        DEPENDENCIES
            .borrow_mut()
            .insert(self.path.clone(), self.clone());

        for dependency in self.dependencies.values() {
            dependency.register()
        }
    }

    pub fn change_dependency_paths(&mut self, change: &dyn Fn(&Path) -> PathBuf) {
        for dependency in self.dependencies.values_mut() {
            dependency.path = change(&dependency.path);
            dependency.change_dependency_paths(change);
        }
    }

    pub fn run(&self, stack: &Stack) -> wipple::Result<Module> {
        let mut stack = stack.clone();

        let project_root_path = self.root_path();
        *project_root_mut_in(&mut stack) = ProjectRoot(Some(project_root_path.to_path_buf()));

        stack
            .evaluation_mut()
            .add(|| format!("Running project '{}'", self.path.to_string_lossy()));

        let project_env = project_env_for_path(project_root_path);

        for (name, project) in &self.dependencies {
            let path = project.root_path();

            if !path.exists() {
                return Err(Return::error(
                    &format!(
                        "Dependency '{}' is not installed at '{}'",
                        name,
                        path.to_string_lossy()
                    ),
                    &stack,
                ));
            }

            project_env
                .borrow_mut()
                .set_variable(name, Value::of(Text::new(&path.to_string_lossy())));
        }

        import(&self.main, &stack)
    }
}

fn setup_project_env(project_root: &Path, env: &EnvironmentRef) {
    let project_root = project_root.to_path_buf();

    env.borrow_mut().set_variable(
        "path",
        Value::of(Function::new(move |value, env, stack| {
            let path =
                value
                    .evaluate(env, stack)?
                    .get_or::<Text>("Expected path text", env, stack)?;

            let path = project_root
                .join(PathBuf::from(path.text))
                .canonicalize()
                .map_err(|error| {
                    Return::error(&format!("Error resolving path: {}", error), stack)
                })?;

            Ok(Value::of(Dependency::new(DependencyLocation::Path(path))))
        })),
    );

    env.borrow_mut().set_variable(
        "url",
        Value::of(Function::new(|value, env, stack| {
            let url =
                value
                    .evaluate(env, stack)?
                    .get_or::<Text>("Expected URL text", env, stack)?;

            Ok(Value::of(Dependency::new(DependencyLocation::Url(
                url.text,
            ))))
        })),
    );

    env.borrow_mut().set_variable(
        "git",
        Value::of(Function::new(|value, env, stack| {
            let repo = value
                .evaluate(env, stack)?
                .get_or::<Text>("Expected URL to Git repository", env, stack)?
                .text;

            let git = if repo.contains(' ') {
                let repo = repo.split(' ').collect::<Vec<_>>();

                if repo.len() != 2 {
                    return Err(Return::error(
                        "Expected a URL and a branch separated by a space",
                        stack,
                    ));
                }

                DependencyLocation::Git {
                    url: repo[0].to_string(),
                    branch: Some(repo[1].to_string()),
                }
            } else {
                DependencyLocation::Git {
                    url: repo,
                    branch: None,
                }
            };

            Ok(Value::of(Dependency::new(git)))
        })),
    );

    env.borrow_mut().set_variable(
        "plugin",
        Value::of(Function::new(|value, env, stack| {
            let mut dependency = value.evaluate(env, stack)?.get_or::<Dependency>(
                "Expected dependency",
                env,
                stack,
            )?;

            dependency.is_plugin = true;

            Ok(Value::of(dependency))
        })),
    );

    env.borrow_mut()
        .set_variable("target", Value::of(Text::new(TARGET)));
}

fn parse_project_env(
    project_path: &Path,
    env: &EnvironmentRef,
    stack: &Stack,
) -> wipple::Result<Project> {
    let dependencies = (|| {
        let mut stack = stack.clone();
        stack
            .evaluation_mut()
            .add(|| String::from("Updating dependencies"));

        let dependencies_value = match Name::new("dependencies").resolve_if_present(&env, &stack)? {
            Some(dependencies) => dependencies,
            None => return Ok(HashMap::new()),
        };

        let dependencies_module = dependencies_value.get_or::<Module>(
            "Expected a module containing dependencies",
            env,
            &stack,
        )?;

        let mut dependencies = HashMap::new();

        for (name, dependency_variable) in
            dependencies_module.env.borrow_mut().variables().0.clone()
        {
            let mut stack = stack.clone();
            stack
                .evaluation_mut()
                .add(|| format!("Parsing dependency '{}'", name));

            let dependency = dependency_variable
                .get_value(env, &stack)?
                .get_or::<Dependency>("Expected dependency", env, &stack)?;

            dependencies.insert(name.clone(), dependency);
        }

        Ok(dependencies)
    })()?;

    let main = (|| {
        let mut stack = stack.clone();
        stack.evaluation_mut().add(|| {
            format!(
                "Resolving main file in project '{}'",
                project_path.to_string_lossy()
            )
        });

        let main = Name::new("main").resolve(&env, &stack)?.get_or::<Text>(
            "Expected a Text value containing the path to the main file",
            env,
            &stack,
        )?;

        Ok(main.text)
    })()?;

    Ok(Project {
        path: project_path.to_path_buf(),
        dependencies,
        main,
    })
}

#[derive(TypeInfo, Clone, Hash)]
pub struct Dependency {
    pub location: DependencyLocation,
    pub is_plugin: bool,
}

primitive!(pub dependency for Dependency);

impl Dependency {
    pub fn new(location: DependencyLocation) -> Self {
        Dependency {
            location,
            is_plugin: false,
        }
    }
}

#[derive(Clone, Hash)]
pub enum DependencyLocation {
    Url(String),
    Git { url: String, branch: Option<String> },
    Path(PathBuf),
}

impl Dependency {
    pub fn update(
        &self,
        install_path: &Path,
        on_install: impl FnOnce(),
        stack: &Stack,
    ) -> Result<Project, Box<dyn Error>> {
        use DependencyLocation::*;

        let path = self.location.cache_path(install_path);

        if path.exists() && !matches!(self.location, Path(_)) {
            return project_file_in(&path, stack);
        }

        on_install();

        match &self.location {
            Path(dependency_path) => copy_dir(dependency_path, &path)?,
            Url(url) => download_url(url, &path, !self.is_plugin)?,
            Git { url, branch } => download_git(url, branch.as_deref(), &path)?,
        }

        project_file_in(&path, stack)
    }
}

fn project_file_in(path: &Path, stack: &Stack) -> Result<Project, Box<dyn Error>> {
    let project_path = path.join("project.wpl");
    let project = Project::from_file(&project_path, stack)?;
    Ok(project)
}

impl DependencyLocation {
    pub fn cache_path(&self, base: &Path) -> PathBuf {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let hash = hasher.finish();

        base.join(&hash.to_string())
    }
}

/// https://stackoverflow.com/a/65192210/5569234
pub fn copy_dir(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> std::io::Result<()> {
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

pub fn download_url(url: &str, path: &Path, extract: bool) -> Result<(), String> {
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

            let tempdir = tempfile::tempdir()?;

            std::fs::create_dir_all(&tempdir)?;
            zip.extract(&tempdir)?;

            let entries = tempdir
                .as_ref()
                .read_dir()?
                .filter_map(|e| e.ok())
                .collect::<Vec<_>>();

            // Flatten zip containing a single directory
            if entries.len() == 1 {
                let entry = entries.first().unwrap().path();

                if entry.is_dir() {
                    copy_dir(entry, path)?;
                    return Ok(());
                }
            }

            copy_dir(tempdir, path)?;
        } else {
            std::fs::write(path, data)?;
        }

        Ok(())
    })()
    .map_err(|e| format!("{}", e))?;

    Ok(())
}

pub fn download_git(
    url: &str,
    branch: Option<&str>,
    dir: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut repo = git2::build::RepoBuilder::new();

    if let Some(branch) = branch {
        repo.branch(branch);
    }

    repo.clone(url, dir)?;

    Ok(())
}
