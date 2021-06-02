use crate::*;
use serde::{Deserialize, Serialize};
use std::{
    collections::hash_map::DefaultHasher,
    collections::HashMap,
    hash::{Hash, Hasher},
    io::Write,
    path::{Path, PathBuf},
};
use wipple::*;

const TARGET: &str = env!("TARGET");

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct ProjectRoot(pub Option<PathBuf>);

stack_key!(pub project_root: ProjectRoot);

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct DependencyPath(pub Option<PathBuf>);

stack_key!(dependency_path_inner: DependencyPath);

#[ext(pub, name = StackDependencyPathExt)]
impl Stack {
    fn dependency_path(&self) -> Result<Cow<PathBuf>> {
        let error = || wipple::error("Dependency path not set", self);

        match self.dependency_path_inner() {
            Cow::Owned(path) => path.0.map(Cow::Owned).ok_or_else(error),
            Cow::Borrowed(path) => path.0.as_ref().map(Cow::Borrowed).ok_or_else(error),
        }
    }

    fn set_dependency_path(&mut self, path: PathBuf) {
        *self.dependency_path_inner_mut() = DependencyPath(Some(path))
    }
}

thread_local! {
    pub static PROJECT_ENVS: Rc<RefCell<HashMap<PathBuf, Env>>> = Default::default();
    pub static DEPENDENCIES: Rc<RefCell<HashMap<PathBuf, ParsedProject>>> = Default::default();
}

pub fn project_env_for_path(path: &Path) -> Env {
    PROJECT_ENVS
        .with(Clone::clone)
        .borrow_mut()
        .entry(path.to_path_buf())
        .or_insert_with(|| Env::global().child())
        .clone()
}

pub fn project_env_in(stack: &Stack) -> Env {
    match stack.project_root().0.as_ref() {
        Some(path) => project_env_for_path(&path),
        None => Env::global(),
    }
}

#[derive(Clone)]
pub struct Project {
    pub path: PathBuf,
    pub dependencies: HashMap<String, Dependency>,
    pub main: String,
}

impl Project {
    pub fn from_file(path: PathBuf, stack: &Stack) -> Result<Project> {
        let mut stack = stack.clone();
        stack
            .diagnostics_mut()
            .add(|| format!("Loading project '{}'", path.to_string_lossy()));

        let env = project_env_for_path(&path).child();
        setup_project_env(path.parent().unwrap(), &env, &stack)?;

        let module = import_file_with_parent_env(path.clone(), &env, &stack)?;

        parse_project_env(&path, &module.env, &stack)
    }

    pub fn update_dependencies(
        &self,
        install_path: &Path,
        on_install: &dyn Fn(),
        stack: &Stack,
    ) -> Result<HashMap<String, ParsedProject>> {
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
            .with(Clone::clone)
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

    pub fn run(&self, stack: &Stack) -> Result<Module> {
        let mut stack = stack.clone();

        let project_root_path = self.root_path();
        *stack.project_root_mut() = ProjectRoot(Some(project_root_path.to_path_buf()));
        stack
            .diagnostics_mut()
            .add(|| format!("Running project '{}'", self.path.to_string_lossy()));

        let project_env = project_env_for_path(project_root_path);

        for (name, project) in &self.dependencies {
            let path = project.root_path();

            if !path.exists() {
                return Err(error(
                    &format!(
                        "Dependency '{}' is not installed at '{}'",
                        name,
                        path.to_string_lossy()
                    ),
                    &stack,
                ));
            }

            project_env.set_variable(
                &stack,
                name,
                Value::of(Text::new(path.to_string_lossy().to_string())),
            )?;
        }

        import(&self.main, &stack)
    }
}

fn setup_project_env(project_root: &Path, env: &Env, stack: &Stack) -> Result<()> {
    let project_root = project_root.to_path_buf();

    env.set_variable(
        stack,
        "path",
        Value::of(Function::new(move |value, env, stack| {
            let value = value.evaluate(env, stack)?;
            let path = value.get_or::<Text>("Expected path text", env, stack)?;

            let path = project_root
                .join(PathBuf::from(path.text.to_string()))
                .canonicalize()
                .map_err(|error| {
                    wipple::error(&format!("Error resolving path: {}", error), stack)
                })?;

            Ok(Value::of(Dependency::Path(path)))
        })),
    )?;

    env.set_variable(
        stack,
        "url",
        Value::of(Function::new(|value, env, stack| {
            let value = value.evaluate(env, stack)?;
            let url = value.get_or::<Text>("Expected URL text", env, stack)?;
            Ok(Value::of(Dependency::Url(url.text.to_string())))
        })),
    )?;

    env.set_variable(
        stack,
        "git",
        Value::of(Function::new(|value, env, stack| {
            let value = value.evaluate(env, stack)?;

            let repo = &value
                .get_or::<Text>("Expected URL to Git repository", env, stack)?
                .text;

            let dependency = if repo.contains(' ') {
                let repo = repo.split(' ').collect::<Vec<_>>();

                if repo.len() != 2 {
                    return Err(error(
                        "Expected a URL and a branch separated by a space",
                        stack,
                    ));
                }

                Dependency::Git {
                    url: repo[0].to_string(),
                    branch: Some(repo[1].to_string()),
                }
            } else {
                Dependency::Git {
                    url: repo.to_string(),
                    branch: None,
                }
            };

            Ok(Value::of(dependency))
        })),
    )?;

    env.set_variable(stack, "target", Value::of(Text::new(TARGET.to_string())))?;

    Ok(())
}

fn parse_project_env(project_path: &Path, env: &Env, stack: &Stack) -> Result<Project> {
    let dependencies = (|| {
        let mut stack = stack.clone();
        stack
            .diagnostics_mut()
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

        let variables = dependencies_module.env.variables().0;
        for (name, dependency_variable) in variables {
            let mut stack = stack.clone();
            stack
                .diagnostics_mut()
                .add(|| format!("Parsing dependency '{}'", name));

            let dependency = dependency_variable
                .get_value(env, &stack)?
                .get_or::<Dependency>("Expected dependency", env, &stack)?
                .into_owned();

            dependencies.insert(name.clone(), dependency);
        }

        Ok(dependencies)
    })()?;

    let main = (|| {
        let mut stack = stack.clone();
        stack.diagnostics_mut().add(|| {
            format!(
                "Resolving main file in project '{}'",
                project_path.to_string_lossy()
            )
        });

        let main = Name::new("main")
            .resolve(&env, &stack)?
            .get_or::<Text>(
                "Expected a Text value containing the path to the main file",
                env,
                &stack,
            )?
            .text
            .clone();

        Ok(main)
    })()?;

    Ok(Project {
        path: project_path.to_path_buf(),
        dependencies,
        main,
    })
}

#[derive(TypeInfo, Clone, Hash)]
pub enum Dependency {
    Url(String),
    Git { url: String, branch: Option<String> },
    Path(PathBuf),
}

impl Primitive for Dependency {}

impl Dependency {
    pub fn update(
        &self,
        install_path: &Path,
        on_install: impl FnOnce(),
        stack: &Stack,
    ) -> Result<Project> {
        use Dependency::*;

        let path = self.cache_path(install_path);

        if path.exists() && !matches!(self, Path(_)) {
            return project_file_in(&path, stack);
        }

        on_install();

        match &self {
            Path(dependency_path) => copy_dir(dependency_path, &path),
            Url(url) => download_zip(url, &path),
            Git { url, branch } => download_git(url, branch.as_deref(), &path),
        }
        .map_err(|error| wipple::error(&error.to_string(), stack))?;

        project_file_in(&path, stack)
    }
}

fn project_file_in(path: &Path, stack: &Stack) -> Result<Project> {
    let project_path = path.join("project.wpl");
    Project::from_file(project_path, stack)
}

impl Dependency {
    pub fn cache_path(&self, base: &Path) -> PathBuf {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let hash = hasher.finish();

        base.join(&hash.to_string())
    }
}

/// https://stackoverflow.com/a/65192210/5569234
pub fn copy_dir(
    src: impl AsRef<Path>,
    dst: impl AsRef<Path>,
) -> std::result::Result<(), Box<dyn std::error::Error>> {
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

pub fn download_zip(url: &str, path: &Path) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let client = reqwest::blocking::Client::new();
    let response = client.get(url).send()?.error_for_status()?;

    let data = response.bytes()?;

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

    Ok(())
}

pub fn download_git(
    url: &str,
    branch: Option<&str>,
    dir: &Path,
) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let mut repo = git2::build::RepoBuilder::new();

    if let Some(branch) = branch {
        repo.branch(branch);
    }

    repo.clone(url, dir)?;

    Ok(())
}
