use crate::{
    scope::{Captures, Scopes},
    statements::{
        resolve_statements, split_executable_statements, EagerTraitDeclarationInfo,
        EagerTypeDeclarationInfo,
    },
    Driver,
};
use derivative::Derivative;
use std::{collections::HashMap, mem};
use wipple_util::WithInfo;

#[derive(Debug, Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct Info<D: Driver> {
    pub errors: Vec<WithInfo<D::Info, crate::Diagnostic>>,
    pub dependencies: crate::Interface<D>,
    pub syntax_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::SyntaxDeclaration<D>>>>,
    pub type_declarations: HashMap<
        crate::Path,
        WithInfo<D::Info, (EagerTypeDeclarationInfo, Option<crate::TypeDeclaration<D>>)>,
    >,
    pub trait_declarations: HashMap<
        crate::Path,
        WithInfo<
            D::Info,
            (
                EagerTraitDeclarationInfo,
                Option<crate::TraitDeclaration<D>>,
            ),
        >,
    >,
    pub constant_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::ConstantDeclaration<D>>>>,
    pub type_parameter_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::TypeParameterDeclaration<D>>>>,
    pub language_declarations: HashMap<String, Vec<crate::Path>>,
    pub instance_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::InstanceDeclaration<D>>>>,
    pub ide: crate::Ide<D>,
    pub library: crate::Library<D>,
    pub path: crate::Path,
    pub scopes: Scopes<D>,
    pub next_variable: u32,
    pub captures: Vec<Captures>,
}

impl<D: Driver> Info<D> {
    pub fn reset_next_variable(&mut self) -> u32 {
        mem::take(&mut self.next_variable)
    }

    pub fn make_path(&self, component: crate::PathComponent) -> crate::Path {
        let mut path = self.path.clone();
        path.push(component);
        path
    }

    pub fn top_level_path(&self) -> crate::Path {
        crate::Path(vec![self.path.first().unwrap().clone()])
    }

    pub fn declare_variable(&mut self, path: &crate::Path) {
        if let Some(captures) = self.captures.last_mut() {
            captures.declared.insert(path.clone());
        }
    }

    pub fn capture_if_variable(&mut self, path: &crate::Path) {
        if !matches!(path.last(), Some(crate::PathComponent::Variable(_))) {
            return;
        }

        for captures in self.captures.iter_mut().rev() {
            if captures.declared.contains(path) {
                break;
            }

            captures.used.insert(path.clone());
        }
    }
}

/// Resolve a list of files into an interface and a library.
pub fn resolve<D: Driver>(
    files: impl IntoIterator<Item = WithInfo<D::Info, crate::UnresolvedFile<D>>>,
    dependencies: Vec<crate::Interface<D>>,
) -> crate::Result<D> {
    let mut info = Info::default();
    info.scopes.push_block_scope();

    for dependency in dependencies {
        macro_rules! add {
            ($declarations:ident($f:expr)) => {
                for (path, declaration) in dependency.$declarations {
                    info.$declarations
                        .insert(path.clone(), declaration.map($f));
                }
            };
            ($($declarations:ident($f:expr)),* $(,)?) => {
                $(add!($declarations($f));)*
            }
        }

        add!(
            syntax_declarations(Some),
            type_declarations(|declaration| (
                EagerTypeDeclarationInfo::from(&declaration),
                Some(declaration)
            )),
            trait_declarations(|declaration| (
                EagerTraitDeclarationInfo::from(&declaration),
                Some(declaration)
            )),
            constant_declarations(Some),
            type_parameter_declarations(Some),
            instance_declarations(Some),
        );

        for (name, paths) in dependency.language_declarations {
            info.language_declarations
                .entry(name)
                .or_default()
                .extend(paths);
        }

        for (name, paths) in dependency.top_level {
            for path in paths {
                info.scopes.define(name.clone(), path);
            }
        }
    }

    let dependencies_info = info.clone();

    let statements_by_file = files
        .into_iter()
        .map(|file| {
            info.path
                .push(crate::PathComponent::File(file.item.name.clone()));

            info.scopes.push_block_scope();

            let (declaration_statements, executable_statements) =
                split_executable_statements(file.item.statements, &mut info);

            let path_component = info.path.pop().unwrap();
            assert!(info.path.is_empty());

            let scope = info.scopes.pop_scope();
            info.scopes.merge(&scope);

            (
                path_component,
                declaration_statements,
                executable_statements,
            )
        })
        .collect::<Vec<_>>();

    let executable_statements_by_file = statements_by_file
        .into_iter()
        .map(
            |(file_path_component, declaration_statements, executable_statements)| {
                info.path.push(file_path_component);

                for statements in declaration_statements {
                    resolve_statements(statements, &mut info);
                }

                let file_path_component = info.path.pop().unwrap();
                assert!(info.path.is_empty());

                (file_path_component, executable_statements)
            },
        )
        .collect::<Vec<_>>();

    for (file_path_component, executable_statements) in executable_statements_by_file {
        info.path.push(file_path_component);
        info.scopes.push_block_scope();

        let statements = resolve_statements(executable_statements, &mut info);

        info.scopes.pop_scope();
        assert!(info.scopes.0.len() == 1);

        let path = crate::Path(vec![info.path.pop().unwrap()]);
        assert!(info.path.is_empty());

        info.library
            .code
            .insert(path, crate::TopLevelCode { statements });
    }

    let mut interface = crate::Interface::default();

    for (name, paths) in info.scopes.pop_scope().into_paths() {
        for path in paths {
            let from_dependency = dependencies_info
                .scopes
                .0
                .last()
                .unwrap()
                .paths
                .contains_key(&name);

            if !from_dependency && !path.item.last().unwrap().is_local() {
                interface
                    .top_level
                    .entry(name.clone())
                    .or_default()
                    .push(path);
            }
        }
    }

    assert!(info.scopes.0.is_empty());

    macro_rules! unwrap {
        ($declarations:ident($f:expr)) => {
            for (name, declaration) in info.$declarations {
                let from_dependency = dependencies_info.$declarations.contains_key(&name);

                if !from_dependency {
                    interface.$declarations.insert(name, declaration.map($f));
                }
            }
        };
        ($($declarations:ident($f:expr)),* $(,)?) => {
            $(unwrap!($declarations($f));)*
        }
    }

    unwrap!(
        syntax_declarations(Option::unwrap),
        type_declarations(|(_, declaration)| declaration.unwrap()),
        trait_declarations(|(_, declaration)| declaration.unwrap()),
        constant_declarations(Option::unwrap),
        type_parameter_declarations(Option::unwrap),
        instance_declarations(Option::unwrap),
    );

    for (name, mut paths) in info.language_declarations {
        let dependencies_paths = dependencies_info.language_declarations.get(&name);

        paths.retain(|path| dependencies_paths.map_or(true, |paths| !paths.contains(path)));

        interface
            .language_declarations
            .entry(name)
            .or_default()
            .extend(paths);
    }

    crate::Result {
        interface,
        library: info.library,
        ide: info.ide,
        diagnostics: info.errors,
    }
}
