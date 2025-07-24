use crate::{
    lower::{
        scope::{Captures, Scopes},
        statements::{
            EagerTraitDeclarationInfo, EagerTypeDeclarationInfo, resolve_statements,
            split_executable_statements,
        },
    },
    util::WithInfo,
};
use std::{collections::HashMap, mem, sync::Arc};

#[derive(Debug, Clone, Default)]
pub struct Info {
    pub errors: Vec<WithInfo<crate::lower::Diagnostic>>,
    pub existing: Arc<crate::lower::Interface>,
    pub syntax_declarations:
        HashMap<crate::lower::Path, WithInfo<Option<crate::lower::SyntaxDeclaration>>>,
    pub type_declarations: HashMap<
        crate::lower::Path,
        WithInfo<(
            EagerTypeDeclarationInfo,
            Option<crate::lower::TypeDeclaration>,
        )>,
    >,
    pub trait_declarations: HashMap<
        crate::lower::Path,
        WithInfo<(
            EagerTraitDeclarationInfo,
            Option<crate::lower::TraitDeclaration>,
        )>,
    >,
    pub constant_declarations:
        HashMap<crate::lower::Path, WithInfo<Option<crate::lower::ConstantDeclaration>>>,
    pub type_parameter_declarations:
        HashMap<crate::lower::Path, WithInfo<Option<crate::lower::TypeParameterDeclaration>>>,
    pub language_declarations: HashMap<String, Vec<crate::lower::Path>>,
    pub instance_declarations:
        HashMap<crate::lower::Path, WithInfo<Option<crate::lower::InstanceDeclaration>>>,
    pub library: crate::lower::Library,
    pub path: crate::lower::Path,
    pub scopes: Scopes,
    pub next_variable: u32,
    pub captures: Vec<Captures>,
}

impl Info {
    pub fn reset_next_variable(&mut self) -> u32 {
        mem::take(&mut self.next_variable)
    }

    pub fn make_path(&self, component: crate::lower::PathComponent) -> crate::lower::Path {
        let mut path = self.path.clone();
        path.push(component);
        path
    }

    pub fn top_level_path(&self) -> crate::lower::Path {
        crate::lower::Path(vec![self.path.first().unwrap().clone()])
    }

    pub fn declare_variable(&mut self, path: &crate::lower::Path) {
        if let Some(captures) = self.captures.last_mut() {
            captures.declared.insert(path.clone());
        }
    }

    pub fn capture_if_variable(&mut self, path: &crate::lower::Path) {
        if !matches!(path.last(), Some(crate::lower::PathComponent::Variable(_))) {
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
pub fn resolve(
    files: impl IntoIterator<Item = WithInfo<crate::lower::UnresolvedFile>>,
    existing: crate::lower::Interface,
) -> crate::lower::Result {
    let mut info = Info::default();
    info.scopes.push_block_scope();

    macro_rules! add {
        ($declarations:ident($f:expr)) => {
            for (path, declaration) in existing.$declarations {
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

    for (name, paths) in existing.language_declarations {
        info.language_declarations
            .entry(name)
            .or_default()
            .extend(paths);
    }

    for (name, paths) in existing.top_level {
        for path in paths {
            info.scopes.define(name.clone(), path);
        }
    }

    let existing_info = info.clone();

    let statements_by_file = files
        .into_iter()
        .map(|file| {
            info.path
                .push(crate::lower::PathComponent::File(file.item.name.clone()));

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

        let path = crate::lower::Path(vec![info.path.pop().unwrap()]);
        assert!(info.path.is_empty());

        info.library
            .code
            .insert(path, crate::lower::TopLevelCode { statements });
    }

    let mut interface = crate::lower::Interface::default();

    for (name, paths) in info.scopes.pop_scope().into_paths() {
        for path in paths {
            let from_existing = existing_info
                .scopes
                .0
                .last()
                .unwrap()
                .paths
                .contains_key(&name);

            if !from_existing && !path.item.last().unwrap().is_local() {
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
                let from_existing = existing_info.$declarations.contains_key(&name);

                if !from_existing {
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
        let existing_paths = existing_info.language_declarations.get(&name);

        paths.retain(|path| existing_paths.is_none_or(|paths| !paths.contains(path)));

        interface
            .language_declarations
            .entry(name)
            .or_default()
            .extend(paths);
    }

    crate::lower::Result {
        interface,
        library: info.library,
        diagnostics: info.errors,
    }
}
