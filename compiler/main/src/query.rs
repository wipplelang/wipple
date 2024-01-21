use crate::{render_type, Driver, Info, Interface, Library};
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap};
use wipple_lower::Path;
use wipple_typecheck::{Pattern, Traverse, TypedExpressionKind};
use wipple_util::WithInfo;

/// Perform queries on an analyzed program.
pub struct Query<'a> {
    interface: &'a Interface,
    library: &'a Library,
    source_code_for_file: Box<dyn Fn(&str) -> String + 'a>,
    source_cache: RefCell<HashMap<String, String>>,
}

impl<'a> Query<'a> {
    /// Perform queries on the provided interface and library.
    pub fn new(
        interface: &'a Interface,
        library: &'a Library,
        source_code_for_file: impl Fn(&str) -> String + 'a,
    ) -> Self {
        Query {
            interface,
            library,
            source_code_for_file: Box::new(source_code_for_file),
            source_cache: Default::default(),
        }
    }

    /// Retrieve the source code at the provided location.
    pub fn source_code_at(&self, info: &Info) -> Option<String> {
        use std::collections::hash_map::Entry;

        let slice = |s: &str| {
            s.get((info.parser_info.span.start as usize)..(info.parser_info.span.end as usize))
                .map(ToString::to_string)
        };

        let mut source_cache = self.source_cache.borrow_mut();
        match source_cache.entry(info.parser_info.path.clone()) {
            Entry::Occupied(entry) => slice(entry.get()),
            Entry::Vacant(entry) => {
                slice(entry.insert((self.source_code_for_file)(&info.parser_info.path)))
            }
        }
    }
}

impl<'a> Query<'a> {
    /// Return the [`Info`] associated with the definition at the provided path.
    pub fn info_at_path(&self, path: &Path) -> Info {
        self.interface
            .type_declarations
            .get(path)
            .map(|declaration| declaration.info.clone())
            .or_else(|| {
                self.interface
                    .trait_declarations
                    .get(path)
                    .map(|declaration| declaration.info.clone())
            })
            .or_else(|| {
                self.interface
                    .type_parameter_declarations
                    .get(path)
                    .map(|declaration| declaration.info.clone())
            })
            .or_else(|| {
                self.interface
                    .constant_declarations
                    .get(path)
                    .map(|declaration| declaration.info.clone())
            })
            .or_else(|| {
                self.interface
                    .instance_declarations
                    .get(path)
                    .map(|declaration| declaration.info.clone())
            })
            .or_else(|| {
                self.collect_variables_matching(|candidate| candidate.item == path)
                    .into_iter()
                    .next()
                    .map(|path| path.info)
            })
            .unwrap_or_else(|| panic!("no declaration found for path {path:?}"))
    }

    /// Return names related to the provided name at the name's position.
    pub fn related_names(&self, name: WithInfo<Info, &str>) -> Vec<WithInfo<Info, &'a str>> {
        const THRESHOLD: f64 = 0.4;

        let names_at_cursor = self.collect_variables_matching(|candidate| {
            name.info
                .parser_info
                .span_is_within(&candidate.info.parser_info)
        });

        let len = name.item.len() as f64;

        names_at_cursor
            .into_iter()
            .map(|path| path.map(|path| path.last().unwrap().name().unwrap()))
            .chain(
                self.interface
                    .top_level
                    .iter()
                    .flat_map(|(name, paths)| paths.iter().map(|path| path.replace(name.as_str()))),
            )
            .filter(|candidate| (candidate.item.len() as f64 - len).abs() / len < THRESHOLD)
            .sorted_by_key(|candidate| candidate.item)
            .map(|candidate| {
                (
                    candidate.clone(),
                    distance::damerau_levenshtein(name.item, candidate.item) as f64 / len,
                )
            })
            .filter(|(_, distance)| *distance < THRESHOLD)
            .map(|(candidate, _)| candidate)
            .collect()
    }

    /// Return the paths of all variables in the program satisfying `f`.
    pub fn collect_variables_matching(
        &self,
        mut f: impl FnMut(WithInfo<Info, &'a Path>) -> bool,
    ) -> Vec<WithInfo<Info, &'a Path>> {
        let mut names = Vec::new();

        let mut check_pattern = |pattern: WithInfo<Info, &'a Pattern<Driver>>| {
            pattern.traverse(&mut |pattern| {
                if let Pattern::Variable(path) = &pattern.item {
                    let path = pattern.replace(path);

                    if f(path.clone()) {
                        names.push(path);
                    }
                }
            });
        };

        for item in self.library.code.iter().chain(self.library.items.values()) {
            item.expression
                .as_ref()
                .traverse(&mut |expression| match &expression.item.kind {
                    TypedExpressionKind::Block(statements) => {
                        for statement in statements {
                            if let TypedExpressionKind::Initialize { pattern, .. } =
                                &statement.item.kind
                            {
                                check_pattern(pattern.as_ref());
                            }
                        }
                    }
                    TypedExpressionKind::Function { pattern, .. } => {
                        check_pattern(pattern.as_ref());
                    }
                    TypedExpressionKind::When { arms, .. } => {
                        for arm in arms {
                            check_pattern(arm.item.pattern.as_ref());
                        }
                    }
                    _ => {}
                });
        }

        names
    }

    /// Retrieve the value of the `show-code` attribute for a definition.
    pub fn get_show_code_enabled(&self, path: &Path) -> bool {
        let info = self.info_at_path(path);

        info.parser_info
            .documentation
            .iter()
            .any(|documentation| match documentation {
                wipple_parser::reader::Documentation::Attribute { name, value }
                    if name == "show-code" =>
                {
                    matches!(
                        value,
                        wipple_parser::reader::DocumentationAttributeValue::True
                    )
                }
                _ => false,
            })
    }

    /// Retrieve the `on-unimplemented` message for a trait definition.
    pub fn get_on_unimplemented_message(
        &self,
        path: &Path,
        parameters: &[WithInfo<Info, wipple_typecheck::Type<Driver>>],
    ) -> Option<String> {
        let info = self.info_at_path(path);

        let trait_declaration = self.interface.trait_declarations.get(path)?;
        let help_show_code_enabled = self.get_show_code_enabled(path);

        info.parser_info
            .documentation
            .iter()
            .find_map(|documentation| match documentation {
                wipple_parser::reader::Documentation::Attribute { name, value }
                    if name == "on-unimplemented" =>
                {
                    match value {
                        wipple_parser::reader::DocumentationAttributeValue::Text(message) => {
                            Some(message.clone())
                        }
                        wipple_parser::reader::DocumentationAttributeValue::FormattedText(
                            segments,
                            trailing,
                        ) => Some(
                            segments
                                .iter()
                                .map(|(text, parameter_name)| {
                                    let index = trait_declaration.item.parameters.iter().position(
                                        |path| {
                                            path.last()
                                                .and_then(|path| path.name())
                                                .is_some_and(|name| name == *parameter_name)
                                        },
                                    )?;

                                    let r#type = parameters.get(index)?;

                                    let code = help_show_code_enabled
                                        .then(|| self.source_code_at(&r#type.info))
                                        .flatten()
                                        .unwrap_or_else(|| render_type(&r#type.item, true));

                                    Some(format!("{}`{}`", text, code))
                                })
                                .collect::<Option<String>>()?
                                + trailing,
                        ),
                        _ => None,
                    }
                }
                _ => None,
            })
    }
}
