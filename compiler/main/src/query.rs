use crate::{Driver, Info, Interface, Library};
use itertools::Itertools;
use wipple_lower::Path;
use wipple_typecheck::{Pattern, Traverse, TypedExpressionKind};
use wipple_util::WithInfo;

/// Perform queries on an analyzed program.
pub struct Query<'a> {
    interface: &'a Interface,
    library: &'a Library,
}

impl<'a> Query<'a> {
    /// Perform queries on the provided interface and library.
    pub fn new(interface: &'a Interface, library: &'a Library) -> Self {
        Query { interface, library }
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
}
