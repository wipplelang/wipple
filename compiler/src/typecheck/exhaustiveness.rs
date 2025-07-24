//! Algorithm adapted from https://julesjacobs.com/notes/patternmatching/patternmatching.pdf
//! and https://github.com/yorickpeterse/pattern-matching-in-rust

#![allow(clippy::non_canonical_partial_ord_impl)]

use crate::{
    lower::Path,
    syntax::Location,
    typecheck::{Driver, Traverse},
    util::WithInfo,
};
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

/// Check if every pattern in the expression is exhaustive.
pub fn check_exhaustiveness(
    driver: &dyn Driver,
    expression: WithInfo<&crate::typecheck::TypedExpression>,
) -> Vec<WithInfo<crate::typecheck::Diagnostic>> {
    let mut errors = Vec::new();

    expression.traverse(&mut |expression| {
        let result = match &expression.item.kind {
            crate::typecheck::TypedExpressionKind::Initialize { pattern, value } => {
                missing_extra_patterns_in(driver, [pattern.as_ref()], &value.item.r#type)
            }
            crate::typecheck::TypedExpressionKind::Function {
                inputs: patterns, ..
            } => {
                if let crate::typecheck::Type::Function { inputs, .. } = &expression.item.r#type {
                    let mut extra_patterns = Vec::new();
                    let mut missing_patterns = Vec::new();

                    for (pattern, input) in patterns.iter().zip(inputs) {
                        if let Some((extra, missing)) =
                            missing_extra_patterns_in(driver, [pattern.as_ref()], &input.item)
                        {
                            extra_patterns.extend(extra);
                            missing_patterns.extend(missing);
                        }
                    }

                    Some((extra_patterns, missing_patterns))
                } else {
                    None
                }
            }
            crate::typecheck::TypedExpressionKind::When { input, arms } => {
                missing_extra_patterns_in(
                    driver,
                    arms.iter().map(|arm| arm.item.pattern.as_ref()),
                    &input.item.r#type,
                )
            }
            _ => None,
        };

        if let Some((extra_patterns, mut missing_patterns)) = result {
            for info in extra_patterns {
                errors.push(WithInfo {
                    info,
                    item: crate::typecheck::Diagnostic::ExtraPattern,
                });
            }

            if !missing_patterns.is_empty() {
                missing_patterns.sort();

                errors.push(WithInfo {
                    info: expression.info,
                    item: crate::typecheck::Diagnostic::MissingPatterns(missing_patterns),
                });
            }
        }
    });

    errors
}

fn missing_extra_patterns_in<'a>(
    driver: &dyn Driver,
    patterns: impl IntoIterator<Item = WithInfo<&'a crate::typecheck::Pattern>>,
    input_type: &'a crate::typecheck::Type,
) -> Option<(Vec<Location>, Vec<Pattern>)> {
    let input_type = convert_type(driver, input_type, &HashMap::new())?;

    let mut match_compiler = MatchCompiler {
        driver,
        next_row_id: 0,
        next_variable: 0,
        missing: false,
        reachable: Vec::new(),
    };

    let input = match_compiler.new_variable(input_type);

    let mut row_ids = HashMap::new();
    let patterns = patterns
        .into_iter()
        .map(|pattern| {
            let id = match_compiler.new_row_id();
            row_ids.insert(id, pattern.info);
            Some((id, convert_pattern(driver, pattern.item)?))
        })
        .collect::<Option<Vec<_>>>()?;

    let result = match_compiler.compile(input, patterns)?;

    let missing = result
        .missing
        .then(|| result.missing_patterns())
        .unwrap_or_default();

    let extra = result
        .unreachable_arms(row_ids.keys().copied().collect())
        .into_iter()
        .map(|id| row_ids.remove(&id).unwrap())
        .collect::<Vec<_>>();

    Some((extra, missing))
}

fn convert_type(
    driver: &dyn Driver,
    r#type: &crate::typecheck::Type,
    substitutions: &HashMap<Path, crate::typecheck::Type>,
) -> Option<Type> {
    match r#type {
        crate::typecheck::Type::Parameter(path) => Some(
            substitutions
                .get(path)
                .and_then(|r#type| convert_type(driver, r#type, substitutions))
                .unwrap_or(Type::Unmatchable),
        ),
        crate::typecheck::Type::Declared { path, parameters } => {
            let declaration = driver.get_type_declaration(path);

            let substitutions = declaration
                .item
                .parameters
                .into_iter()
                .zip(parameters.iter().map(|r#type| r#type.item.clone()))
                .collect::<HashMap<_, _>>();

            match declaration.item.representation.item {
                crate::typecheck::TypeRepresentation::Marker => {
                    Some(Type::Marker { substitutions })
                }
                crate::typecheck::TypeRepresentation::Structure(fields) => Some(Type::Structure {
                    substitutions,
                    fields: fields
                        .into_iter()
                        .map(|(name, field)| (name, field.item.r#type.item))
                        .collect(),
                }),
                crate::typecheck::TypeRepresentation::Enumeration(variants) => {
                    Some(Type::Enumeration {
                        substitutions,
                        variants: variants
                            .into_iter()
                            .map(|(name, variant)| {
                                (
                                    name,
                                    variant
                                        .item
                                        .value_types
                                        .into_iter()
                                        .map(|r#type| r#type.item)
                                        .collect(),
                                )
                            })
                            .collect(),
                    })
                }
                crate::typecheck::TypeRepresentation::Wrapper(value) => Some(Type::Wrapper {
                    path: path.clone(),
                    substitutions,
                    value: value.item,
                }),
            }
        }
        crate::typecheck::Type::Tuple(elements) => Some(Type::Tuple(
            elements.iter().map(|r#type| r#type.item.clone()).collect(),
        )),
        crate::typecheck::Type::Intrinsic => Some(Type::Unmatchable),
        crate::typecheck::Type::Equal { right, .. } => {
            convert_type(driver, &right.item, substitutions)
        }
        crate::typecheck::Type::Unknown
        | crate::typecheck::Type::Block(_)
        | crate::typecheck::Type::Function { .. }
        | crate::typecheck::Type::Message { .. } => None,
    }
}

fn convert_pattern(driver: &dyn Driver, pattern: &crate::typecheck::Pattern) -> Option<Pattern> {
    match pattern {
        crate::typecheck::Pattern::Unknown => None,
        crate::typecheck::Pattern::Wildcard
        | crate::typecheck::Pattern::Variable(_, _)
        | crate::typecheck::Pattern::Marker(_) => Some(Pattern::Binding),
        crate::typecheck::Pattern::Text(_) => {
            let path = driver.path_for_language_type("text")?;

            Some(Pattern::Constructor(
                Constructor::Wrapper(path),
                vec![Pattern::Constructor(Constructor::Unbounded, Vec::new())],
            ))
        }
        crate::typecheck::Pattern::Number(_) => {
            let path = driver.path_for_language_type("number")?;

            Some(Pattern::Constructor(
                Constructor::Wrapper(path),
                vec![Pattern::Constructor(Constructor::Unbounded, Vec::new())],
            ))
        }
        crate::typecheck::Pattern::Destructure { field_patterns, .. } => {
            let patterns = field_patterns
                .iter()
                .map(|field| convert_pattern(driver, &field.item.pattern.item))
                .collect::<Option<Vec<_>>>()?;

            Some(Pattern::Constructor(Constructor::Structure, patterns))
        }
        crate::typecheck::Pattern::Variant {
            variant,
            value_patterns,
        } => {
            let patterns = value_patterns
                .iter()
                .map(|pattern| convert_pattern(driver, &pattern.item))
                .collect::<Option<Vec<_>>>()?;

            Some(Pattern::Constructor(
                Constructor::Variant(variant.item.clone()),
                patterns,
            ))
        }
        crate::typecheck::Pattern::Wrapper {
            path,
            value_pattern,
        } => {
            let pattern = convert_pattern(driver, &value_pattern.item)?;
            Some(Pattern::Constructor(
                Constructor::Wrapper(path.item.clone()),
                vec![pattern],
            ))
        }
        crate::typecheck::Pattern::Tuple(elements) => {
            let patterns = elements
                .iter()
                .map(|pattern| convert_pattern(driver, &pattern.item))
                .collect::<Option<Vec<_>>>()?;

            Some(Pattern::Constructor(Constructor::Tuple, patterns))
        }
        crate::typecheck::Pattern::Or { left, right } => {
            let left = convert_pattern(driver, &left.item)?;
            let right = convert_pattern(driver, &right.item)?;

            Some(Pattern::Or(vec![left, right]))
        }
        crate::typecheck::Pattern::Annotate { pattern, .. } => {
            convert_pattern(driver, &pattern.item)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Marker {
        substitutions: HashMap<Path, crate::typecheck::Type>,
    },
    Enumeration {
        substitutions: HashMap<Path, crate::typecheck::Type>,
        variants: HashMap<Path, Vec<crate::typecheck::Type>>,
    },
    Structure {
        substitutions: HashMap<Path, crate::typecheck::Type>,
        fields: HashMap<String, crate::typecheck::Type>,
    },
    Wrapper {
        path: Path,
        substitutions: HashMap<Path, crate::typecheck::Type>,
        value: crate::typecheck::Type,
    },
    Tuple(Vec<crate::typecheck::Type>),
    Unmatchable,
}

/// A compiled pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern {
    /// A pattern representing a concrete type.
    Constructor(Constructor, Vec<Pattern>),

    /// A pattern for a variable.
    Binding,

    /// Match any pattern in the list.
    Or(Vec<Pattern>),
}

/// Represents the concrete type a pattern matches.

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constructor {
    /// A variant of an enumeration.
    Variant(Path),

    /// A tuple.
    Tuple,

    /// A structure.
    Structure,

    /// A wrapper type.
    Wrapper(Path),

    /// A type that cannot be matched except with a variable binding or
    /// wildcard (eg. `Number`).
    Unbounded,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Variable {
    counter: u32,
    r#type: Type,
}

impl Pattern {
    fn flatten_or(self, row: Row) -> Vec<(Self, Row)> {
        if let Pattern::Or(patterns) = self {
            patterns
                .into_iter()
                .map(|pattern| (pattern, row.clone()))
                .collect()
        } else {
            vec![(self, row)]
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Row {
    id: u32,
    columns: Vec<Column>,
}

impl Row {
    fn remove_column(&mut self, variable: u32) -> Option<Column> {
        self.columns
            .iter()
            .position(|column| column.variable.counter == variable)
            .map(|index| self.columns.remove(index))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Column {
    variable: Variable,
    pattern: Pattern,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Case {
    constructor: Constructor,
    arguments: Vec<Variable>,
    body: Decision,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Decision {
    Matched(u32),
    NotMatched,
    Switch(Variable, HashMap<Option<Path>, Case>, Option<Box<Decision>>),
}

struct Match {
    tree: Decision,
    missing: bool,
    reachable: Vec<u32>,
}

struct Term {
    variable: Variable,
    constructor: Constructor,
    arguments: Vec<Variable>,
}

impl Term {
    fn pattern(
        &self,
        terms: &HashMap<Option<Path>, Term>,
        mapping: &HashMap<u32, Option<Path>>,
    ) -> Pattern {
        Pattern::Constructor(
            self.constructor.clone(),
            self.arguments
                .iter()
                .map(|variable| {
                    mapping
                        .get(&variable.counter)
                        .map_or(Pattern::Binding, |variant| {
                            terms.get(variant).unwrap().pattern(terms, mapping)
                        })
                })
                .collect(),
        )
    }
}

impl Match {
    fn missing_patterns(&self) -> Vec<Pattern> {
        fn add_missing_patterns(
            node: &Decision,
            terms: &mut HashMap<Option<Path>, Term>,
            missing: &mut HashSet<Pattern>,
        ) {
            match node {
                Decision::Matched(_) => {}
                Decision::NotMatched => {
                    let mut mapping = HashMap::new();
                    for (path, step) in terms.iter() {
                        mapping.insert(step.variable.counter, path.clone());
                    }

                    let constructor = terms
                        .values()
                        .next()
                        .map_or(Pattern::Binding, |term| term.pattern(terms, &mapping));

                    missing.insert(constructor);
                }
                Decision::Switch(variable, cases, fallback) => {
                    for (variant, case) in cases {
                        terms.insert(
                            variant.clone(),
                            Term {
                                variable: variable.clone(),
                                constructor: case.constructor.clone(),
                                arguments: case.arguments.clone(),
                            },
                        );

                        add_missing_patterns(&case.body, terms, missing);
                        terms.remove(variant);
                    }

                    if let Some(node) = fallback {
                        add_missing_patterns(node, terms, missing)
                    }
                }
            }
        }

        let mut patterns = HashSet::new();
        add_missing_patterns(&self.tree, &mut HashMap::new(), &mut patterns);

        Vec::from_iter(patterns)
    }

    fn unreachable_arms(&self, arms: Vec<u32>) -> Vec<u32> {
        HashSet::from_iter(arms)
            .difference(&self.reachable.iter().copied().collect::<HashSet<_>>())
            .copied()
            .sorted()
            .collect()
    }
}

struct MatchCompiler<'a> {
    driver: &'a dyn Driver,
    next_row_id: u32,
    next_variable: u32,
    missing: bool,
    reachable: Vec<u32>,
}

impl MatchCompiler<'_> {
    fn compile(
        mut self,
        input: Variable,
        patterns: impl IntoIterator<Item = (u32, Pattern)>,
    ) -> Option<Match> {
        let rows = patterns
            .into_iter()
            .map(|(id, pattern)| Row {
                id,
                columns: vec![Column {
                    variable: input.clone(),
                    pattern,
                }],
            })
            .collect::<Vec<_>>();

        let tree = self.compile_rows(rows)?;

        Some(Match {
            tree,
            missing: self.missing,
            reachable: self.reachable,
        })
    }

    fn compile_rows(&mut self, rows: Vec<Row>) -> Option<Decision> {
        if rows.is_empty() {
            self.missing = true;
            return Some(Decision::NotMatched);
        }

        let mut rows = rows
            .into_iter()
            .map(|row| Row {
                columns: row
                    .columns
                    .into_iter()
                    .filter(|col| !matches!(col.pattern, Pattern::Binding))
                    .collect(),
                ..row
            })
            .collect::<Vec<_>>();

        if rows.first().unwrap().columns.is_empty() {
            let row = rows.remove(0);
            self.reachable.push(row.id);

            return Some(Decision::Matched(row.id));
        }

        // All columns in a row share the same variable
        let branch_variable = rows
            .first()
            .unwrap()
            .columns
            .first()
            .unwrap()
            .variable
            .clone();

        match &branch_variable.r#type {
            Type::Marker { .. } => Some(Decision::Matched(rows.first().unwrap().id)),
            Type::Enumeration {
                substitutions,
                variants,
            } => {
                let cases = variants
                    .iter()
                    .map(|(path, types)| {
                        Some((
                            Some(path.clone()),
                            (
                                Constructor::Variant(path.clone()),
                                self.new_variables(
                                    types
                                        .clone()
                                        .iter()
                                        .map(|r#type| {
                                            convert_type(self.driver, r#type, substitutions)
                                        })
                                        .collect::<Option<Vec<_>>>()?,
                                ),
                                Vec::new(),
                            ),
                        ))
                    })
                    .collect::<Option<HashMap<_, _>>>()?;

                let cases = self.compile_constructor_cases(rows, &branch_variable, cases)?;

                Some(Decision::Switch(branch_variable.clone(), cases, None))
            }
            Type::Structure {
                substitutions,
                fields,
            } => {
                let cases = HashMap::from([(
                    None,
                    (
                        Constructor::Structure,
                        self.new_variables(
                            fields
                                .iter()
                                .sorted_by_key(|(name, _)| *name)
                                .map(|(_, r#type)| convert_type(self.driver, r#type, substitutions))
                                .collect::<Option<Vec<_>>>()?,
                        ),
                        Vec::new(),
                    ),
                )]);

                let cases = self.compile_constructor_cases(rows, &branch_variable, cases)?;

                Some(Decision::Switch(branch_variable.clone(), cases, None))
            }
            Type::Tuple(elements) => {
                let cases = HashMap::from([(
                    None,
                    (
                        Constructor::Tuple,
                        self.new_variables(
                            elements
                                .iter()
                                .map(|r#type| convert_type(self.driver, r#type, &HashMap::new()))
                                .collect::<Option<Vec<_>>>()?,
                        ),
                        Vec::new(),
                    ),
                )]);

                let cases = self.compile_constructor_cases(rows, &branch_variable, cases)?;

                Some(Decision::Switch(branch_variable.clone(), cases, None))
            }
            Type::Unmatchable => {
                let (cases, fallback) = self.compile_unbounded_cases(rows, &branch_variable)?;

                Some(Decision::Switch(
                    branch_variable,
                    cases,
                    Some(Box::new(fallback)),
                ))
            }
            Type::Wrapper {
                path,
                substitutions,
                value,
            } => {
                let cases = HashMap::from([(
                    None,
                    (
                        Constructor::Wrapper(path.clone()),
                        vec![self.new_variable(convert_type(self.driver, value, substitutions)?)],
                        Vec::new(),
                    ),
                )]);

                let cases = self.compile_constructor_cases(rows, &branch_variable, cases)?;

                Some(Decision::Switch(branch_variable.clone(), cases, None))
            }
        }
    }

    fn compile_constructor_cases(
        &mut self,
        rows: impl IntoIterator<Item = Row>,
        branch_variable: &Variable,
        mut cases: HashMap<Option<Path>, (Constructor, Vec<Variable>, Vec<Row>)>,
    ) -> Option<HashMap<Option<Path>, Case>> {
        for mut row in rows {
            if let Some(column) = row.remove_column(branch_variable.counter) {
                for (pattern, row) in column.pattern.flatten_or(row) {
                    if let Pattern::Constructor(constructor, arguments) = pattern {
                        let variant = match constructor {
                            Constructor::Variant(path) => Some(path),
                            _ => None,
                        };

                        let mut columns = row.columns;
                        for (variable, pattern) in cases.get(&variant)?.1.iter().zip(arguments) {
                            columns.push(Column {
                                variable: variable.clone(),
                                pattern,
                            });
                        }

                        cases.get_mut(&variant)?.2.push(Row { columns, ..row });
                    }
                }
            } else {
                for (_, _, rows) in cases.values_mut() {
                    rows.push(row.clone());
                }
            }
        }

        cases
            .into_iter()
            .map(|(variant, (constructor, arguments, rows))| {
                Some((
                    variant,
                    Case {
                        constructor,
                        arguments,
                        body: self.compile_rows(rows)?,
                    },
                ))
            })
            .collect()
    }

    fn compile_unbounded_cases(
        &mut self,
        rows: impl IntoIterator<Item = Row>,
        branch_variable: &Variable,
    ) -> Option<(HashMap<Option<Path>, Case>, Decision)> {
        let mut raw_cases = Vec::new();
        let mut fallback_rows = Vec::new();

        for mut row in rows {
            if let Some(column) = row.remove_column(branch_variable.counter) {
                for (_, row) in column.pattern.flatten_or(row) {
                    let constructor = Constructor::Unbounded;
                    raw_cases.push((constructor, Vec::new(), vec![row]));
                }
            } else {
                fallback_rows.push(row);
            }
        }

        for (_, _, rows) in &mut raw_cases {
            rows.append(&mut fallback_rows.clone());
        }

        let cases = raw_cases
            .into_iter()
            .map(|(constructor, arguments, rows)| {
                Some((
                    None,
                    Case {
                        constructor,
                        arguments,
                        body: self.compile_rows(rows)?,
                    },
                ))
            })
            .collect::<Option<_>>()?;

        Some((cases, self.compile_rows(fallback_rows)?))
    }

    fn new_row_id(&mut self) -> u32 {
        let id = self.next_row_id;
        self.next_row_id += 1;
        id
    }

    fn new_variable(&mut self, r#type: Type) -> Variable {
        let variable = Variable {
            counter: self.next_variable,
            r#type,
        };

        self.next_variable += 1;

        variable
    }

    fn new_variables(&mut self, types: impl IntoIterator<Item = Type>) -> Vec<Variable> {
        types
            .into_iter()
            .map(|r#type| self.new_variable(r#type))
            .collect()
    }
}
