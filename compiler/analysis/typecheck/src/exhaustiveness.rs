//! Algorithm adapted from https://julesjacobs.com/notes/patternmatching/patternmatching.pdf
//! and https://github.com/yorickpeterse/pattern-matching-in-rust

use crate::{Driver, Traverse};
use derivative::Derivative;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use wipple_util::WithInfo;

/// Check if every pattern in the expression is exhaustive.
pub fn check_exhaustiveness<D: Driver>(
    driver: &D,
    expression: WithInfo<D::Info, &crate::TypedExpression<D>>,
) -> Vec<WithInfo<D::Info, crate::Diagnostic<D>>> {
    let mut errors = Vec::new();

    expression.traverse(&mut |expression| {
        let result = match &expression.item.kind {
            crate::TypedExpressionKind::Initialize { pattern, value } => {
                missing_extra_patterns_in(driver, [pattern.as_ref()], &value.item.r#type)
            }
            crate::TypedExpressionKind::Function {
                inputs: patterns, ..
            } => {
                if let crate::Type::Function { inputs, .. } = &expression.item.r#type {
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
            crate::TypedExpressionKind::When { input, arms } => missing_extra_patterns_in(
                driver,
                arms.iter().map(|arm| arm.item.pattern.as_ref()),
                &input.item.r#type,
            ),
            _ => None,
        };

        if let Some((extra_patterns, missing_patterns)) = result {
            for info in extra_patterns {
                errors.push(WithInfo {
                    info,
                    item: crate::Diagnostic::ExtraPattern,
                });
            }

            if !missing_patterns.is_empty() {
                errors.push(WithInfo {
                    info: expression.info,
                    item: crate::Diagnostic::MissingPatterns(missing_patterns),
                });
            }
        }
    });

    errors
}

fn missing_extra_patterns_in<'a, D: Driver + 'a>(
    driver: &D,
    patterns: impl IntoIterator<Item = WithInfo<D::Info, &'a crate::Pattern<D>>>,
    input_type: &'a crate::Type<D>,
) -> Option<(Vec<D::Info>, Vec<Pattern<D>>)> {
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
            Some((id, convert_pattern(pattern.item)?))
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

fn convert_type<D: Driver>(
    driver: &D,
    r#type: &crate::Type<D>,
    substitutions: &HashMap<D::Path, crate::Type<D>>,
) -> Option<Type<D>> {
    match r#type {
        crate::Type::Parameter(path) => Some(
            substitutions
                .get(path)
                .and_then(|r#type| convert_type(driver, r#type, substitutions))
                .unwrap_or(Type::Unmatchable),
        ),
        crate::Type::Declared { path, parameters } => {
            let declaration = driver.get_type_declaration(path);

            let substitutions = declaration
                .item
                .parameters
                .into_iter()
                .zip(parameters.iter().map(|r#type| r#type.item.clone()))
                .collect::<HashMap<_, _>>();

            match declaration.item.representation.item {
                crate::TypeRepresentation::Opaque => Some(Type::Unmatchable),
                crate::TypeRepresentation::Structure(fields) => Some(Type::Structure {
                    substitutions,
                    fields: fields
                        .into_iter()
                        .map(|(name, field)| (name, field.item.r#type.item))
                        .collect(),
                }),
                crate::TypeRepresentation::Enumeration(variants) => Some(Type::Enumeration {
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
                }),
            }
        }
        crate::Type::Tuple(elements) => Some(Type::Tuple(
            elements.iter().map(|r#type| r#type.item.clone()).collect(),
        )),
        crate::Type::Unknown(_) | crate::Type::Deferred(_) | crate::Type::Function { .. } => None,
    }
}

fn convert_pattern<D: Driver>(pattern: &crate::Pattern<D>) -> Option<Pattern<D>> {
    match pattern {
        crate::Pattern::Unknown => None,
        crate::Pattern::Wildcard | crate::Pattern::Variable(_, _) => Some(Pattern::Binding),
        crate::Pattern::Text(_) | crate::Pattern::Number(_) => {
            Some(Pattern::Constructor(Constructor::Unbounded, Vec::new()))
        }
        crate::Pattern::Destructure(fields) => {
            let patterns = fields
                .iter()
                .map(|field| convert_pattern(&field.item.pattern.item))
                .collect::<Option<Vec<_>>>()?;

            Some(Pattern::Constructor(Constructor::Structure, patterns))
        }
        crate::Pattern::Variant {
            variant,
            value_patterns,
        } => {
            let patterns = value_patterns
                .iter()
                .map(|pattern| convert_pattern(&pattern.item))
                .collect::<Option<Vec<_>>>()?;

            Some(Pattern::Constructor(
                Constructor::Variant(variant.item.clone()),
                patterns,
            ))
        }
        crate::Pattern::Tuple(elements) => {
            let patterns = elements
                .iter()
                .map(|pattern| convert_pattern(&pattern.item))
                .collect::<Option<Vec<_>>>()?;

            Some(Pattern::Constructor(Constructor::Tuple, patterns))
        }
        crate::Pattern::Or { left, right } => {
            let left = convert_pattern(&left.item)?;
            let right = convert_pattern(&right.item)?;

            Some(Pattern::Or(vec![left, right]))
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
enum Type<D: Driver> {
    Enumeration {
        substitutions: HashMap<D::Path, crate::Type<D>>,
        variants: HashMap<D::Path, Vec<crate::Type<D>>>,
    },
    Structure {
        substitutions: HashMap<D::Path, crate::Type<D>>,
        fields: HashMap<String, crate::Type<D>>,
    },
    Tuple(Vec<crate::Type<D>>),
    Unmatchable,
}

/// A compiled pattern.
#[derive(Derivative, Serialize, Deserialize)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", bound = "")]
pub enum Pattern<D: Driver> {
    /// A pattern representing a concrete type.
    Constructor(Constructor<D>, Vec<Pattern<D>>),

    /// A pattern for a variable.
    Binding,

    /// Match any pattern in the list.
    Or(Vec<Pattern<D>>),
}

/// Represents the concrete type a pattern matches.
#[derive(Derivative, Serialize, Deserialize)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", bound = "")]
pub enum Constructor<D: Driver> {
    /// A variant of an enumeration.
    Variant(D::Path),

    /// A tuple.
    Tuple,

    /// A structure.
    Structure,

    /// A type that cannot be matched except with a variable binding or
    /// wildcard (eg. `Number`).
    Unbounded,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
struct Variable<D: Driver> {
    counter: u32,
    r#type: Type<D>,
}

impl<D: Driver> Pattern<D> {
    fn flatten_or(self, row: Row<D>) -> Vec<(Self, Row<D>)> {
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

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
struct Row<D: Driver> {
    id: u32,
    columns: Vec<Column<D>>,
}

impl<D: Driver> Row<D> {
    fn remove_column(&mut self, variable: u32) -> Option<Column<D>> {
        self.columns
            .iter()
            .position(|column| column.variable.counter == variable)
            .map(|index| self.columns.remove(index))
    }
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
struct Column<D: Driver> {
    variable: Variable<D>,
    pattern: Pattern<D>,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
struct Case<D: Driver> {
    constructor: Constructor<D>,
    arguments: Vec<Variable<D>>,
    body: Decision<D>,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
enum Decision<D: Driver> {
    Matched(u32),
    NotMatched,
    Switch(
        Variable<D>,
        HashMap<Option<D::Path>, Case<D>>,
        Option<Box<Decision<D>>>,
    ),
}

struct Match<D: Driver> {
    tree: Decision<D>,
    missing: bool,
    reachable: Vec<u32>,
}

struct Term<D: Driver> {
    variable: Variable<D>,
    constructor: Constructor<D>,
    arguments: Vec<Variable<D>>,
}

impl<D: Driver> Term<D> {
    fn pattern(
        &self,
        terms: &HashMap<Option<D::Path>, Term<D>>,
        mapping: &HashMap<u32, Option<D::Path>>,
    ) -> Pattern<D> {
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

impl<D: Driver> Match<D> {
    fn missing_patterns(&self) -> Vec<Pattern<D>> {
        fn add_missing_patterns<D: Driver>(
            node: &Decision<D>,
            terms: &mut HashMap<Option<D::Path>, Term<D>>,
            missing: &mut HashSet<Pattern<D>>,
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

struct MatchCompiler<'a, D: Driver> {
    driver: &'a D,
    next_row_id: u32,
    next_variable: u32,
    missing: bool,
    reachable: Vec<u32>,
}

impl<'a, D: Driver> MatchCompiler<'a, D> {
    fn compile(
        mut self,
        input: Variable<D>,
        patterns: impl IntoIterator<Item = (u32, Pattern<D>)>,
    ) -> Option<Match<D>> {
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

    fn compile_rows(&mut self, rows: Vec<Row<D>>) -> Option<Decision<D>> {
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
        }
    }

    fn compile_constructor_cases(
        &mut self,
        rows: impl IntoIterator<Item = Row<D>>,
        branch_variable: &Variable<D>,
        mut cases: HashMap<Option<D::Path>, (Constructor<D>, Vec<Variable<D>>, Vec<Row<D>>)>,
    ) -> Option<HashMap<Option<D::Path>, Case<D>>> {
        for mut row in rows {
            if let Some(column) = row.remove_column(branch_variable.counter) {
                for (pattern, row) in column.pattern.flatten_or(row) {
                    if let Pattern::Constructor(constructor, arguments) = pattern {
                        let variant = match constructor {
                            Constructor::Variant(path) => Some(path),
                            _ => None,
                        };

                        let mut columns = row.columns;
                        for (variable, pattern) in
                            cases.get(&variant).unwrap().1.iter().zip(arguments)
                        {
                            columns.push(Column {
                                variable: variable.clone(),
                                pattern,
                            });
                        }

                        cases
                            .get_mut(&variant)
                            .unwrap()
                            .2
                            .push(Row { columns, ..row });
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
        rows: impl IntoIterator<Item = Row<D>>,
        branch_variable: &Variable<D>,
    ) -> Option<(HashMap<Option<D::Path>, Case<D>>, Decision<D>)> {
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

    fn new_variable(&mut self, r#type: Type<D>) -> Variable<D> {
        let variable = Variable {
            counter: self.next_variable,
            r#type,
        };

        self.next_variable += 1;

        variable
    }

    fn new_variables(&mut self, types: impl IntoIterator<Item = Type<D>>) -> Vec<Variable<D>> {
        types
            .into_iter()
            .map(|r#type| self.new_variable(r#type))
            .collect()
    }
}
