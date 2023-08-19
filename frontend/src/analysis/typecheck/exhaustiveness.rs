//! Algorithm adapted from https://julesjacobs.com/notes/patternmatching/patternmatching.pdf
//! and https://github.com/yorickpeterse/pattern-matching-in-rust

use crate::{
    analysis::{
        self,
        typecheck::{format::format_type, Typechecker},
    },
    diagnostics::Note,
    helpers::InternedString,
    ReachableMarkerId, TypeId, VariableId, VariantIndex,
};
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap, HashSet};

impl Typechecker {
    pub fn check_exhaustiveness(&self, expr: &analysis::Expression) {
        expr.traverse(|expr| match &expr.kind {
            analysis::ExpressionKind::Initialize(pattern, value) => {
                if pattern.contains_error() {
                    return;
                }

                let match_compiler = MatchCompiler {
                    typechecker: self,
                    info: Info::default(),
                };

                let rows = vec![(
                    self.convert_match_pattern(pattern),
                    false,
                    self.compiler.new_reachable_marker_id(), // TODO
                )];

                let result = match_compiler.compile(
                    Variable {
                        id: self.compiler.new_variable_id(),
                        ty: self.convert_ty(&value.ty),
                    },
                    rows,
                );

                if result.info.missing {
                    let mut missing_patterns = result
                        .missing_patterns()
                        .into_iter()
                        .map(|pattern| format!("`{}`", self.format_pattern(&pattern, false)))
                        .sorted()
                        .collect::<Vec<_>>();

                    self.compiler.add_error(
                        "variable assignment not handle all possible values",
                        vec![
                            Note::primary(
                                pattern.span,
                                match missing_patterns.len() {
                                    0 => unreachable!(),
                                    1 => format!(
                                        "the {} pattern is not handled",
                                        missing_patterns.pop().unwrap()
                                    ),
                                    _ => {
                                        let last_missing_pattern = missing_patterns.pop().unwrap();

                                        format!(
                                            "the {} and {} patterns are not handled",
                                            missing_patterns.join(", "),
                                            last_missing_pattern
                                        )
                                    }
                                },
                            ),
                            Note::secondary(pattern.span, "try using a `when` expression instead"),
                        ],
                        "nonexhaustive-variable",
                    );
                }
            }
            analysis::ExpressionKind::Function(pattern, _, _) => {
                if pattern.contains_error() {
                    return;
                }

                let input_ty = match &expr.ty {
                    analysis::Type::Function(input, _) => input.as_ref(),
                    _ => unreachable!(),
                };

                let match_compiler = MatchCompiler {
                    typechecker: self,
                    info: Info::default(),
                };

                let rows = vec![(
                    self.convert_match_pattern(pattern),
                    false,
                    self.compiler.new_reachable_marker_id(), // TODO
                )];

                let result = match_compiler.compile(
                    Variable {
                        id: self.compiler.new_variable_id(),
                        ty: self.convert_ty(input_ty),
                    },
                    rows,
                );

                if result.info.missing {
                    let mut missing_patterns = result
                        .missing_patterns()
                        .into_iter()
                        .map(|pattern| format!("`{}`", self.format_pattern(&pattern, false)))
                        .sorted()
                        .collect::<Vec<_>>();

                    self.compiler.add_error(
                        "function input not handle all possible values",
                        vec![
                            Note::primary(
                                pattern.span,
                                match missing_patterns.len() {
                                    0 => unreachable!(),
                                    1 => format!(
                                        "the {} pattern is not handled",
                                        missing_patterns.pop().unwrap()
                                    ),
                                    _ => {
                                        let last_missing_pattern = missing_patterns.pop().unwrap();

                                        format!(
                                            "the {} and {} patterns are not handled",
                                            missing_patterns.join(", "),
                                            last_missing_pattern
                                        )
                                    }
                                },
                            ),
                            Note::secondary(
                                pattern.span,
                                "try using a `when` expression inside the function instead",
                            ),
                        ],
                        "nonexhaustive-variable",
                    );
                }
            }
            analysis::ExpressionKind::When(input, arms) => {
                let match_compiler = MatchCompiler {
                    typechecker: self,
                    info: Info::default(),
                };

                let mut row_ids = HashMap::new();

                let rows = arms
                    .iter()
                    .filter_map(|arm| {
                        if arm.pattern.contains_error() {
                            return None;
                        }

                        let id = self.compiler.new_reachable_marker_id();

                        row_ids.insert(id, arm.span);

                        Some((
                            self.convert_match_pattern(&arm.pattern),
                            arm.guard.is_some(),
                            id,
                        ))
                    })
                    .collect::<Vec<_>>();

                let result = match_compiler.compile(
                    Variable {
                        id: self.compiler.new_variable_id(),
                        ty: self.convert_ty(&input.ty),
                    },
                    rows,
                );

                if result.info.missing {
                    let mut missing_patterns = result
                        .missing_patterns()
                        .into_iter()
                        .map(|pattern| format!("`{}`", self.format_pattern(&pattern, false)))
                        .sorted()
                        .collect::<Vec<_>>();

                    self.compiler.add_error(
                        format!(
                            "`when` expression does not handle all possible values of {}",
                            self.format_ty(input.ty.clone())
                        ),
                        vec![Note::primary(
                            expr.span,
                            match missing_patterns.len() {
                                0 => unreachable!(),
                                1 => format!(
                                    "try adding a case for the {} pattern",
                                    missing_patterns.pop().unwrap()
                                ),
                                _ => {
                                    let last_missing_pattern = missing_patterns.pop().unwrap();

                                    format!(
                                        "try adding cases for the {} and {} patterns",
                                        missing_patterns.join(", "),
                                        last_missing_pattern
                                    )
                                }
                            },
                        )],
                        "nonexhaustive-when",
                    );
                }

                for id in result.unreachable_arms(row_ids.keys().copied().collect()) {
                    let span = *row_ids.get(&id).unwrap();

                    self.compiler.add_warning(
                        "redundant case in `when` expression",
                        vec![
                            Note::primary(
                                span,
                                "this case will never be executed because a different case already handles the same values",
                            ),
                        ],
                        "redundant-case",
                    );
                }
            }
            _ => {}
        })
    }

    fn convert_match_pattern(&self, pattern: &analysis::Pattern) -> Pattern {
        match &pattern.kind {
            analysis::PatternKind::Error(_) => unreachable!(),
            analysis::PatternKind::Wildcard => Pattern::Binding(None),
            analysis::PatternKind::Variable(variable) => Pattern::Binding(Some(Variable {
                id: *variable,
                ty: self.convert_ty(
                    &self
                        .declarations
                        .borrow()
                        .variables
                        .get(variable)
                        .unwrap()
                        .ty,
                ),
            })),
            analysis::PatternKind::Or(left, right) => Pattern::Or(vec![
                self.convert_match_pattern(left),
                self.convert_match_pattern(right),
            ]),
            analysis::PatternKind::Variant(id, index, patterns) => Pattern::Constructor(
                Constructor::Variant(*id, *index),
                patterns
                    .iter()
                    .map(|pattern| self.convert_match_pattern(pattern))
                    .collect(),
            ),
            analysis::PatternKind::Tuple(patterns) => Pattern::Constructor(
                Constructor::Tuple,
                patterns
                    .iter()
                    .map(|pattern| self.convert_match_pattern(pattern))
                    .collect(),
            ),
            analysis::PatternKind::Destructure(id, fields) => {
                let num_fields = match &self.declarations.borrow().types.get(id).unwrap().kind {
                    analysis::typecheck::TypeDeclKind::Structure { fields, .. } => fields.len(),
                    _ => unreachable!(),
                };

                let mut patterns = vec![Pattern::Binding(None); num_fields];
                for (index, pattern) in fields {
                    patterns[index.0] = self.convert_match_pattern(pattern);
                }

                Pattern::Constructor(Constructor::Structure(*id), patterns)
            }
            analysis::PatternKind::Text(_) => Pattern::Constructor(
                Constructor::Unbounded(analysis::Type::Builtin(
                    analysis::typecheck::BuiltinType::Text,
                )),
                Vec::new(),
            ),
            analysis::PatternKind::Number(_) => Pattern::Constructor(
                Constructor::Unbounded(analysis::Type::Builtin(
                    analysis::typecheck::BuiltinType::Number,
                )),
                Vec::new(),
            ),
            analysis::PatternKind::Integer(_) => Pattern::Constructor(
                Constructor::Unbounded(analysis::Type::Builtin(
                    analysis::typecheck::BuiltinType::Integer,
                )),
                Vec::new(),
            ),
            analysis::PatternKind::Natural(_) => Pattern::Constructor(
                Constructor::Unbounded(analysis::Type::Builtin(
                    analysis::typecheck::BuiltinType::Natural,
                )),
                Vec::new(),
            ),
            analysis::PatternKind::Byte(_) => Pattern::Constructor(
                Constructor::Unbounded(analysis::Type::Builtin(
                    analysis::typecheck::BuiltinType::Byte,
                )),
                Vec::new(),
            ),
            analysis::PatternKind::Signed(_) => Pattern::Constructor(
                Constructor::Unbounded(analysis::Type::Builtin(
                    analysis::typecheck::BuiltinType::Signed,
                )),
                Vec::new(),
            ),
            analysis::PatternKind::Unsigned(_) => Pattern::Constructor(
                Constructor::Unbounded(analysis::Type::Builtin(
                    analysis::typecheck::BuiltinType::Unsigned,
                )),
                Vec::new(),
            ),
            analysis::PatternKind::Float(_) => Pattern::Constructor(
                Constructor::Unbounded(analysis::Type::Builtin(
                    analysis::typecheck::BuiltinType::Float,
                )),
                Vec::new(),
            ),
            analysis::PatternKind::Double(_) => Pattern::Constructor(
                Constructor::Unbounded(analysis::Type::Builtin(
                    analysis::typecheck::BuiltinType::Double,
                )),
                Vec::new(),
            ),
        }
    }

    fn convert_ty(&self, ty: &analysis::Type) -> Type {
        match ty {
            analysis::Type::Named(id, _, analysis::TypeStructure::Marker) => Type::Marker(*id),
            analysis::Type::Named(id, _, analysis::TypeStructure::Enumeration(variants)) => {
                Type::Enumeration(*id, variants.clone())
            }
            analysis::Type::Named(id, _, analysis::TypeStructure::Structure(fields)) => {
                Type::Structure(*id, fields.clone())
            }
            analysis::Type::Named(_, params, analysis::TypeStructure::Recursive(recursive_id)) => {
                let decl = self
                    .declarations
                    .borrow()
                    .types
                    .get(recursive_id)
                    .unwrap()
                    .clone();

                let substitutions = decl
                    .params
                    .into_iter()
                    .zip(params.iter().cloned())
                    .collect::<BTreeMap<_, _>>();

                match decl.kind {
                    analysis::typecheck::TypeDeclKind::Marker => Type::Marker(*recursive_id),
                    analysis::typecheck::TypeDeclKind::Enumeration { variants, .. } => {
                        let variants = variants
                            .into_iter()
                            .map(|variants| {
                                variants
                                    .into_iter()
                                    .map(|(_, mut ty)| {
                                        ty.instantiate_with(&substitutions);
                                        ty
                                    })
                                    .collect::<Vec<_>>()
                            })
                            .collect::<Vec<_>>();

                        Type::Enumeration(*recursive_id, variants)
                    }
                    analysis::typecheck::TypeDeclKind::Structure { fields, .. } => {
                        let fields = fields
                            .into_iter()
                            .map(|(_, mut ty)| {
                                ty.instantiate_with(&substitutions);
                                ty
                            })
                            .collect::<Vec<_>>();

                        Type::Structure(*recursive_id, fields)
                    }
                    analysis::typecheck::TypeDeclKind::Alias(ty) => self.convert_ty(&ty),
                }
            }
            analysis::Type::Tuple(tys) => Type::Tuple(tys.clone()),
            _ => Type::Unmatchable(ty.clone()),
        }
    }

    fn format_pattern(&self, pattern: &Pattern, parenthesize: bool) -> String {
        match pattern {
            Pattern::Binding(_) => String::from("_"),
            Pattern::Constructor(Constructor::Variant(id, index), patterns) => {
                let ty = self.declarations.borrow().types.get(id).unwrap().clone();
                let variant_names = match ty.kind {
                    analysis::typecheck::TypeDeclKind::Enumeration { variant_names, .. } => {
                        variant_names
                    }
                    _ => return String::from("_"),
                };

                let variant_name = variant_names
                    .iter()
                    .find_map(|(name, i)| (i == index).then_some(name.as_str()))
                    .unwrap_or("<unknown>");

                let formatted = if patterns.is_empty() {
                    variant_name.to_string()
                } else {
                    format!(
                        "{} {}",
                        variant_name,
                        patterns
                            .iter()
                            .map(|pattern| self.format_pattern(pattern, true,))
                            .collect::<Vec<_>>()
                            .join(" ")
                    )
                };

                if parenthesize && !patterns.is_empty() {
                    format!("({formatted})")
                } else {
                    formatted
                }
            }
            Pattern::Constructor(Constructor::Tuple, patterns) => {
                let formatted = patterns
                    .iter()
                    .map(|pattern| self.format_pattern(pattern, true))
                    .collect::<Vec<_>>()
                    .join(" , ");

                if parenthesize {
                    format!("({formatted})")
                } else {
                    formatted
                }
            }
            Pattern::Constructor(Constructor::Structure(id), patterns) => {
                let ty = self.declarations.borrow().types.get(id).unwrap().clone();
                let field_names = match ty.kind {
                    analysis::typecheck::TypeDeclKind::Structure { field_names, .. } => field_names,
                    _ => return String::from("_"),
                };

                let fields = patterns
                    .iter()
                    .enumerate()
                    .filter_map(|(index, pattern)| {
                        let field_name = field_names
                            .iter()
                            .find_map(|(name, i)| (i.0 == index).then_some(name.as_str()))
                            .unwrap_or("<unknown>");

                        match pattern {
                            Pattern::Binding(Some(variable)) => {
                                let name = self
                                    .declarations
                                    .borrow()
                                    .variables
                                    .get(&variable.id)
                                    .unwrap()
                                    .name
                                    .map_or("<unknown>", |s| s.as_str());

                                Some(if name == field_name {
                                    name.to_string()
                                } else {
                                    format!("{name} : {field_name}")
                                })
                            }
                            Pattern::Binding(None) => None,
                            _ => Some(format!(
                                "{} : {}",
                                field_name,
                                self.format_pattern(pattern, false)
                            )),
                        }
                    })
                    .collect::<Vec<_>>();

                if fields.is_empty() {
                    String::from("{}")
                } else {
                    format!("{{ {} }}", fields.join(" ; "))
                }
            }
            Pattern::Constructor(Constructor::Marker(id), _) => {
                let ty = self.declarations.borrow().types.get(id).unwrap().clone();
                ty.name.to_string()
            }
            Pattern::Constructor(Constructor::Unbounded(ty), _) => self.format_ty(ty.clone()),
            Pattern::Or(_) => todo!(),
        }
    }

    fn format_ty(&self, ty: analysis::Type) -> String {
        macro_rules! getter {
            ($kind:ident, $f:expr) => {
                |id| $f(self.declarations.borrow().$kind.get(&id).unwrap().name)
            };
        }

        let type_names = getter!(types, |name: InternedString| name.to_string());
        let trait_names = getter!(traits, |name: InternedString| name.to_string());
        let param_names = getter!(type_parameters, |name: Option<_>| {
            name.as_ref().map(ToString::to_string)
        });

        format_type(
            ty,
            type_names,
            trait_names,
            param_names,
            analysis::typecheck::format::Format {
                surround_in_backticks: true,
                type_function: analysis::typecheck::format::TypeFunctionFormat::Description,
                type_variable: analysis::typecheck::format::TypeVariableFormat::Description,
            },
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Type {
    Enumeration(TypeId, Vec<Vec<analysis::Type>>),
    Structure(TypeId, Vec<analysis::Type>),
    Tuple(Vec<analysis::Type>),
    Marker(TypeId),
    Unmatchable(analysis::Type),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Constructor {
    Variant(TypeId, VariantIndex),
    Tuple,
    Structure(TypeId),
    Marker(TypeId),
    Unbounded(analysis::Type),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Variable {
    id: VariableId,
    ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Pattern {
    Constructor(Constructor, Vec<Pattern>),
    Binding(Option<Variable>),
    Or(Vec<Pattern>),
}

impl Pattern {
    fn flatten_or(self, row: Row) -> Vec<(Pattern, Row)> {
        if let Pattern::Or(args) = self {
            args.into_iter()
                .map(|pattern| (pattern, row.clone()))
                .collect()
        } else {
            vec![(self, row)]
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Row {
    columns: Vec<Column>,
    guard: bool,
    reachable: ReachableMarkerId,
}

impl Row {
    fn remove_column(&mut self, variable: &Variable) -> Option<Column> {
        self.columns
            .iter()
            .position(|column| &column.variable == variable)
            .map(|index| self.columns.remove(index))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Column {
    variable: Variable,
    pattern: Pattern,
}

#[derive(Debug, PartialEq, Eq)]
struct Case {
    constructor: Constructor,
    arguments: Vec<Variable>,
    body: Decision,
}

#[derive(Debug, PartialEq, Eq)]
enum Decision {
    Matched(ReachableMarkerId),
    NotMatched,
    Guard(ReachableMarkerId, Box<Decision>),
    Switch(Variable, Vec<Case>, Option<Box<Decision>>),
}

struct Match {
    tree: Decision,
    info: Info,
}

#[derive(Debug, Default)]
struct Info {
    missing: bool,
    reachable: Vec<ReachableMarkerId>,
}

struct Term {
    variable: Variable,
    constructor: Constructor,
    arguments: Vec<Variable>,
}

impl Term {
    fn pattern(&self, terms: &[Term], mapping: &HashMap<Variable, VariantIndex>) -> Pattern {
        Pattern::Constructor(
            self.constructor.clone(),
            self.arguments
                .iter()
                .map(|argument| {
                    mapping
                        .get(argument)
                        .map(|index| terms[index.0].pattern(terms, mapping))
                        .unwrap_or(Pattern::Binding(None))
                })
                .collect(),
        )
    }
}

impl Match {
    fn missing_patterns(&self) -> Vec<Pattern> {
        fn add_missing_patterns(
            node: &Decision,
            terms: &mut Vec<Term>,
            missing: &mut HashSet<Pattern>,
        ) {
            match node {
                Decision::Matched(_) => {}
                Decision::NotMatched => {
                    let mut mapping = HashMap::new();
                    for (index, step) in terms.iter().enumerate() {
                        mapping.insert(step.variable.clone(), VariantIndex(index));
                    }

                    let constructor = terms
                        .first()
                        .map(|term| term.pattern(terms, &mapping))
                        .unwrap_or(Pattern::Binding(None));

                    missing.insert(constructor);
                }
                Decision::Guard(_, fallback) => {
                    add_missing_patterns(fallback, terms, missing);
                }
                Decision::Switch(variable, cases, fallback) => {
                    for case in cases {
                        terms.push(Term {
                            variable: variable.clone(),
                            constructor: case.constructor.clone(),
                            arguments: case.arguments.clone(),
                        });

                        add_missing_patterns(&case.body, terms, missing);
                        terms.pop();
                    }

                    if let Some(node) = fallback {
                        add_missing_patterns(node, terms, missing);
                    }
                }
            }
        }

        let mut patterns = HashSet::new();
        add_missing_patterns(&self.tree, &mut Vec::new(), &mut patterns);

        patterns.into_iter().collect()
    }

    fn unreachable_arms(&self, arms: Vec<ReachableMarkerId>) -> Vec<ReachableMarkerId> {
        HashSet::<ReachableMarkerId>::from_iter(arms)
            .difference(&HashSet::from_iter(self.info.reachable.iter().copied()))
            .copied()
            .collect()
    }
}

struct MatchCompiler<'a> {
    typechecker: &'a Typechecker,
    info: Info,
}

impl<'a> MatchCompiler<'a> {
    fn compile(mut self, input: Variable, arms: Vec<(Pattern, bool, ReachableMarkerId)>) -> Match {
        let rows = arms
            .into_iter()
            .map(|(pattern, guard, body)| Row {
                columns: vec![Column {
                    variable: input.clone(),
                    pattern,
                }],
                guard,
                reachable: body,
            })
            .collect();

        let tree = self.compile_rows(rows);

        Match {
            tree,
            info: self.info,
        }
    }

    fn compile_rows(&mut self, rows: Vec<Row>) -> Decision {
        if rows.is_empty() {
            self.info.missing = true;
            return Decision::NotMatched;
        }

        let mut rows = rows
            .into_iter()
            .map(|row| Row {
                columns: row
                    .columns
                    .into_iter()
                    .filter(|col| !matches!(col.pattern, Pattern::Binding(_)))
                    .collect(),
                ..row
            })
            .collect::<Vec<_>>();

        if rows.first().unwrap().columns.is_empty() {
            let row = rows.remove(0);

            self.info.reachable.push(row.reachable);

            return if row.guard {
                Decision::Guard(row.reachable, Box::new(self.compile_rows(rows)))
            } else {
                Decision::Matched(row.reachable)
            };
        }

        let branch_var = self.branch_variable(rows.first().unwrap(), &rows).clone();
        match &branch_var.ty {
            Type::Enumeration(id, variants) => {
                let cases = variants
                    .iter()
                    .enumerate()
                    .map(|(index, tys)| {
                        (
                            Constructor::Variant(*id, VariantIndex(index)),
                            self.new_variables(
                                tys.iter()
                                    .map(|ty| self.typechecker.convert_ty(ty))
                                    .collect::<Vec<_>>(),
                            ),
                            Vec::new(),
                        )
                    })
                    .collect();

                let cases = self.compile_constructor_cases(rows, &branch_var, cases);

                Decision::Switch(branch_var, cases, None)
            }
            Type::Structure(id, fields) => {
                let cases = vec![(
                    Constructor::Structure(*id),
                    self.new_variables(
                        fields
                            .iter()
                            .map(|ty| self.typechecker.convert_ty(ty))
                            .collect::<Vec<_>>(),
                    ),
                    Vec::new(),
                )];

                let cases = self.compile_constructor_cases(rows, &branch_var, cases);

                Decision::Switch(branch_var, cases, None)
            }
            Type::Tuple(tys) => {
                let cases = vec![(
                    Constructor::Tuple,
                    self.new_variables(
                        tys.iter()
                            .map(|ty| self.typechecker.convert_ty(ty))
                            .collect::<Vec<_>>(),
                    ),
                    Vec::new(),
                )];

                let cases = self.compile_constructor_cases(rows, &branch_var, cases);

                Decision::Switch(branch_var, cases, None)
            }
            Type::Marker(id) => {
                let cases = vec![(Constructor::Marker(*id), Vec::new(), Vec::new())];
                let cases = self.compile_constructor_cases(rows, &branch_var, cases);
                Decision::Switch(branch_var, cases, None)
            }
            Type::Unmatchable(ty) => {
                let (cases, fallback) = self.compile_unbounded_cases(ty, rows, &branch_var);
                Decision::Switch(branch_var, cases, Some(Box::new(fallback)))
            }
        }
    }

    fn compile_constructor_cases(
        &mut self,
        rows: Vec<Row>,
        branch_var: &Variable,
        mut cases: Vec<(Constructor, Vec<Variable>, Vec<Row>)>,
    ) -> Vec<Case> {
        for mut row in rows {
            if let Some(column) = row.remove_column(branch_var) {
                for (pattern, row) in column.pattern.flatten_or(row) {
                    if let Pattern::Constructor(constructor, arguments) = pattern {
                        let index = match constructor {
                            Constructor::Variant(_, index) => index.0,
                            _ => 0,
                        };

                        let mut columns = row.columns;
                        for (variable, pattern) in cases[index].1.iter().zip(arguments.into_iter())
                        {
                            columns.push(Column {
                                variable: variable.clone(),
                                pattern,
                            });
                        }

                        cases[index].2.push(Row { columns, ..row });
                    }
                }
            } else {
                for (_, _, rows) in &mut cases {
                    rows.push(row.clone());
                }
            }
        }

        cases
            .into_iter()
            .map(|(constructor, variables, rows)| Case {
                constructor,
                arguments: variables,
                body: self.compile_rows(rows),
            })
            .collect()
    }

    fn compile_unbounded_cases(
        &mut self,
        ty: &analysis::Type,
        rows: Vec<Row>,
        branch_var: &Variable,
    ) -> (Vec<Case>, Decision) {
        let mut raw_cases: Vec<(Constructor, Vec<Variable>, Vec<Row>)> = Vec::new();
        let mut fallback_rows = Vec::new();

        for mut row in rows {
            if let Some(col) = row.remove_column(branch_var) {
                for (_, row) in col.pattern.flatten_or(row) {
                    let cons = Constructor::Unbounded(ty.clone());
                    raw_cases.push((cons, Vec::new(), vec![row]));
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
            .map(|(cons, vars, rows)| Case {
                constructor: cons,
                arguments: vars,
                body: self.compile_rows(rows),
            })
            .collect();

        (cases, self.compile_rows(fallback_rows))
    }

    // TODO: This may not be necessary if we're just checking for
    // exhasutiveness; we may be able to just use the first column's variable
    // instead
    fn branch_variable<'r>(&self, row: &'r Row, rows: &'r [Row]) -> &'r Variable {
        let mut counts = HashMap::new();
        for row in rows {
            for column in &row.columns {
                *counts.entry(&column.variable).or_insert(0usize) += 1;
            }
        }

        row.columns
            .iter()
            .map(|column| &column.variable)
            .max_by_key(|var| counts[var])
            .unwrap()
    }

    fn new_variables(&mut self, tys: impl IntoIterator<Item = Type>) -> Vec<Variable> {
        tys.into_iter()
            .map(|ty| Variable {
                id: self.typechecker.compiler.new_variable_id(),
                ty,
            })
            .collect()
    }
}
