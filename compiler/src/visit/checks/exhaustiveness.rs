use crate::{
    database::{Db, Fact, NodeRef, Render},
    nodes::{EnumerationVariants, StructureFields},
    typecheck::{ConstructedType, ConstructedTypeTag, Substitutions, Type, Typed},
    visit::{Defined, Definition, TypeDefinition},
};
use itertools::Itertools;
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MatchPathSegment {
    Match,
    NoMatch,
    Field(NodeRef),
    TupleElement(usize, usize),
    Variant(NodeRef),
    VariantElement(NodeRef, usize, usize),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct MatchPath(pub Vec<MatchPathSegment>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MatchTree {
    Wildcard,
    Field(NodeRef, Box<MatchTree>),
    Tuple(Vec<MatchTree>),
    Variant(NodeRef, Vec<MatchTree>),
}

impl MatchTree {
    fn from_path(path: MatchPath) -> Option<Self> {
        let mut coverage = MatchTree::Wildcard;

        for segment in path.0.into_iter().rev() {
            match segment {
                MatchPathSegment::Match | MatchPathSegment::NoMatch => {}
                MatchPathSegment::Field(field) => {
                    coverage = MatchTree::Field(field, Box::new(coverage));
                }
                MatchPathSegment::TupleElement(index, len) => {
                    let mut elements = vec![MatchTree::Wildcard; len];
                    elements[index] = coverage;
                    coverage = MatchTree::Tuple(elements);
                }
                MatchPathSegment::Variant(variant) => {
                    coverage = MatchTree::Variant(variant, Vec::new());
                }
                MatchPathSegment::VariantElement(variant, index, len) => {
                    let mut elements = vec![MatchTree::Wildcard; len];
                    elements[index] = coverage;
                    coverage = MatchTree::Variant(variant, elements);
                }
            }
        }

        Some(coverage)
    }

    fn merge(&mut self, other: Self) {
        match (&mut *self, other) {
            (MatchTree::Wildcard, other) => *self = other,
            (_, MatchTree::Wildcard) => {}
            (
                MatchTree::Field(left_field, left_value),
                MatchTree::Field(right_field, right_value),
            ) if *left_field == right_field => {
                left_value.merge(*right_value);
            }
            (MatchTree::Tuple(left_elements), MatchTree::Tuple(right_elements))
                if left_elements.len() == right_elements.len() =>
            {
                for (left, right) in left_elements.iter_mut().zip(right_elements) {
                    left.merge(right);
                }
            }
            (
                MatchTree::Variant(left_variant, left_elements),
                MatchTree::Variant(right_variant, right_elements),
            ) if *left_variant == right_variant && left_elements.len() == right_elements.len() => {
                for (left, right) in left_elements.iter_mut().zip(right_elements) {
                    left.merge(right);
                }
            }
            _ => {}
        }
    }
}

impl Render for MatchTree {
    fn write(&self, w: &mut dyn std::fmt::Write, db: &Db) -> std::fmt::Result {
        write!(w, "{}", self.to_string(db, true))
    }

    fn link(&self) -> Option<&NodeRef> {
        match self {
            MatchTree::Field(node, _) | MatchTree::Variant(node, _) => Some(node),
            _ => None,
        }
    }
}

impl MatchTree {
    fn to_string(&self, db: &Db, root: bool) -> String {
        match self {
            MatchTree::Wildcard => String::from("_"),
            MatchTree::Field(field, inner) => {
                format!(
                    "{{ {}: {} }}",
                    db.render_nested(field),
                    inner.to_string(db, false)
                )
            }
            MatchTree::Tuple(elements) => {
                let elements = elements.iter().map(|e| e.to_string(db, false)).join("; ");
                format!("({})", elements)
            }
            MatchTree::Variant(variant, elements) => {
                if elements.is_empty() {
                    if root {
                        db.render(variant).to_string()
                    } else {
                        db.render_nested(variant).to_string()
                    }
                } else {
                    let elements = elements.iter().map(|e| e.to_string(db, false)).join(" ");

                    if root {
                        format!("{} {}", db.render(variant), elements)
                    } else {
                        format!("({} {})", db.render_nested(variant), elements)
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Matches {
    pub value: NodeRef,
    pub arm: Option<NodeRef>,
    pub path: Option<MatchPath>,
}

impl Fact for Matches {}

impl Render for Matches {}

#[derive(Debug, Clone, Default)]
pub struct MissingPatterns(pub Vec<MatchTree>);

impl Fact for MissingPatterns {}

impl Render for MissingPatterns {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is missing patterns")
    }
}

impl Db {
    pub fn check_exhaustiveness(&mut self) {
        let mut values: BTreeMap<NodeRef, BTreeMap<NodeRef, Vec<MatchPath>>> = Default::default();

        for (_, Matches { value, arm, path }) in self.iter::<Matches>() {
            if let Some(arm) = arm
                && let Some(path) = path
            {
                values
                    .entry(value)
                    .or_default()
                    .entry(arm)
                    .or_default()
                    .push(path);
            }
        }

        for (value, groups) in values {
            let paths = groups
                .into_values()
                .filter(|group| !group.is_empty())
                .collect::<Vec<_>>();

            let max = paths
                .iter()
                .filter_map(|group| group.iter().map(|path| path.0.len()).max())
                .max()
                .unwrap_or(0);

            let expected = self
                .get_type(&value)
                .map(|ty| self.collect_paths(ty, &Substitutions::new(), &[], max))
                .unwrap_or_default();

            let missing = expected
                .into_iter()
                .filter(|expected| {
                    !paths
                        .iter()
                        .any(|actual| matches_coverage(actual, expected))
                })
                .collect::<Vec<_>>();

            if !missing.is_empty() {
                let mut missing = missing
                    .into_iter()
                    .map(|coverage| {
                        coverage.into_iter().filter_map(MatchTree::from_path).fold(
                            MatchTree::Wildcard,
                            |mut result, tree| {
                                result.merge(tree);
                                result
                            },
                        )
                    })
                    .collect::<Vec<_>>();

                missing.sort();

                self.insert(&value, MissingPatterns(missing));
            }
        }
    }

    fn collect_paths(
        &self,
        mut ty: ConstructedType,
        substitutions: &Substitutions,
        prefix: &[MatchPathSegment],
        max: usize,
    ) -> Vec<Vec<MatchPath>> {
        if prefix.len() >= max {
            return vec![vec![MatchPath(prefix.to_vec())]];
        }

        if let ConstructedTypeTag::Parameter(parameter) = &ty.tag
            && let Some(Type::Constructed(substitution)) = substitutions.get(parameter)
        {
            ty = substitution;
        }

        match ty.tag {
            ConstructedTypeTag::Named(definition) => {
                let Some(Defined(Definition::Type(TypeDefinition { parameters, .. }))) =
                    self.get(&definition)
                else {
                    return Vec::new();
                };

                for (parameter, ty) in parameters.into_iter().zip(ty.children) {
                    substitutions.insert(parameter, ty);
                }

                if let Some(StructureFields(fields)) = self.get(&definition) {
                    fields
                        .into_iter()
                        .map(|field| {
                            let Some(ty) = self.get_type(&field) else {
                                return Vec::new();
                            };

                            let mut prefix = prefix.to_vec();
                            prefix.push(MatchPathSegment::Field(field));

                            self.collect_paths(ty, substitutions, &prefix, max)
                        })
                        .multi_cartesian_product()
                        .map(|combination| combination.into_iter().flatten().collect())
                        .collect()
                } else if let Some(EnumerationVariants(variants)) = self.get(&definition) {
                    variants
                        .into_iter()
                        .flat_map(|(variant, fields)| {
                            if fields.is_empty() {
                                let mut prefix = prefix.to_vec();
                                prefix.push(MatchPathSegment::Variant(variant));

                                vec![vec![MatchPath(prefix)]]
                            } else {
                                fields
                                    .iter()
                                    .enumerate()
                                    .map(|(index, field)| {
                                        let Some(field_ty) = self.get_type(field) else {
                                            return Vec::new();
                                        };

                                        let mut prefix = prefix.to_vec();
                                        prefix.push(MatchPathSegment::VariantElement(
                                            variant.clone(),
                                            index,
                                            fields.len(),
                                        ));

                                        self.collect_paths(field_ty, substitutions, &prefix, max)
                                    })
                                    .multi_cartesian_product()
                                    .map(|combination| combination.into_iter().flatten().collect())
                                    .collect()
                            }
                            .into_iter()
                        })
                        .collect()
                } else {
                    Vec::new()
                }
            }
            ConstructedTypeTag::Tuple => {
                let len = ty.children.len();

                ty.children
                    .into_iter()
                    .enumerate()
                    .map(|(index, element)| {
                        let Type::Constructed(element) = element else {
                            return Vec::new();
                        };

                        let mut prefix = prefix.to_vec();
                        prefix.push(MatchPathSegment::TupleElement(index, len));

                        self.collect_paths(element, substitutions, &prefix, max)
                    })
                    .multi_cartesian_product()
                    .map(|combination| combination.into_iter().flatten().collect())
                    .collect()
            }
            _ => {
                let mut prefix = prefix.to_vec();
                prefix.push(MatchPathSegment::Match);

                vec![vec![MatchPath(prefix)]]
            }
        }
    }

    fn get_type(&self, node: &NodeRef) -> Option<ConstructedType> {
        let Some(Typed { group: Some(group) }) = self.get(node) else {
            return None;
        };

        let [ty] = group.types.as_slice() else {
            return None;
        };

        Some(ty.clone())
    }
}

fn matches_coverage(actual: &[MatchPath], expected: &[MatchPath]) -> bool {
    expected.iter().all(|expected| {
        actual.iter().any(|actual| {
            for (segment, other) in actual.0.iter().zip(&expected.0) {
                let matches = match (segment, other) {
                    (MatchPathSegment::Match, _) => {
                        return true; // short-circuit
                    }
                    (MatchPathSegment::Field(field), MatchPathSegment::Field(other_field)) => {
                        field == other_field
                    }
                    (
                        MatchPathSegment::TupleElement(index, len),
                        MatchPathSegment::TupleElement(other_index, other_len),
                    ) => index == other_index && len == other_len,
                    (
                        MatchPathSegment::Variant(variant),
                        MatchPathSegment::Variant(other_variant),
                    ) => variant == other_variant,
                    (
                        MatchPathSegment::VariantElement(variant, index, len),
                        MatchPathSegment::VariantElement(other_variant, other_index, other_len),
                    ) => variant == other_variant && index == other_index && len == other_len,
                    _ => false,
                };

                if !matches {
                    return false;
                }
            }

            true
        })
    })
}
