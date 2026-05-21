use crate::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::{
        groups::update_type,
        ty::{Ty, TyTag},
    },
    visit::definitions::{Defined, TypeDefinition},
};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, ops::ControlFlow};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarkerType {
    pub constructor: Node,
}

#[typetag::serde]
impl Fact for MarkerType {}

impl Render for MarkerType {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructureFields {
    pub constructor: Node,
    pub fields: Vec<Node>,
}

#[typetag::serde]
impl Fact for StructureFields {}

impl Render for StructureFields {}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EnumerationVariants(pub Vec<(Node, Vec<Node>)>);

#[typetag::serde]
impl Fact for EnumerationVariants {}

impl Render for EnumerationVariants {}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum MatchPathSegment {
    Match,
    NoMatch,
    Field(Node),
    TupleElement(usize, usize),
    Variant(Node),
    VariantElement(Node, usize, usize),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct MatchPath(pub Vec<MatchPathSegment>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum MatchTree {
    Wildcard,
    Field(Node, Box<MatchTree>),
    Tuple(Vec<MatchTree>),
    Variant(Node, Vec<MatchTree>),
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
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
        let node = match self {
            MatchTree::Field(node, _) | MatchTree::Variant(node, _) => Some(node),
            _ => None,
        };

        if let Some(node) = node {
            ctx.link(self.to_string(db, true), *node);
        } else {
            ctx.string(self.to_string(db, true));
        }
    }
}

impl MatchTree {
    fn to_string(&self, db: &Db, root: bool) -> String {
        match self {
            MatchTree::Wildcard => String::from("_"),
            MatchTree::Field(field, inner) => {
                let field_name = db.get::<Defined>(*field).unwrap().0.name().unwrap();
                format!("{{ {}: {} }}", field_name, inner.to_string(db, false))
            }
            MatchTree::Tuple(elements) => {
                let elements = elements.iter().map(|e| e.to_string(db, false)).join("; ");
                format!("({elements})")
            }
            MatchTree::Variant(variant, elements) => {
                let variant_name = db.get::<Defined>(*variant).unwrap().0.name().unwrap();

                if elements.is_empty() {
                    variant_name.to_string()
                } else {
                    let elements = elements.iter().map(|e| e.to_string(db, false)).join(" ");

                    if root {
                        format!("{variant_name} {elements}")
                    } else {
                        format!("({variant_name} {elements})")
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Matches {
    pub value: Node,
    pub arm: Option<Node>,
    pub path: Option<MatchPath>,
}

#[typetag::serde]
impl Fact for Matches {}

impl Render for Matches {}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct MissingPatterns(pub Vec<MatchTree>);

#[typetag::serde]
impl Fact for MissingPatterns {}

impl Render for MissingPatterns {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("is missing patterns");
    }
}

pub fn check_exhaustiveness(db: &mut Db) {
    let mut values: BTreeMap<Node, BTreeMap<Node, Vec<MatchPath>>> = Default::default();

    db.for_each_fact::<Matches, ()>(&mut |_, _, Matches { value, arm, path }| {
        if let Some(arm) = arm
            && let Some(path) = path
        {
            values
                .entry(*value)
                .or_default()
                .entry(*arm)
                .or_default()
                .push(path.clone());
        }

        ControlFlow::Continue(())
    });

    for (value, groups) in values {
        let actual = groups
            .into_values()
            .filter(|group| !group.is_empty())
            .collect::<Vec<_>>();

        let max = actual
            .iter()
            .filter_map(|group| group.iter().map(|path| path.0.len()).max())
            .max()
            .unwrap_or(0);

        let expected = collect_paths(db, Ty::Node(value), &BTreeMap::new(), &[], max);

        let missing = expected
            .into_iter()
            .filter(|expected| {
                !actual
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

            db.insert(value, MissingPatterns(missing));
        }
    }
}

fn collect_paths(
    db: &Db,
    ty: Ty,
    parameters: &BTreeMap<Node, Ty>,
    prefix: &[MatchPathSegment],
    max: usize,
) -> Vec<Vec<MatchPath>> {
    if prefix.len() >= max {
        return vec![vec![MatchPath(prefix.to_vec())]];
    }

    let mut ty = update_type(db, &ty);

    if let Ty::Constructed(inner) = &ty
        && let TyTag::Parameter(parameter) = inner.tag
        && let Some(substitution) = parameters.get(&parameter)
    {
        ty = substitution.clone();
    }

    let Ty::Constructed(ty) = update_type(db, &ty) else {
        return Vec::new();
    };

    match ty.tag {
        TyTag::Named(definition) => {
            let Some(TypeDefinition {
                attributes,
                parameters: type_parameters,
                ..
            }) = db
                .get(definition)
                .and_then(|Defined(definition)| definition.downcast_ref())
            else {
                return Vec::new();
            };

            let mut parameters = parameters.clone();
            for (parameter, ty) in type_parameters.iter().zip(ty.children) {
                parameters.insert(*parameter, ty);
            }

            if attributes.intrinsic {
                let mut prefix = prefix.to_vec();
                prefix.push(MatchPathSegment::NoMatch);

                vec![vec![MatchPath(prefix)]]
            } else if let Some(StructureFields { fields, .. }) = db.get(definition) {
                fields
                    .iter()
                    .map(|&field| {
                        let mut prefix = prefix.to_vec();
                        prefix.push(MatchPathSegment::Field(field));

                        collect_paths(db, Ty::Node(field), &parameters, &prefix, max)
                    })
                    .multi_cartesian_product()
                    .map(|combination| combination.into_iter().flatten().collect())
                    .collect()
            } else if let Some(EnumerationVariants(variants)) = db.get(definition) {
                variants
                    .iter()
                    .flat_map(|&(variant, ref fields)| {
                        if fields.is_empty() {
                            let mut prefix = prefix.to_vec();
                            prefix.push(MatchPathSegment::Variant(variant));

                            vec![vec![MatchPath(prefix)]]
                        } else {
                            fields
                                .iter()
                                .enumerate()
                                .map(|(index, &field)| {
                                    let mut prefix = prefix.to_vec();
                                    prefix.push(MatchPathSegment::VariantElement(
                                        variant,
                                        index,
                                        fields.len(),
                                    ));

                                    collect_paths(db, Ty::Node(field), &parameters, &prefix, max)
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
        TyTag::Tuple => {
            let len = ty.children.len();

            ty.children
                .into_iter()
                .enumerate()
                .map(|(index, element)| {
                    let mut prefix = prefix.to_vec();
                    prefix.push(MatchPathSegment::TupleElement(index, len));

                    collect_paths(db, element, parameters, &prefix, max)
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

fn matches_coverage(actual: &[MatchPath], expected: &[MatchPath]) -> bool {
    expected.iter().all(|expected| {
        actual.iter().any(|actual| {
            for (segment, other) in actual.0.iter().zip(&expected.0) {
                match (segment, other) {
                    (MatchPathSegment::Match, _) => {
                        return true; // short-circuit
                    }
                    (MatchPathSegment::Field(field), MatchPathSegment::Field(other_field))
                        if field != other_field =>
                    {
                        return true; // allow omitted fields
                    }
                    (MatchPathSegment::Field(field), MatchPathSegment::Field(other_field))
                        if field == other_field => {}
                    (
                        MatchPathSegment::TupleElement(index, len),
                        MatchPathSegment::TupleElement(other_index, other_len),
                    ) if index == other_index && len == other_len => {}
                    (
                        MatchPathSegment::Variant(variant),
                        MatchPathSegment::Variant(other_variant),
                    ) if variant == other_variant => {}
                    (
                        MatchPathSegment::VariantElement(variant, index, len),
                        MatchPathSegment::VariantElement(other_variant, other_index, other_len),
                    ) if variant == other_variant && index == other_index && len == other_len => {}
                    _ => return false,
                }
            }

            true
        })
    })
}
