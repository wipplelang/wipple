use crate::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::{
        instantiate::Instantiated,
        ty::{ConstructedTy, Ty},
    },
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Group {
    pub nodes: Vec<Node>,
    pub tys: Vec<ConstructedTy>,
}

impl Group {
    pub fn with_nodes(nodes: impl IntoIterator<Item = Node>) -> Self {
        Group {
            nodes: Vec::from_iter(nodes),
            ..Default::default()
        }
    }

    pub fn unify(
        &mut self,
        db: &mut Db,
        tys: &[ConstructedTy],
        representative: Option<Node>,
        mut unify: impl FnMut(&mut Db, &ConstructedTy, &ConstructedTy) -> bool,
    ) {
        // Unify against the representative type
        let Some(mut ty) = representative
            .and_then(|representative| representative_ty(db, tys, representative))
            .or_else(|| tys.first())
            .cloned()
        else {
            return;
        };

        let index = tys.iter().position(|other| *other == ty).unwrap();

        if ty.representative.is_none() {
            ty.representative = representative;
        }

        if let Some(existing) = self.tys.iter_mut().find(|existing| **existing == ty) {
            // If the type already exists in `self`, update its representative
            let representative = match (existing.representative, ty.representative) {
                (None, representative) | (representative, None) => representative,
                (Some(existing), Some(representative)) => {
                    // Prefer nodes that aren't type parameters
                    if db.contains::<Instantiated>(existing)
                        && !db.contains::<Instantiated>(representative)
                    {
                        Some(representative)
                    } else {
                        Some(existing)
                    }
                }
            };

            existing.representative = representative;
        }

        if self.tys.is_empty() || !unify(db, &self.tys[0], &ty) {
            self.tys.push(ty.clone());
        }

        // Add the remaining types as-is
        for (other_index, other) in tys.iter().enumerate() {
            if index != other_index {
                self.tys.push(other.clone());
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Groups(BTreeMap<usize, Option<Group>>);

impl Groups {
    pub fn index_of(&self, node: Node) -> Option<usize> {
        self.0
            .iter()
            .find(|(_, slot)| slot.as_ref().is_some_and(|slot| slot.nodes.contains(&node)))
            .map(|(index, _)| *index)
    }

    pub fn get(&self, index: usize) -> &Group {
        self.0.get(&index).unwrap().as_ref().unwrap()
    }

    pub fn get_mut(&mut self, index: usize) -> &mut Group {
        self.0.get_mut(&index).unwrap().as_mut().unwrap()
    }

    pub fn insert(&mut self, group: Group) -> usize {
        let index = self
            .0
            .iter()
            .find(|(_, slot)| slot.is_none())
            .map(|(index, _)| *index)
            .unwrap_or_else(|| self.0.len());

        self.0.insert(index, Some(group));

        index
    }

    pub fn insert_existing(&mut self, index: usize, group: Group) {
        self.0.insert(index, Some(group));
    }

    pub fn remove_existing(&mut self, index: usize) -> Group {
        self.0.insert(index, None).unwrap().unwrap()
    }

    pub fn merge(
        db: &mut Db,
        old_group: Group,
        new_group: &mut Group,
        unify: impl FnMut(&mut Db, &ConstructedTy, &ConstructedTy) -> bool,
    ) {
        new_group.nodes.extend(old_group.nodes);
        new_group.unify(db, &old_group.tys, None, unify);
    }

    pub fn indices(&self) -> impl Iterator<Item = usize> {
        self.0
            .iter()
            .filter(|(_, slot)| slot.is_some())
            .map(|(index, _)| *index)
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Group> {
        self.0.values_mut().flatten()
    }

    pub fn into_sorted_groups<K: Ord>(self, mut key: impl FnMut(Node) -> K) -> Vec<Group> {
        let mut groups = self.0.into_values().flatten().collect::<Vec<_>>();

        for group in &mut groups {
            group.nodes.sort_by_key(|node| key(*node));
        }

        groups
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Typed(pub Option<Group>);

#[typetag::serde]
impl Fact for Typed {}

impl Render for Typed {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx<'_>) {
        let Some(group) = &self.0 else {
            ctx.string("types not solved");
            return;
        };

        if group.tys.is_empty() {
            ctx.string("missing type");

            if group.nodes.len() > 1 {
                ctx.string(" (group: ");

                for (index, node) in group.nodes.iter().enumerate() {
                    if index > 0 {
                        ctx.string(", ");
                    }

                    ctx.node(*node);
                }

                ctx.string(")");
            }
        } else {
            ctx.string("has type ");

            for (index, ty) in group.tys.iter().enumerate() {
                if index > 0 {
                    ctx.string(" or ");
                }

                ctx.render(db, &Ty::Constructed(ty.clone()));
            }
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Annotated;

#[typetag::serde]
impl Fact for Annotated {}

impl Render for Annotated {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx<'_>) {
        ctx.string("type is annotated");
    }
}

pub fn types_of(db: &Db, node: Node) -> &[ConstructedTy] {
    db.get(node)
        .and_then(|Typed(group)| group.as_ref())
        .map(|group| group.tys.as_slice())
        .unwrap_or_default()
}

pub fn update_type(db: &Db, ty: &Ty, representative: impl Into<Option<Node>>) -> (Ty, Vec<Ty>) {
    match ty {
        Ty::Node(node) => {
            let tys = types_of(db, *node);

            if tys.is_empty() {
                (Ty::Node(*node), Vec::new())
            } else {
                (
                    Ty::Constructed(
                        representative_ty(db, tys, representative.into().unwrap_or(*node))
                            .unwrap_or_else(|| tys.first().unwrap())
                            .clone(),
                    ),
                    tys.iter().cloned().map(Ty::Constructed).collect(),
                )
            }
        }
        Ty::Constructed(ty) => (Ty::Constructed(ty.clone()), Vec::new()),
    }
}

pub fn representative_ty<'a>(
    db: &Db,
    tys: &'a [ConstructedTy],
    representative: Node,
) -> Option<&'a ConstructedTy> {
    tys.iter()
        .find(|ty| {
            ty.representative.is_some_and(|node| {
                node == representative
                    || db
                        .get::<Instantiated>(node)
                        .is_some_and(|instantiated| instantiated.definition == representative)
            })
        })
        .or_else(|| tys.first())
}
