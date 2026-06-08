use crate::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::{
        bounds::Instance,
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
        tys: impl IntoIterator<Item = ConstructedTy>,
        representative: Option<Node>,
        mut unify: impl FnMut(&ConstructedTy, &ConstructedTy) -> bool,
    ) {
        for mut ty in tys {
            if self.tys.is_empty() || !unify(&self.tys[0], &ty) {
                // If the type cannot be unified, add it to the group separately

                if self.tys.contains(&ty) {
                    continue;
                }

                if let Some(representative) = representative
                    && ty.representative.is_none()
                {
                    ty.representative = Some(representative)
                }

                self.tys.push(ty);
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
        old_group: Group,
        new_group: &mut Group,
        unify: impl FnMut(&ConstructedTy, &ConstructedTy) -> bool,
    ) {
        new_group.nodes.extend(old_group.nodes);
        new_group.unify(old_group.tys, None, unify);
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
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
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
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("type is annotated");
    }
}

pub fn types_of(db: &Db, node: Node) -> &[ConstructedTy] {
    db.get(node)
        .and_then(|Typed(group)| group.as_ref())
        .map(|group| group.tys.as_slice())
        .unwrap_or_default()
}

pub fn update_type(db: &Db, ty: &Ty, ignore_conflicts: bool) -> Ty {
    ty.traverse(&mut |ty| {
        if let Ty::Node(node) = ty {
            let tys = types_of(db, *node);

            let latest = if let [ty] = tys {
                ty
            } else if ignore_conflicts
                && let Some(ty) = tys
                    .iter()
                    .find(|ty| ty.representative == Some(*node))
                    .or(tys.first())
            {
                ty
            } else {
                return ty.clone();
            };

            Ty::Constructed(latest.clone())
        } else {
            ty.clone()
        }
    })
}

pub fn update_instance(db: &Db, instance: &mut Instance) {
    for ty in instance.parameters.values_mut() {
        *ty = update_type(db, ty, false);
    }
}
