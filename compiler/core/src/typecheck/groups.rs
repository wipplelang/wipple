use crate::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::ty::{ConstructedTy, Ty},
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Group(BTreeMap<Node, (NodeRank, Vec<ConstructedTy>)>);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum NodeRank {
    #[default]
    Inherited,
    Annotated,
}

impl Group {
    pub fn with_nodes(nodes: impl IntoIterator<Item = Node>) -> Self {
        Group(
            nodes
                .into_iter()
                .map(|node| (node, Default::default()))
                .collect(),
        )
    }

    pub fn nodes(&self) -> impl Iterator<Item = Node> + '_ {
        self.0.keys().copied()
    }

    pub fn tys(&self) -> impl Iterator<Item = &ConstructedTy> + '_ {
        let mut seen = Vec::new();
        self.0
            .values()
            .flat_map(|(_, tys)| tys.iter())
            .filter(move |&ty| {
                if seen.contains(&ty) {
                    false
                } else {
                    seen.push(ty);
                    true
                }
            })
    }

    pub fn entries(&self) -> impl Iterator<Item = (Node, NodeRank, &ConstructedTy)> {
        self.0
            .iter()
            .flat_map(|(&node, &(rank, ref tys))| tys.iter().map(move |ty| (node, rank, ty)))
    }

    pub fn entries_mut(&mut self) -> impl Iterator<Item = (Node, NodeRank, &mut ConstructedTy)> {
        self.0
            .iter_mut()
            .flat_map(|(&node, &mut (rank, ref mut tys))| {
                tys.iter_mut().map(move |ty| (node, rank, ty))
            })
    }

    pub fn unify(
        &mut self,
        db: &mut Db,
        other: &Self,
        error: Option<&mut bool>,
        mut unify: impl FnMut(&mut Db, &ConstructedTy, &ConstructedTy, Option<&mut bool>) -> bool,
    ) {
        // Merge in all the nodes even if unification fails
        for (&node, &(rank, _)) in &other.0 {
            self.set_rank(node, rank);
        }

        let mut other_entries = other.entries();
        let Some((other_node, _, other_ty)) = other_entries.next() else {
            return;
        };

        // Add the first type to the group...
        let mut queue = Some((other_node, other_ty));
        if let Some((_, _, existing_ty)) = self.entries().next()
            && unify(db, existing_ty, other_ty, error)
        {
            // ...unless it unifies
            queue = None;
        }

        // And add the remaining types as-is
        for (other_node, other_ty) in queue
            .into_iter()
            .chain(other_entries.map(|(node, _, ty)| (node, ty)))
        {
            self.0
                .entry(other_node)
                .or_default()
                .1
                .push(other_ty.clone());
        }
    }

    pub fn get_tys(&self, node: Node) -> &[ConstructedTy] {
        self.0
            .get(&node)
            .map(|(_, tys)| tys.as_slice())
            .unwrap_or_default()
    }

    pub fn insert_ty(&mut self, node: Node, ty: ConstructedTy) {
        self.0.entry(node).or_default().1.push(ty);
    }

    pub fn get_rank(&self, node: Node) -> NodeRank {
        self.0.get(&node).map(|(rank, _)| *rank).unwrap_or_default()
    }

    pub fn set_rank(&mut self, node: Node, rank: NodeRank) {
        self.0.entry(node).or_default().0 = rank;
    }

    pub fn min_ranked_nodes(&self) -> Option<(BTreeSet<Node>, NodeRank)> {
        let mut nodes = self
            .0
            .iter()
            .map(|(&node, (rank, _))| (node, *rank))
            .collect::<BTreeMap<_, _>>();

        let min_rank = nodes.values().copied().min()?;

        nodes.retain(|_, rank| *rank == min_rank);
        Some((nodes.into_keys().collect(), min_rank))
    }
}

#[derive(Debug, Clone, Default)]
pub struct Groups(BTreeMap<usize, Option<Group>>);

impl Groups {
    pub fn index_of(&self, node: Node) -> Option<usize> {
        self.0
            .iter()
            .find(|(_, slot)| slot.as_ref().is_some_and(|slot| slot.0.contains_key(&node)))
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
        error: Option<&mut bool>,
        unify: impl FnMut(&mut Db, &ConstructedTy, &ConstructedTy, Option<&mut bool>) -> bool,
    ) {
        new_group.unify(db, &old_group, error, unify);
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

    pub fn into_vec(self) -> Vec<Group> {
        self.0.into_values().flatten().collect()
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

        if group.tys().next().is_none() {
            ctx.string("missing type");

            if group.nodes().count() > 1 {
                ctx.string(" (group: ");

                for (index, node) in group.nodes().enumerate() {
                    if index > 0 {
                        ctx.string(", ");
                    }

                    ctx.node(node);
                }

                ctx.string(")");
            }
        } else {
            ctx.string("has type ");

            for (index, ty) in group.tys().enumerate() {
                if index > 0 {
                    ctx.string(" or ");
                }

                ctx.render(db, &Ty::Constructed(ty.clone()));
            }
        }
    }
}

pub fn representative_types_of<'a>(
    db: &'a Db,
    node: Node,
    relevant: &[Node],
) -> Vec<&'a ConstructedTy> {
    let Some(Typed(Some(group))) = db.get(node) else {
        return Vec::new();
    };

    // Prefer relevant nodes that belong to the same group
    let candidates = [node]
        .into_iter()
        .chain(
            relevant
                .iter()
                .copied()
                .filter(|node| group.0.contains_key(node)),
        )
        .collect::<Vec<_>>();

    for node in candidates {
        let tys = group.get_tys(node);
        if !tys.is_empty() {
            return tys.iter().collect();
        }
    }

    group.tys().collect()
}

pub fn update_type(db: &Db, ty: &Ty) -> Ty {
    match ty {
        Ty::Node(node) => {
            let tys = representative_types_of(db, *node, &[]);

            if let Some(&ty) = tys.first() {
                Ty::Constructed(ty.clone())
            } else {
                Ty::Node(*node)
            }
        }
        Ty::Constructed(ty) => Ty::Constructed(ty.clone()),
    }
}
