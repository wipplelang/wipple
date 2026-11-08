use crate::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::ty::{ConstructedTy, Ty},
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Group(Vec<(Node, NodeRank, Vec<ConstructedTy>)>);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum NodeRank {
    #[default]
    Inherited,
    Annotated,
    Type,
    Statement,
}

impl Group {
    pub fn with_nodes(nodes: impl IntoIterator<Item = Node>) -> Self {
        Group(
            nodes
                .into_iter()
                .map(|node| (node, NodeRank::default(), Vec::new()))
                .collect(),
        )
    }

    pub fn nodes(&self) -> impl Iterator<Item = Node> + '_ {
        self.0.iter().map(|(node, _, _)| *node)
    }

    pub fn tys(&self) -> impl Iterator<Item = &ConstructedTy> + '_ {
        self.0.iter().flat_map(|(_, _, tys)| tys)
    }

    pub fn entries(&self) -> impl Iterator<Item = (Node, NodeRank, &ConstructedTy)> {
        self.0
            .iter()
            .flat_map(|&(node, rank, ref tys)| tys.iter().map(move |ty| (node, rank, ty)))
    }

    pub fn entries_mut(&mut self) -> impl Iterator<Item = (Node, NodeRank, &mut ConstructedTy)> {
        self.0
            .iter_mut()
            .flat_map(|&mut (node, rank, ref mut tys)| {
                tys.iter_mut().map(move |ty| (node, rank, ty))
            })
    }

    pub fn contains_node(&self, node: Node) -> bool {
        self.0.iter().any(|(other, _, _)| *other == node)
    }

    pub fn merge(
        &mut self,
        db: &mut Db,
        other: &Self,
        mut unify: impl FnMut(&mut Db, &ConstructedTy, &ConstructedTy) -> bool,
    ) {
        // Merge in all the nodes even if unification fails
        for &(node, rank, _) in &other.0 {
            self.set_rank(node, rank);
        }

        let mut other_entries = other.entries();
        let Some((other_node, other_rank, other_ty)) = other_entries.next() else {
            return;
        };

        // Add the first type to the group...
        let mut queue = Some((other_node, other_rank, other_ty));
        if let Some((_, _, existing_ty)) = self.entries().next()
            && unify(db, existing_ty, other_ty)
        {
            // ...unless it unifies
            queue = None;
        }

        // And add the remaining types as-is
        for (other_node, other_rank, other_ty) in queue.into_iter().chain(other_entries) {
            if let Some((_, _, tys)) = self.0.iter_mut().find(|(node, _, _)| *node == other_node) {
                tys.push(other_ty.clone());
            } else {
                self.0
                    .push((other_node, other_rank, vec![other_ty.clone()]));
            }
        }
    }

    pub fn get_tys(&self, node: Node) -> &[ConstructedTy] {
        self.0
            .iter()
            .find(|(other, _, _)| *other == node)
            .map(|(_, _, tys)| tys.as_slice())
            .unwrap_or_default()
    }

    pub fn insert_ty(&mut self, node: Node, ty: ConstructedTy) {
        if let Some((_, _, tys)) = self.0.iter_mut().find(|(other, _, _)| *other == node) {
            tys.push(ty);
        } else {
            self.0.push((node, NodeRank::default(), vec![ty]));
        }
    }

    pub fn get_rank(&self, node: Node) -> NodeRank {
        self.0
            .iter()
            .find(|(other, _, _)| *other == node)
            .map(|(_, rank, _)| *rank)
            .unwrap_or_default()
    }

    pub fn set_rank(&mut self, node: Node, rank: NodeRank) {
        if let Some((_, entry, _)) = self.0.iter_mut().find(|(other, _, _)| *other == node) {
            *entry = rank;
        } else {
            self.0.push((node, rank, Vec::new()));
        }
    }

    pub fn min_rank(&self) -> NodeRank {
        self.0
            .iter()
            .map(|(_, rank, _)| *rank)
            .min()
            .unwrap_or_default()
    }
}

#[derive(Debug, Clone, Default)]
pub struct Groups(BTreeMap<usize, Option<Group>>);

impl Groups {
    pub fn index_of(&self, node: Node) -> Option<usize> {
        self.0
            .iter()
            .find(|(_, slot)| slot.as_ref().is_some_and(|slot| slot.contains_node(node)))
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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum Prefer {
    #[default]
    DirectType,
    RepresentativeType,
}

pub fn representative_types_of<'a>(
    db: &'a Db,
    node: Node,
    relevant: &[Node],
    prefer: Prefer,
) -> Vec<(NodeRank, &'a ConstructedTy)> {
    let Some(Typed(Some(group))) = db.get(node) else {
        return Default::default();
    };

    // Prefer relevant nodes that belong to the same group
    let mut candidates = relevant
        .iter()
        .copied()
        .filter(|&node| group.contains_node(node))
        .collect::<Vec<_>>();

    match prefer {
        Prefer::DirectType => candidates.insert(0, node),
        Prefer::RepresentativeType => candidates.push(node),
    }

    for node in candidates {
        let tys = group.get_tys(node);
        let rank = group.get_rank(node);
        if !tys.is_empty() {
            return tys.iter().map(|ty| (rank, ty)).collect();
        }
    }

    group.entries().map(|(_, rank, ty)| (rank, ty)).collect()
}

pub fn update_type(db: &Db, ty: &Ty, relevant: &[Node], prefer: Prefer) -> Ty {
    match ty {
        Ty::Node(node) => {
            let tys = representative_types_of(db, *node, relevant, prefer);

            if let Some(&(_, ty)) = tys.first() {
                Ty::Constructed(ty.clone())
            } else {
                Ty::Node(*node)
            }
        }
        Ty::Constructed(ty) => Ty::Constructed(ty.clone()),
    }
}
