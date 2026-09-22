use crate::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::{
        bounds::Instance,
        constraints::{Constraint, ConstraintConsequence, ConstraintKind, Constraints},
        groups::{Group, Groups, NodeRank},
        ty::{ConstructedTy, Ty},
    },
};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, mem};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DirectlyGroupedWith(pub Vec<Node>);

#[typetag::serde]
impl Fact for DirectlyGroupedWith {}

impl Render for DirectlyGroupedWith {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx<'_>) {
        ctx.string("grouped with ");

        for (index, node) in self.0.iter().enumerate() {
            if index > 0 {
                ctx.string(", ");
            }

            ctx.node(*node);
        }
    }
}

static ITERATION_LIMIT: usize = 32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SubstitutionsKey(pub usize);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Substitutions {
    pub nodes: BTreeMap<Node, Node>,
    pub parameters: BTreeMap<Node, Ty>,
}

#[derive(Debug, Default)]
pub struct Solver {
    pub trace: bool,
    pub constraints: Constraints,
    pub substitutions: Vec<Substitutions>,
    pub(crate) groups: Groups,
    pub(crate) implied_instances: Vec<Instance>,
    pub(crate) tracing_node: Option<Node>,
    iterations: usize,
}

impl Solver {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn copy(&self) -> Self {
        Solver {
            groups: self.groups.clone(),
            implied_instances: self.implied_instances.clone(),
            substitutions: self.substitutions.clone(),
            iterations: self.iterations,
            ..Default::default()
        }
    }

    pub fn inherit(&mut self, other: Self) -> Constraints {
        self.groups = other.groups;
        self.substitutions = other.substitutions;
        self.iterations = other.iterations;
        other.constraints
    }

    pub fn extend(
        &mut self,
        constraints: impl IntoIterator<Item = (Node, Box<dyn Constraint>)>,
        ranks: impl IntoIterator<Item = (Node, NodeRank)>,
        substitutions: impl IntoIterator<Item = Substitutions>,
    ) {
        self.constraints.extend_back(constraints);

        for (node, rank) in ranks {
            self.rank(node, rank);
        }

        self.substitutions.extend(substitutions);
    }

    pub fn into_groups(mut self, db: &Db) -> Vec<Group> {
        self.apply_all(db);
        self.groups.into_vec()
    }

    pub fn run(&mut self, db: &mut Db) {
        while !self.constraints.is_empty() {
            if self.iterations >= ITERATION_LIMIT {
                return;
            }

            self.run_pass(db, ConstraintKind::Ty);
            self.run_pass(db, ConstraintKind::Bound);

            self.iterations += 1;
        }
    }

    pub fn run_pass(&mut self, db: &mut Db, kind: ConstraintKind) {
        let mut constraints = mem::take(&mut self.constraints);
        constraints.run(db, self, kind);
        self.constraints = constraints;
    }

    pub fn imply(&mut self, instance: Instance) {
        if self
            .implied_instances
            .iter()
            .any(|implied| implied.node == instance.node)
        {
            return;
        }

        self.implied_instances.push(instance);
    }

    pub fn add_consequence(&mut self, db: &mut Db, consequence: ConstraintConsequence) {
        if let Some(node) = self.tracing_node {
            for relevant in [node].into_iter().chain(consequence.relevant_nodes()) {
                let traces = db
                    .traces
                    .entry(relevant)
                    .or_default()
                    .entry(node)
                    .or_default();

                if !traces.contains(&consequence) {
                    traces.push(consequence.clone());
                }
            }
        }
    }

    pub fn insert_substitutions(
        &mut self,
        nodes: BTreeMap<Node, Node>,
        parameters: BTreeMap<Node, Ty>,
    ) -> SubstitutionsKey {
        let key = SubstitutionsKey(self.substitutions.len());

        self.substitutions.push(Substitutions { nodes, parameters });

        key
    }

    pub fn get_substitutions(
        &self,
        key: SubstitutionsKey,
    ) -> (BTreeMap<Node, Node>, BTreeMap<Node, Ty>) {
        let substitutions = &self.substitutions[key.0];

        (
            substitutions.nodes.clone(),
            substitutions.parameters.clone(),
        )
    }

    pub fn with_substitutions_mut<T>(
        &mut self,
        key: SubstitutionsKey,
        f: impl FnOnce(&mut Self, &mut Substitutions) -> T,
    ) -> T {
        let mut substitutions = self.substitutions[key.0].clone();
        let result = f(self, &mut substitutions);
        self.substitutions[key.0] = substitutions;
        result
    }

    pub fn unify(&mut self, db: &mut Db, node: Node, ty: &Ty, mut on_error: impl FnMut()) {
        self.unify_inner(db, &Ty::Node(node), ty, &mut on_error);
    }

    fn unify_inner(&mut self, db: &mut Db, left: &Ty, right: &Ty, on_error: &mut dyn FnMut()) {
        if left == right {
            return;
        }

        let original_left_node = left.node();
        let original_right_node = right.node();

        if let Some(original_left_node) = original_left_node
            && let Some(original_right_node) = original_right_node
        {
            self.merge(db, original_left_node, original_right_node, on_error);
        } else {
            let left = self.apply_ty(db, left);
            let right = self.apply_ty(db, right);

            match (left, right) {
                (Ty::Node(left), Ty::Node(right)) => {
                    self.merge(db, left, right, on_error);
                }
                (Ty::Node(node), Ty::Constructed(ty)) | (Ty::Constructed(ty), Ty::Node(node)) => {
                    self.insert(db, node, ty, true);
                }
                (Ty::Constructed(left), Ty::Constructed(right)) => {
                    if !self.unify_inner_constructed(db, &left, &right, &mut |_| on_error()) {
                        // Report conflicts on the original nodes

                        if let Some(original_left_node) = original_left_node {
                            self.insert(db, original_left_node, right, false);
                        }

                        if let Some(original_right_node) = original_right_node {
                            self.insert(db, original_right_node, left, false);
                        }
                    }
                }
            }
        }
    }

    fn unify_inner_constructed(
        &mut self,
        db: &mut Db,
        left: &ConstructedTy,
        right: &ConstructedTy,
        on_error: &mut dyn FnMut(bool),
    ) -> bool {
        let left_child_count = left.children.len();
        let right_child_count = right.children.len();

        if left.tag == right.tag {
            for (&left_child, &right_child) in std::iter::zip(&left.children, &right.children) {
                self.unify_inner(
                    db,
                    &Ty::Node(left_child),
                    &Ty::Node(right_child),
                    &mut || on_error(true),
                );
            }
        }

        if left.tag != right.tag || left_child_count != right_child_count {
            on_error(false);
            return false;
        }

        true
    }

    pub fn unify_parameters(
        &mut self,
        db: &mut Db,
        left: &BTreeMap<Node, Ty>,
        right: &BTreeMap<Node, Ty>,
        mut on_error: impl FnMut(),
    ) {
        for (parameter, left) in left.iter() {
            if let Some(right) = right.get(parameter) {
                self.unify_inner(db, left, right, &mut on_error);
            }
        }
    }

    fn merge(
        &mut self,
        db: &mut Db,
        left_node: Node,
        right_node: Node,
        on_error: &mut dyn FnMut(),
    ) {
        db.get_mut_or_default::<DirectlyGroupedWith>(left_node)
            .0
            .push(right_node);

        db.get_mut_or_default::<DirectlyGroupedWith>(right_node)
            .0
            .push(left_node);

        let left_index = self.groups.index_of(left_node);
        let right_index = self.groups.index_of(right_node);

        if (left_index.is_some() || right_index.is_some()) && left_index == right_index {
            return; // already the same group
        }

        let (index, group) = match (left_index, right_index) {
            (Some(left_index), Some(right_index)) => {
                (Some(left_index), self.groups.remove_existing(right_index))
            }
            (Some(left_index), None) => (Some(left_index), Group::with_nodes([right_node])),
            (None, Some(right_index)) => (Some(right_index), Group::with_nodes([left_node])),
            (None, None) => (None, Group::with_nodes([left_node, right_node])),
        };

        let mut merged = true;
        if let Some(index) = index {
            let mut new_group = self.groups.remove_existing(index);

            Groups::merge(db, group, &mut new_group, |db, left, right| {
                self.unify_inner_constructed(db, left, right, &mut |nested| {
                    on_error();

                    if !nested {
                        merged = false;
                    }
                })
            });

            self.groups.insert(new_group);
        } else {
            self.groups.insert(group);
        }

        self.add_consequence(
            db,
            ConstraintConsequence::Group(left_node, right_node, merged),
        );
    }

    fn insert(&mut self, db: &mut Db, node: Node, ty: ConstructedTy, merged: bool) {
        self.add_consequence(db, ConstraintConsequence::Ty(node, ty.clone(), merged));

        self.with_group_mut(node, |group| {
            group.insert_ty(node, ty);
        });
    }

    pub fn rank_of(&self, node: Node) -> NodeRank {
        self.groups
            .index_of(node)
            .map(|index| self.groups.get(index).get_rank(node))
            .unwrap_or_default()
    }

    pub fn rank(&mut self, node: Node, rank: NodeRank) {
        self.with_group_mut(node, |group| {
            group.set_rank(node, rank);
        })
    }

    fn with_group_mut<T>(&mut self, node: Node, f: impl FnOnce(&mut Group) -> T) -> T {
        if let Some(index) = self.groups.index_of(node) {
            f(self.groups.get_mut(index))
        } else {
            let mut group = Group::with_nodes([node]);
            let result = f(&mut group);
            self.groups.insert(group);
            result
        }
    }

    pub fn apply_parameters(&self, db: &Db, parameters: &mut BTreeMap<Node, Ty>) {
        for ty in parameters.values_mut() {
            *ty = self.apply_ty(db, ty);
        }
    }

    pub fn apply_ty(&self, _db: &Db, ty: &Ty) -> Ty {
        let Ty::Node(node) = ty else {
            return ty.clone();
        };

        let Some(index) = self.groups.index_of(*node) else {
            return ty.clone();
        };

        let group = self.groups.get(index);

        let Some(applied) = group.get_tys(*node).first().or_else(|| group.tys().next()) else {
            return ty.clone();
        };

        Ty::Constructed(applied.clone())
    }

    fn apply_all(&mut self, db: &Db) {
        let indices = self.groups.indices().collect::<Vec<_>>();

        for index in indices {
            let mut group = self.groups.remove_existing(index);

            for (_, _, ty) in &mut group.entries_mut() {
                *ty = match self.apply_ty(db, &Ty::Constructed(ty.clone())) {
                    Ty::Constructed(ty) => ty,
                    Ty::Node(_) => {
                        unreachable!("constructed types remain constructed after `apply_ty`")
                    }
                };
            }

            self.groups.insert_existing(index, group);
        }
    }
}
