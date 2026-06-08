use crate::{
    db::{Db, Fact, Node},
    render::Render,
    typecheck::{
        bounds::Instance,
        constraints::Constraints,
        groups::{Group, Groups},
        ty::{ConstructedTy, Ty},
    },
};
use serde::{Deserialize, Serialize};
use std::{
    any::TypeId,
    collections::{BTreeMap, BTreeSet},
    mem,
};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GroupedWith(pub BTreeSet<Node>);

#[typetag::serde]
impl Fact for GroupedWith {}

impl Render for GroupedWith {}

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
    pub error: bool,
    pub constraints: Constraints,
    pub substitutions: Vec<Substitutions>,
    pub(crate) groups: Groups,
    pub(crate) implied_instances: Vec<Instance>,
    pub(crate) progress: bool,
    iterations: usize,
}

impl Solver {
    pub fn copy(&self) -> Self {
        Solver {
            groups: self.groups.clone(),
            implied_instances: self.implied_instances.clone(),
            substitutions: self.substitutions.clone(),
            ..Default::default()
        }
    }

    pub fn inherit(&mut self, other: Self) -> Constraints {
        self.groups = other.groups;
        self.substitutions = other.substitutions;
        other.constraints
    }

    pub fn into_sorted_groups<K: Ord>(mut self, key: impl FnMut(Node) -> K) -> Vec<Group> {
        self.apply_all();
        self.groups.into_sorted_groups(key)
    }

    pub fn run(&mut self, db: &mut Db) {
        loop {
            self.progress = false;

            self.run_pass_until(db, None);

            if !self.progress {
                break;
            }
        }

        // Run a final pass
        self.run_pass_until(db, None);
    }

    pub fn run_pass_until(&mut self, db: &mut Db, stop: Option<TypeId>) {
        if self.iterations >= ITERATION_LIMIT {
            return;
        }

        let mut constraints = mem::take(&mut self.constraints);
        constraints.run_until(db, self, stop);
        if !self.progress {
            constraints.run_defaults(db, self);
        }

        self.constraints = constraints;
        self.iterations += 1;
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

    pub fn unify_with_node(&mut self, db: &mut Db, left: Node, right: Node) {
        self.unify_inner(db, &Ty::Node(left), &Ty::Node(right));
    }

    pub fn unify_with_ty(&mut self, db: &mut Db, node: Node, ty: ConstructedTy) {
        self.unify_inner(db, &Ty::Node(node), &Ty::Constructed(ty));
    }

    fn unify_inner(&mut self, db: &mut Db, left: &Ty, right: &Ty) {
        if left == right {
            return;
        }

        let original_left_node = left.node();
        let original_right_node = right.node();

        if let Some(original_left_node) = original_left_node
            && let Some(original_right_node) = original_right_node
        {
            self.progress = true;
            self.merge(db, original_left_node, original_right_node);
            return;
        }

        let left = self.apply_ty_shallow(left);
        let right = self.apply_ty_shallow(right);

        match (left, right) {
            (Ty::Node(left), Ty::Node(right)) => {
                self.progress = true;
                self.merge(db, left, right);
            }
            (Ty::Node(node), Ty::Constructed(ty)) | (Ty::Constructed(ty), Ty::Node(node)) => {
                self.progress = true;
                self.insert(node, ty);
            }
            (Ty::Constructed(left), Ty::Constructed(right)) => {
                let original_nodes = BTreeSet::from_iter(
                    [original_left_node, original_right_node]
                        .into_iter()
                        .flatten(),
                );

                if !self.unify_inner_constructed(db, &left, &right, original_nodes) {
                    // Report conflicts on the original nodes

                    if let Some(original_left_node) = original_left_node {
                        self.insert(original_left_node, right);
                    }

                    if let Some(original_right_node) = original_right_node {
                        self.insert(original_right_node, left);
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
        original_nodes: BTreeSet<Node>,
    ) -> bool {
        let left_child_count = left.children.len();
        let right_child_count = right.children.len();

        if left.tag == right.tag {
            for (left_child, right_child) in std::iter::zip(&left.children, &right.children) {
                if !left_child.referenced_nodes().is_disjoint(&original_nodes)
                    || !right_child.referenced_nodes().is_disjoint(&original_nodes)
                {
                    // recursive types
                    continue;
                }

                self.unify_inner(db, left_child, right_child);
            }
        }

        if left.tag != right.tag || left_child_count != right_child_count {
            self.error = true;
            return false;
        }

        true
    }

    pub fn unify_parameters(
        &mut self,
        db: &mut Db,
        left: &BTreeMap<Node, Ty>,
        right: &BTreeMap<Node, Ty>,
    ) {
        for (parameter, left) in left.iter() {
            if let Some(right) = right.get(parameter) {
                self.unify_inner(db, left, right);
            }
        }
    }

    pub fn merge(&mut self, db: &mut Db, left_node: Node, right_node: Node) {
        db.get_mut_or_default::<GroupedWith>(left_node)
            .0
            .insert(right_node);

        db.get_mut_or_default::<GroupedWith>(right_node)
            .0
            .insert(left_node);

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

        if let Some(index) = index {
            let mut new_group = self.groups.remove_existing(index);

            Groups::merge(group, &mut new_group, |left, right| {
                self.unify_inner_constructed(
                    db,
                    left,
                    right,
                    BTreeSet::from([left_node, right_node]),
                )
            });

            self.groups.insert(new_group);
        } else {
            self.groups.insert(group);
        }
    }

    pub fn insert(&mut self, node: Node, mut ty: ConstructedTy) {
        if ty.representative.is_none() {
            ty.representative = Some(node);
        }

        if let Some(index) = self.groups.index_of(node) {
            self.groups.get_mut(index).tys.push(ty);
        } else {
            let mut group = Group::with_nodes([node]);
            group.tys.push(ty);
            self.groups.insert(group);
        }
    }

    pub fn apply_ty(&self, ty: Ty) -> Ty {
        ty.traverse(&mut |ty| self.apply_ty_shallow(ty))
    }

    pub fn apply_parameters(&self, parameters: &mut BTreeMap<Node, Ty>) {
        for ty in parameters.values_mut() {
            *ty = self.apply_ty(ty.clone());
        }
    }

    fn apply_ty_shallow(&self, ty: &Ty) -> Ty {
        let Ty::Node(node) = ty else {
            return ty.clone();
        };

        let Some(index) = self.groups.index_of(*node) else {
            return ty.clone();
        };

        let group = self.groups.get(index);

        let Some(applied) = group.tys.first() else {
            return ty.clone();
        };

        Ty::Constructed(applied.clone())
    }

    fn apply_all(&mut self) {
        let indices = self.groups.indices().collect::<Vec<_>>();

        for index in indices {
            let mut group = self.groups.remove_existing(index);

            for ty in &mut group.tys {
                *ty = match self.apply_ty(Ty::Constructed(ty.clone())) {
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
