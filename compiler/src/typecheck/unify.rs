use crate::{
    database::NodeRef,
    typecheck::{Constraint, ConstructedType, Group, Groups, Substitutions, Type},
};

pub struct UnifyCtx<'a> {
    pub groups: &'a mut Groups,
    pub progress: &'a mut bool,
    pub error: &'a mut bool,
}

impl UnifyCtx<'_> {
    pub fn unify(
        &mut self,
        trace: Option<Box<dyn Constraint>>,
        left: impl Into<Type>,
        right: impl Into<Type>,
    ) {
        let left = left.into();
        let right = right.into();

        if left == right {
            return;
        }

        let original_left_node = left.as_node();
        let original_right_node = right.as_node();

        if let Some(original_left_node) = original_left_node
            && let Some(original_right_node) = original_right_node
        {
            *self.progress = true;
            self.merge(trace.clone(), original_left_node, original_right_node);
            return;
        }

        let left = self.apply_shallow(left.clone());
        let right = self.apply_shallow(right.clone());

        match (left, right) {
            (Type::Node(left), Type::Node(right)) => {
                self.merge(trace, &left, &right);
                *self.progress = true;
            }
            (Type::Node(node), Type::Constructed(ty))
            | (Type::Constructed(ty), Type::Node(node)) => {
                self.insert(trace, &node, ty);
                *self.progress = true;
            }
            (Type::Constructed(left), Type::Constructed(right)) => {
                let original_nodes = [original_left_node, original_right_node]
                    .into_iter()
                    .flatten()
                    .cloned()
                    .collect::<Vec<_>>();

                if !self.unify_constructed_types(&left, &right, &original_nodes) {
                    // Report conflicts on the original nodes

                    if let Some(original_left_node) = original_left_node {
                        self.insert(trace.clone(), original_left_node, right);
                    }

                    if let Some(original_right_node) = original_right_node {
                        self.insert(trace.clone(), original_right_node, left);
                    }
                }
            }
        }
    }

    pub fn unify_constructed_types(
        &mut self,
        left: &ConstructedType,
        right: &ConstructedType,
        original_nodes: &[NodeRef],
    ) -> bool {
        // Type parameters are unique
        if left.instantiate != right.instantiate {
            *self.error = true;
            return false;
        }

        if left.tag == right.tag {
            for (left_child, right_child) in left.children.iter().zip(&right.children) {
                if left_child.references_nodes(|node| original_nodes.contains(node))
                    || right_child.references_nodes(|node| original_nodes.contains(node))
                {
                    // Recursive types
                    continue;
                }

                self.unify(None, left_child.clone(), right_child.clone());
            }
        }

        if left.tag != right.tag || left.children.len() != right.children.len() {
            *self.error = true;
            return false;
        }

        true
    }

    pub fn unify_substitutions(
        &mut self,
        trace: Option<Box<dyn Constraint>>,
        left: &Substitutions,
        right: &Substitutions,
    ) {
        for (parameter, left) in left.entries() {
            if let Some(right) = right.get(&parameter) {
                self.unify(trace.clone(), left, right);
            }
        }
    }

    fn merge(
        &mut self,
        trace: Option<Box<dyn Constraint>>,
        left_node: &NodeRef,
        right_node: &NodeRef,
    ) {
        let left_index = self.groups.index_of(left_node);
        let right_index = self.groups.index_of(right_node);

        if (left_index.is_some() || right_index.is_some()) && left_index == right_index {
            return; // already the same group
        }

        let (index, group) = match (left_index, right_index) {
            (Some(left_key), Some(right_key)) => {
                (Some(left_key), self.groups.remove_existing(right_key))
            }
            (Some(left_key), None) => (Some(left_key), Group::new([right_node.clone()])),
            (None, Some(right_key)) => (Some(right_key), Group::new([left_node.clone()])),
            (None, None) => (None, Group::new([left_node.clone(), right_node.clone()])),
        };

        if let Some(index) = index {
            self.groups
                .merge(trace, index, group, |groups, left, right| {
                    let mut ctx = UnifyCtx {
                        groups,
                        progress: self.progress,
                        error: self.error,
                    };

                    ctx.unify_constructed_types(
                        left,
                        right,
                        &[left_node.clone(), right_node.clone()],
                    )
                });
        } else {
            self.groups.add(group);
        }
    }

    fn insert(
        &mut self,
        trace: Option<Box<dyn Constraint>>,
        node: &NodeRef,
        mut ty: ConstructedType,
    ) {
        ty.representative.get_or_insert_with(|| node.clone());

        let insert_into = |group: &mut Group| {
            group.types.push(ty);
            if let Some(trace) = &trace {
                group.trace.push(trace.clone());
            }
        };

        if let Some(index) = self.groups.index_of(node) {
            insert_into(self.groups.get_mut(index));
        } else {
            let mut group = Group::new([node.clone()]);
            insert_into(&mut group);
            self.groups.add(group);
        }
    }

    pub fn apply(&self, ty: &Type) -> Type {
        ty.traverse(|ty| self.apply_shallow(ty))
    }

    pub fn apply_substitutions(&self, substitutions: &Substitutions) {
        substitutions.modify(|ty| *ty = self.apply(ty));
    }

    fn apply_shallow(&self, ty: Type) -> Type {
        let Type::Node(node) = &ty else {
            return ty;
        };

        let Some(group) = self
            .groups
            .index_of(node)
            .map(|index| self.groups.get(index))
        else {
            return ty;
        };

        let Some(ty) = group.types.first() else {
            return ty;
        };

        Type::Constructed(ty.clone())
    }

    pub fn apply_all(&mut self) {
        for index in self.groups.indices().collect::<Vec<_>>() {
            let mut group = self.groups.remove_existing(index);

            for ty in &mut group.types {
                // This works because types cannot refer to their own group
                *ty = match self.apply(&Type::Constructed(ty.clone())) {
                    Type::Node(_) => unreachable!(),
                    Type::Constructed(ty) => ty,
                };
            }

            self.groups.insert_existing(index, group);
        }
    }
}
