use crate::{
    database::{Db, Fact, NodeRef, Render},
    typecheck::{Constraint, ConstructedType, Type},
};
use std::sync::Arc;

#[derive(Debug, Clone, Default)]
pub struct Typed {
    pub group: Option<Arc<Group>>,
}

impl Fact for Typed {}

impl Render for Typed {
    fn write(&self, w: &mut dyn std::fmt::Write, db: &Db) -> std::fmt::Result {
        let Some(group) = &self.group else {
            return write!(w, "types not solved");
        };

        if group.types.is_empty() {
            write!(w, "missing type")?;

            if group.nodes.len() > 1 {
                write!(w, " (group: ")?;

                let mut first = true;
                for node in &group.nodes {
                    if !first {
                        write!(w, ", ")?;
                    }

                    write!(w, "{}", db.render(node))?;
                    first = false;
                }

                write!(w, ")")?;
            }
        } else {
            write!(w, "has type ")?;

            for (index, ty) in group.types.iter().enumerate() {
                if index > 0 {
                    write!(w, " or ")?;
                }

                Type::from(ty.clone()).render(true).write(w, db)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Group {
    pub nodes: Vec<NodeRef>,
    pub types: Vec<ConstructedType>,
    pub trace: Vec<Box<dyn Constraint>>,
}

impl Group {
    pub fn new(nodes: impl IntoIterator<Item = NodeRef>) -> Self {
        Group {
            nodes: Vec::from_iter(nodes),
            types: Vec::new(),
            trace: Vec::new(),
        }
    }

    pub fn unify_with_types(
        &mut self,
        groups: &mut Groups,
        representative: Option<&NodeRef>,
        types: impl IntoIterator<Item = ConstructedType>,
        mut unify: impl FnMut(&mut Groups, &ConstructedType, &ConstructedType) -> bool,
    ) {
        for mut ty in types {
            if self
                .types
                .first()
                .cloned()
                .is_none_or(|first| !unify(groups, &first, &ty))
            {
                // If the type cannot be unified, add it to the group separately

                if let Some(node) = representative {
                    ty.representative.get_or_insert_with(|| node.clone());
                }

                self.types.push(ty);
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Groups(im::Vector<Option<Group>>);

impl Groups {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn index_of(&self, node: &NodeRef) -> Option<usize> {
        self.0
            .iter()
            .enumerate()
            .flat_map(|(index, group)| {
                group.as_ref().and_then(|group| {
                    if group.nodes.contains(node) {
                        Some(index)
                    } else {
                        None
                    }
                })
            })
            .next()
    }

    pub fn get(&self, index: usize) -> &Group {
        self.0[index].as_ref().expect("group being replaced")
    }

    pub fn get_mut(&mut self, index: usize) -> &mut Group {
        self.0[index].as_mut().expect("group being replaced")
    }

    pub fn remove_existing(&mut self, index: usize) -> Group {
        self.0[index].take().expect("group being replaced")
    }

    pub fn insert(
        &mut self,
        group: Group,
        mut unify: impl FnMut(&mut Self, &ConstructedType, &ConstructedType) -> bool,
    ) {
        let mut add = false;
        let mut groups_to_unify = Vec::new();

        for node in &group.nodes {
            if let Some(index) = self.index_of(node) {
                groups_to_unify.push((node.clone(), index));
            } else {
                add = true;
            }
        }

        // Delay unifying until after adding the group below
        let unify = (!groups_to_unify.is_empty()).then(|| {
            let types = group.types.clone();
            move |groups: &mut Self| {
                for (node, index) in groups_to_unify {
                    let mut group = groups.remove_existing(index);
                    group.unify_with_types(groups, Some(&node), types.clone(), &mut unify);
                    groups.insert_existing(index, group);
                }
            }
        });

        if add {
            self.add(group);
        }

        if let Some(unify) = unify {
            unify(self);
        }
    }

    pub fn add(&mut self, group: Group) -> usize {
        if let Some(index) = self.0.iter().position(|group| group.is_none()) {
            self.0[index] = Some(group);
            index
        } else {
            let index = self.0.len();
            self.0.push_back(Some(group));
            index
        }
    }

    pub fn insert_existing(&mut self, index: usize, group: Group) {
        self.0[index].replace(group);
    }

    pub fn merge(
        &mut self,
        trace: Option<Box<dyn Constraint>>,
        new_index: usize,
        old_group: Group,
        unify: impl FnMut(&mut Self, &ConstructedType, &ConstructedType) -> bool,
    ) {
        let mut new_group = self.0[new_index].take().expect("group being replaced");

        new_group.nodes.extend(old_group.nodes.iter().cloned());
        new_group.unify_with_types(self, None, old_group.types, unify);
        new_group.trace.extend(old_group.trace);

        if let Some(trace) = trace {
            new_group.trace.push(trace);
        }

        self.add(new_group);
    }

    pub fn indices(&self) -> impl Iterator<Item = usize> {
        (0..self.0.len()).filter(|&index| self.0[index].is_some())
    }

    pub fn into_sorted<K: Ord>(
        self,
        mut key: impl FnMut(&NodeRef) -> K,
    ) -> impl Iterator<Item = Group> {
        self.0.into_iter().flatten().map(move |mut group| {
            group.nodes.sort_by_key(&mut key);
            group
        })
    }
}
