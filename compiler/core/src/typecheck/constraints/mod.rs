pub mod bound_constraint;
pub mod default_constraint;
pub mod generic_constraint;
pub mod instantiate_constraint;
pub mod ty_constraint;

use crate::{
    db::{Db, Node},
    facts::Children,
    render::{ExplainOptions, ListBuilder, RenderCtx},
    typecheck::{
        bounds::Instance,
        groups::{NodeRank, Typed},
        instantiate::InstantiateCtx,
        solver::Solver,
        ty::ConstructedTy,
    },
    visit::{
        Resolved,
        definitions::{Defined, InstanceDefinition, VariableDefinition},
    },
};
use dyn_clone::DynClone;
use serde::{Deserialize, Serialize};
use std::{
    any::Any,
    collections::{BTreeMap, VecDeque},
    fmt::Debug,
    fmt::Write,
};

pub enum RunResult {
    None,
    Insert(Vec<(Node, Box<dyn Constraint>)>),
    Enqueue(Vec<(Node, Box<dyn Constraint>)>),
}

#[typetag::serde]
pub trait Constraint: Debug + DynClone + Any + Send + Sync {
    fn kind(&self) -> ConstraintKind;

    fn instantiate(
        &self,
        db: &mut Db,
        solver: &mut Solver,
        ctx: &mut InstantiateCtx,
    ) -> Option<Box<dyn Constraint>>;

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult;
}

dyn_clone::clone_trait_object!(Constraint);

impl dyn Constraint {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        (self as &dyn Any).downcast_ref()
    }

    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        (self as &mut dyn Any).downcast_mut()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConstraintKind {
    Ty,
    Bound,
}

#[derive(Debug, Default)]
pub struct Constraints(BTreeMap<ConstraintKind, VecDeque<(Node, Box<dyn Constraint>)>>);

impl Constraints {
    pub fn is_empty(&self) -> bool {
        self.0.values().all(|constraints| constraints.is_empty())
    }

    pub fn insert_front(&mut self, node: Node, constraint: Box<dyn Constraint>) {
        self.insert_inner(node, constraint, true);
    }

    pub fn insert_back(&mut self, node: Node, constraint: Box<dyn Constraint>) {
        self.insert_inner(node, constraint, false);
    }

    fn insert_inner(&mut self, node: Node, constraint: Box<dyn Constraint>, front: bool) {
        let constraints = self.0.entry(constraint.kind()).or_default();

        if front {
            constraints.push_front((node, constraint));
        } else {
            constraints.push_back((node, constraint));
        }
    }

    pub fn extend_front(
        &mut self,
        constraints: impl IntoIterator<
            Item = (Node, Box<dyn Constraint>),
            IntoIter: DoubleEndedIterator,
        >,
    ) {
        for (node, constraint) in constraints.into_iter().rev() {
            self.insert_front(node, constraint);
        }
    }

    pub fn extend_back(
        &mut self,
        constraints: impl IntoIterator<Item = (Node, Box<dyn Constraint>)>,
    ) {
        for (node, constraint) in constraints.into_iter() {
            self.insert_back(node, constraint);
        }
    }

    pub fn run(&mut self, db: &mut Db, solver: &mut Solver, kind: ConstraintKind) {
        let mut requeued_constraints = Vec::new();
        while let Some((node, constraint)) = self.0.get_mut(&kind).and_then(|c| c.pop_front()) {
            solver.tracing.push(node);

            match constraint.run(db, solver) {
                RunResult::None => {}
                RunResult::Insert(constraints) => self.extend_front(constraints),
                RunResult::Enqueue(constraints) => requeued_constraints.extend(constraints),
            }

            solver.tracing.pop().unwrap();
        }

        self.extend_back(requeued_constraints);
    }

    pub fn constraints_mut(&mut self) -> impl Iterator<Item = &mut dyn Constraint> {
        self.0
            .values_mut()
            .flatten()
            .map(|(_, constraint)| constraint.as_mut())
    }
}

impl IntoIterator for Constraints {
    type Item = (Node, Box<dyn Constraint>);
    type IntoIter = Box<dyn Iterator<Item = Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.0.into_values().flatten())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum ConstraintConsequence {
    Group(Node, Node),
    Ty(Node, ConstructedTy),
    Instance(Instance, bool),
}

impl ConstraintConsequence {
    pub fn sort_key(&self) -> impl Ord + use<> {
        match self {
            ConstraintConsequence::Ty(..) => 0,
            ConstraintConsequence::Group(..) => 1,
            ConstraintConsequence::Instance(..) => 2,
        }
    }

    pub fn relevant_nodes(&self) -> Vec<Node> {
        match self {
            ConstraintConsequence::Group(left, right) => vec![*left, *right],
            ConstraintConsequence::Ty(node, _) => vec![*node],
            ConstraintConsequence::Instance(_, _) => Vec::new(),
        }
    }

    fn requires_explain_full(&self, db: &Db) -> bool {
        match *self {
            ConstraintConsequence::Group(node, _) => {
                // Hide expressions without conflicts by default
                if db
                    .get(node)
                    .and_then(|Typed(group)| group.as_ref())
                    .is_none_or(|group| group.tys().count() == 1)
                {
                    return true;
                }
            }
            ConstraintConsequence::Ty(node, _) => {
                // Hide complex expressions by default
                if db
                    .get(node)
                    .is_some_and(|Children(children)| !children.is_empty())
                {
                    return true;
                }
            }
            ConstraintConsequence::Instance(ref instance, resolved) => {
                // Hide resolved non-error instances by default
                if resolved
                    && db
                        .get(instance.node)
                        .and_then(|Defined(definition)| {
                            definition.downcast_ref::<InstanceDefinition>()
                        })
                        .is_none_or(|definition| !definition.error)
                {
                    return true;
                }
            }
        }

        false
    }

    pub fn should_render(&self, db: &Db, ctx: &mut RenderCtx) -> bool {
        match ctx.options.explain {
            ExplainOptions::None => return false,
            ExplainOptions::Enabled => {
                if self.requires_explain_full(db) {
                    return false;
                }
            }
            ExplainOptions::Full => {}
        };

        match *self {
            ConstraintConsequence::Group(left, right) => {
                if left == right || db.is_hidden(left) || db.is_hidden(right) {
                    return false;
                }

                let Some(Typed(Some(group))) = db.get(left) else {
                    return false;
                };

                // Hide obvious consequences involving type annotations
                if group.get_rank(left) == NodeRank::Type || group.get_rank(right) == NodeRank::Type
                {
                    return false;
                }

                let variable_definition = |node| {
                    db.get::<Resolved>(node)
                        .map_or_default(|Resolved { definitions, .. }| definitions.iter().copied())
                        .chain([node])
                        .filter_map(|node| db.get::<Defined>(node))
                        .find_map(|Defined(definition)| {
                            definition.downcast_ref::<VariableDefinition>()
                        })
                };

                // Hide obvious consequences involving uses of the same variable
                if variable_definition(left).is_some() && variable_definition(right).is_some() {
                    return false;
                }

                true
            }
            ConstraintConsequence::Ty(node, ref ty) => {
                if db.is_hidden(node) {
                    return false;
                }

                // Hide obvious consequences involving type annotations
                if db
                    .get(node)
                    .and_then(|Typed(group)| group.as_ref())
                    .is_some_and(|group| group.get_rank(node) >= NodeRank::Annotated)
                {
                    return false;
                }

                ty.should_render_consequences(db, ctx, node)
            }
            ConstraintConsequence::Instance(_, _) => true,
        }
    }
}

impl ConstraintConsequence {
    pub fn render_into_list<'a>(
        &'a self,
        db: &'a Db,
        ctx: &mut RenderCtx,
        list: &mut ListBuilder<'a>,
    ) {
        match *self {
            ConstraintConsequence::Group(left, right) => {
                ctx.string("This means ");
                ctx.node(left);
                ctx.string(" must have the same type as ");
                ctx.node(right);
            }
            ConstraintConsequence::Ty(node, ref ty) => {
                ctx.string("This means ");
                ty.render_consequences(db, ctx, list, node);
            }
            ConstraintConsequence::Instance(ref instance, resolved) => {
                if resolved {
                    let mut instance_string = String::new();
                    if let Some(definition) =
                        db.get(instance.node).and_then(|Defined(definition)| {
                            definition.downcast_ref::<InstanceDefinition>()
                        })
                    {
                        if definition.default {
                            write!(instance_string, "default ").unwrap();
                        }

                        if definition.error {
                            write!(instance_string, "error ").unwrap();
                        }
                    }

                    write!(
                        instance_string,
                        "instance ({})",
                        instance.display(db, &ctx.options.relevant, ctx.options.prefer)
                    )
                    .unwrap();

                    ctx.string("This uses ");
                    ctx.link(instance_string, instance.node);
                } else {
                    ctx.string("This requires ");
                    ctx.render(db, instance);
                }
            }
        }
    }
}
