use crate::{
    database::{Db, Fact, HiddenNode, NodeRef, Parent, Render},
    typecheck::Constraint,
    visit::{Defined, Definition},
};
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, VecDeque},
    ops::{Deref, DerefMut},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct Resolved {
    pub name: String,
    pub definitions: Vec<NodeRef>,
}

impl Fact for Resolved {}

impl Render for Resolved {
    fn write(&self, w: &mut dyn std::fmt::Write, db: &Db) -> std::fmt::Result {
        if self.definitions.is_empty() {
            return write!(w, "unresolved");
        }

        write!(w, "resolved to {} definition(s): ", self.definitions.len())?;

        for (index, definition) in self.definitions.iter().enumerate() {
            if index > 0 {
                write!(w, ", ")?;
            }

            write!(w, "{}", db.render(definition))?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct DefinitionConstraints(pub Vec<Box<dyn Constraint>>);

impl Fact for DefinitionConstraints {}

impl Render for DefinitionConstraints {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "has definition constraints")
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypeParameters(pub Vec<NodeRef>);

impl Fact for TypeParameters {}

impl Render for TypeParameters {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "has type parameters")
    }
}

pub trait Visit {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>);
}

pub struct Visitor<'db> {
    pub db: &'db mut Db,
    pub scopes: Vec<Rc<RefCell<Scope<Rc<RefCell<Definition>>>>>>,
    current_node: Option<NodeRef>,
    current_definition: Option<CurrentDefinition>,
    current_match: Option<CurrentMatch>,
    queue: Queue<'db>,
    constraints: Vec<Box<dyn Constraint>>,
}

impl Deref for Visitor<'_> {
    type Target = Db;

    fn deref(&self) -> &Self::Target {
        self.db
    }
}

impl DerefMut for Visitor<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.db
    }
}

struct Utility {
    name: &'static str,
    match_definition: fn(&Definition) -> Option<NodeRef>,
}

const UTILITIES: &[Utility] = &[Utility {
    name: "Mismatched",
    match_definition: |definition| match definition {
        Definition::Trait(definition) => Some(definition.node.clone()),
        _ => None,
    },
}];

#[derive(Debug)]
pub struct Result {
    pub constraints: Vec<Box<dyn Constraint>>,
    pub scope: Scope<Definition>,
    pub utilities: HashMap<&'static str, NodeRef>,
}

impl<'db> Visitor<'db> {
    pub fn new(db: &'db mut Db, scopes: Vec<Scope<Definition>>) -> Self {
        let mut visitor = Visitor {
            db,
            scopes: scopes
                .into_iter()
                .map(|scope| {
                    Rc::new(RefCell::new(
                        scope
                            .into_iter()
                            .map(|(name, definitions)| {
                                (
                                    name,
                                    definitions
                                        .into_iter()
                                        .map(|definition| Rc::new(RefCell::new(definition)))
                                        .collect(),
                                )
                            })
                            .collect(),
                    ))
                })
                .collect(),
            current_node: Default::default(),
            current_definition: Default::default(),
            current_match: Default::default(),
            queue: Default::default(),
            constraints: Default::default(),
        };

        visitor.push_scope();

        visitor
    }

    pub fn visit(&mut self, node: &NodeRef) {
        let parent = self.current_node.replace(node.clone());

        if let Some(parent) = &parent {
            self.insert(node, Parent(parent.clone()));
        }

        node.visit(node, self);

        self.current_node = parent;
    }

    pub fn edge(&mut self, from: &NodeRef, to: &NodeRef, label: &'static str) {
        self.db.graph.edge(from, to, label);
    }

    pub fn constraint(&mut self, constraint: Box<dyn Constraint>) {
        if let Some(definition) = &self.current_definition {
            definition.constraint(self.db, constraint);
        } else {
            self.constraints.push(constraint);
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    pub fn peek_scope(&self) -> Ref<'_, Scope<Rc<RefCell<Definition>>>> {
        self.scopes.last().unwrap().borrow()
    }

    pub fn peek_scope_mut(&mut self) -> RefMut<'_, Scope<Rc<RefCell<Definition>>>> {
        self.scopes.last_mut().unwrap().borrow_mut()
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    pub fn resolve<'a, T>(
        &'a mut self,
        name: &'a str,
        node: &NodeRef,
        match_definition: impl Fn(&Definition) -> Option<T> + Copy,
    ) -> Option<T> {
        let (results, nodes): (Vec<_>, Vec<_>) = self
            .peek(name, |definition| {
                match_definition(definition).map(|result| (result, definition.node()))
            })
            .unzip();

        self.db.insert(
            node,
            Resolved {
                name: name.to_string(),
                definitions: nodes,
            },
        );

        results.into_iter().next()
    }

    pub fn peek<'a, T, F: Fn(&Definition) -> Option<T> + Copy>(
        &'a self,
        name: &'a str,
        match_definition: F,
    ) -> impl Iterator<Item = T> + use<'a, T, F> {
        self.scopes
            .iter()
            .rev()
            .find_map(move |scope| {
                scope.borrow().get(name).map(|scope| {
                    scope
                        .iter()
                        .filter_map(|definition| match_definition(&definition.borrow()))
                        .collect::<Vec<_>>()
                })
            })
            .into_iter()
            .flatten()
    }

    pub fn with(
        &mut self,
        name: &str,
        mut f: impl FnMut(&mut Self, &Definition) -> Option<Definition>,
    ) -> bool {
        for scope in self.scopes.iter().rev().cloned().collect::<Vec<_>>() {
            if let Some(definitions) = scope.borrow().get(name) {
                for definition in definitions {
                    let result = f(self, &definition.borrow());
                    if let Some(replacement) = result {
                        *definition.borrow_mut() = replacement;
                        return true;
                    }
                }
            }
        }

        false
    }

    pub fn define(&mut self, name: &str, definition: impl Into<Definition>) {
        self.peek_scope_mut()
            .entry(name.to_string())
            .or_default()
            .push(Rc::new(RefCell::new(definition.into())));
    }

    pub fn defining<T>(&mut self, node: &NodeRef, f: impl FnOnce(&mut Visitor<'_>) -> T) -> T
    where
        T: Into<Definition> + Clone,
    {
        let existing_definition = self.current_definition.clone();
        self.current_definition = Some(CurrentDefinition {
            node: node.clone(),
            implicit_type_parameters: false,
            within_constant_value: false,
        });

        let result_definition = f(self);

        self.current_definition = existing_definition;

        self.db
            .insert(node, Defined(result_definition.clone().into()));

        result_definition
    }

    pub fn current_definition(&self) -> &CurrentDefinition {
        self.current_definition.as_ref().unwrap()
    }

    pub fn current_definition_mut(&mut self) -> &mut CurrentDefinition {
        self.current_definition.as_mut().unwrap()
    }

    pub fn with_implicit_type_parameters<T>(&mut self, f: impl FnOnce(&mut Visitor<'_>) -> T) -> T {
        let previous = self.current_definition().implicit_type_parameters;
        self.current_definition_mut().implicit_type_parameters = true;
        let result = f(self);
        self.current_definition_mut().implicit_type_parameters = previous;
        result
    }

    pub fn within_constant_value<T>(&mut self, f: impl FnOnce(&mut Visitor<'_>) -> T) -> T {
        self.current_definition_mut().within_constant_value = true;
        let result = f(self);
        self.current_definition_mut().within_constant_value = false;
        result
    }

    pub fn current_match(&self) -> &CurrentMatch {
        self.current_match.as_ref().unwrap()
    }

    pub fn matching<T>(
        &mut self,
        temporary: NodeRef,
        allow_set: bool,
        f: impl FnOnce(&mut Visitor<'_>) -> T,
    ) -> T {
        let existing_match = self.current_match.take();

        self.current_match = Some(CurrentMatch {
            node: temporary,
            allow_set,
        });

        let result = f(self);

        self.current_match = existing_match;

        result
    }

    pub fn visit_matching(&mut self, pattern: &NodeRef) -> NodeRef {
        let span = self.db.span(pattern);
        let temporary = self.db.node(span, HiddenNode(None));

        self.matching(temporary.clone(), false, |visitor| {
            visitor.visit(pattern);
        });

        temporary
    }

    pub fn finish(mut self) -> Result {
        while let Some(entry) = self.queue.dequeue() {
            let previous_scopes = self.scopes.clone();
            let previous_definition = self.current_definition.clone();
            self.scopes = entry.scopes;
            self.current_definition = entry.current_definition;

            (entry.f)(&mut self);

            self.scopes = previous_scopes;
            self.current_definition = previous_definition;
        }

        let utilities = self.collect_utilities();

        Result {
            constraints: self.constraints,
            scope: Rc::try_unwrap(self.scopes.pop().unwrap())
                .unwrap()
                .into_inner()
                .into_iter()
                .map(|(name, definitions)| {
                    (
                        name,
                        definitions
                            .into_iter()
                            .map(|definition| Rc::try_unwrap(definition).unwrap().into_inner())
                            .collect(),
                    )
                })
                .collect(),
            utilities,
        }
    }

    pub fn after_type_definitions(&mut self, f: impl FnOnce(&mut Visitor<'_>) + 'static) {
        self.enqueue(|visitor| &mut visitor.queue.after_type_definitions, f);
    }

    pub fn after_all_definitions(&mut self, f: impl FnOnce(&mut Visitor<'_>) + 'static) {
        self.enqueue(|visitor| &mut visitor.queue.after_all_definitions, f);
    }

    pub fn after_all_expressions(&mut self, f: impl FnOnce(&mut Visitor<'_>) + 'static) {
        self.enqueue(|visitor| &mut visitor.queue.after_all_expressions, f);
    }

    fn collect_utilities(&self) -> HashMap<&'static str, NodeRef> {
        let mut utilities = HashMap::new();
        for utility in UTILITIES {
            let mut definitions = self.peek(utility.name, utility.match_definition);
            if let Some(definition) = definitions.next()
                && definitions.next().is_none()
            {
                utilities.insert(utility.name, definition);
            }
        }

        utilities
    }
}

pub type Scope<T> = HashMap<String, Vec<T>>;

#[derive(Debug, Clone)]
pub struct CurrentDefinition {
    pub node: NodeRef,
    pub implicit_type_parameters: bool,
    within_constant_value: bool,
}

impl CurrentDefinition {
    pub fn constraints(&self, db: &Db) -> Vec<Box<dyn Constraint>> {
        db.get::<DefinitionConstraints>(&self.node)
            .map(|DefinitionConstraints(constraints)| constraints)
            .unwrap_or_default()
    }

    pub fn constraint(&self, db: &mut Db, mut constraint: Box<dyn Constraint>) {
        if self.within_constant_value {
            constraint.info_mut().instantiate = false;
        }

        db.with_fact(&self.node, |DefinitionConstraints(constraints)| {
            constraints.push(constraint);
        });
    }

    pub fn with_implicit_type_parameters(&mut self, f: impl FnOnce(&mut Self)) {
        self.implicit_type_parameters = true;
        f(self);
        self.implicit_type_parameters = false;
    }
}

#[derive(Debug, Clone)]
pub struct CurrentMatch {
    pub node: NodeRef,
    pub allow_set: bool,
}

// Needed so definitions are resolved before nodes that reference them
struct QueuedVisit<'db> {
    f: Box<dyn FnOnce(&mut Visitor<'db>)>,
    scopes: Vec<Rc<RefCell<Scope<Rc<RefCell<Definition>>>>>>,
    current_definition: Option<CurrentDefinition>,
}

#[derive(Default)]
struct Queue<'db> {
    after_type_definitions: VecDeque<QueuedVisit<'db>>,
    after_all_definitions: VecDeque<QueuedVisit<'db>>,
    after_all_expressions: VecDeque<QueuedVisit<'db>>,
}

impl<'db> Visitor<'db> {
    fn enqueue(
        &mut self,
        entry: impl for<'a> FnOnce(&'a mut Visitor<'db>) -> &'a mut VecDeque<QueuedVisit<'db>>,
        f: impl FnOnce(&mut Visitor<'_>) + 'static,
    ) {
        let visit = QueuedVisit {
            f: Box::new(f),
            scopes: self.scopes.clone(),
            current_definition: self.current_definition.clone(),
        };

        entry(self).push_back(visit);
    }
}

impl<'db> Queue<'db> {
    fn dequeue(&mut self) -> Option<QueuedVisit<'db>> {
        let entries = [
            &mut self.after_type_definitions,
            &mut self.after_all_definitions,
            &mut self.after_all_expressions,
        ];

        entries.into_iter().find_map(|queue| queue.pop_front())
    }
}
