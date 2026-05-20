pub mod attributes;
pub mod definitions;
pub mod exhaustiveness;

use crate::{
    codegen::CodegenValue,
    db::{Db, Fact, Node},
    facts::{Children, Codegen, Parent, Syntax},
    render::{Render, RenderCtx},
    span::Span,
    typecheck::{
        constraints::{Constraint, generic_constraint::GenericConstraint},
        solver::{Substitutions, SubstitutionsKey},
        ty::Ty,
    },
    visit::{
        definitions::{Defined, Definition},
        exhaustiveness::{MatchPath, MatchPathSegment, Matches},
    },
};
use arcstr::Substr;
use dyn_clone::DynClone;
use serde::{Deserialize, Serialize};
use std::{
    any::Any,
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
    mem,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Resolved {
    pub name: Substr,
    pub definitions: BTreeSet<Node>,
}

#[typetag::serde]
impl Fact for Resolved {}

impl Render for Resolved {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        if self.definitions.is_empty() {
            ctx.string("unresolved");
            return;
        }

        ctx.string(format!(
            "resolved to {} definition(s): ",
            self.definitions.len()
        ));

        for (index, &definition) in self.definitions.iter().enumerate() {
            if index > 0 {
                ctx.string(", ");
            }

            ctx.node(definition);
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Scope {
    pub definitions: BTreeMap<Substr, BTreeMap<Node, Box<dyn Definition>>>,
}

#[typetag::serde]
impl Fact for Scope {}

impl Render for Scope {}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DefinitionConstraints(pub Vec<Box<dyn Constraint>>);

#[typetag::serde]
impl Fact for DefinitionConstraints {}

impl Render for DefinitionConstraints {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("has definition constraints");
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TypeParameters(pub Vec<Node>);

#[typetag::serde]
impl Fact for TypeParameters {}

impl Render for TypeParameters {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("has type parameters");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Captures(pub BTreeSet<Node>);

#[typetag::serde]
impl Fact for Captures {}

impl Render for Captures {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        if self.0.is_empty() {
            return;
        }

        ctx.string("captures ");
        for (index, &capture) in self.0.iter().enumerate() {
            if index > 0 {
                ctx.string(", ");
            }

            ctx.node(capture);
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsCaptured;

#[typetag::serde]
impl Fact for IsCaptured {}

impl Render for IsCaptured {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsMutated;

#[typetag::serde]
impl Fact for IsMutated {}

impl Render for IsMutated {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("is mutated");
    }
}

#[typetag::serde]
pub trait Visit: Debug + DynClone + Any + Send + Sync {
    fn span(&self) -> &Span;

    fn is_hidden(&self) -> bool {
        false
    }

    fn visit_definitions(
        &self,
        db: &mut Db,
        node: Node,
        visitor: &mut Visitor,
    ) -> Vec<(Node, Box<dyn Definition>)> {
        let _ = db;
        let _ = node;
        let _ = visitor;
        Vec::new()
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        let _ = db;
        let _ = node;
        let _ = visitor;
    }

    fn type_name(&self) -> &'static str {
        let mut type_name = std::any::type_name_of_val(self);

        if let Some((_, last)) = type_name.rsplit_once("::") {
            type_name = last;
        }

        type_name
    }
}

dyn_clone::clone_trait_object!(Visit);

impl dyn Visit {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        (self as &dyn Any).downcast_ref()
    }

    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        (self as &mut dyn Any).downcast_mut()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisitAs {
    pub node: Node,
    pub syntax: Box<dyn Visit>,
}

#[typetag::serde]
impl Visit for VisitAs {
    fn span(&self) -> &Span {
        self.syntax.span()
    }

    fn is_hidden(&self) -> bool {
        self.syntax.is_hidden()
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        assert_eq!(self.node, node);
        self.syntax.visit(db, node, visitor);
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Hidden(pub Box<dyn Visit>);

impl Hidden {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(visit: impl Visit) -> Box<dyn Visit> {
        Box::new(Hidden(Box::new(visit)))
    }
}

#[typetag::serde]
impl Visit for Hidden {
    fn span(&self) -> &Span {
        self.0.span()
    }

    fn is_hidden(&self) -> bool {
        true
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        self.0.visit(db, node, visitor);
    }
}

#[derive(Debug, Default)]
pub struct ScopeValues {
    names: BTreeMap<Substr, Vec<Node>>,
    defined: BTreeMap<Node, Box<dyn Definition>>,
    captured: BTreeSet<Node>,
}

impl ScopeValues {
    pub fn peek_as<T: Definition + Clone>(&self, name: &Substr) -> Vec<(Node, T)> {
        self.peek_matching(name, |_, definition| {
            definition.downcast_ref::<T>().cloned()
        })
    }

    fn peek_matching<T>(
        &self,
        name: &Substr,
        mut f: impl FnMut(Node, &dyn Definition) -> Option<T>,
    ) -> Vec<(Node, T)> {
        self.names
            .get(name)
            .into_iter()
            .flatten()
            .filter_map(|&node| {
                let definition = self.defined.get(&node).unwrap();
                f(node, definition.as_ref()).map(move |result| (node, result))
            })
            .collect()
    }
}

#[derive(Debug)]
pub struct CurrentDefinition {
    pub node: Node,
    pub implicit_type_parameters: bool,
    pub within_constant_value: bool,
}

impl CurrentDefinition {
    fn new(node: Node) -> Self {
        CurrentDefinition {
            node,
            implicit_type_parameters: false,
            within_constant_value: false,
        }
    }

    pub fn constraints<'a>(&self, db: &'a mut Db) -> &'a [Box<dyn Constraint>] {
        &db.get_mut_or_default::<DefinitionConstraints>(self.node).0
    }
}

#[derive(Debug)]
pub struct CurrentMatch {
    pub root: Option<Node>,
    pub arm: Option<Node>, // opt into exhaustiveness checking
    pub value: Node,
    pub allow_or: bool,
    pub allow_set: bool,
    pub mutable: bool,
    pub path: MatchPath,
}

#[derive(Debug, Default)]
pub struct VisitResult {
    pub top_level_statements: Vec<Node>,
    pub constraints: Vec<Box<dyn Constraint>>,
    pub substitutions: Vec<Substitutions>,
    pub definitions: BTreeMap<Substr, Vec<(Node, Box<dyn Definition>)>>,
    pub utilities: VisitUtilities,
}

pub type VisitUtilities = BTreeMap<Substr, Node>;

static UTILITIES: &[&str] = &["Mismatched"];

#[derive(Debug)]
pub struct Visitor {
    pub current_node: Node,
    pub current_definition: Option<CurrentDefinition>,
    pub current_match: Option<CurrentMatch>,
    scopes: Vec<(Option<Node>, ScopeValues)>,
    top_level_statements: Vec<(Node, Box<dyn Visit>)>,
    constraints: Vec<Box<dyn Constraint>>,
    substitutions: Vec<Substitutions>,
}

impl Visitor {
    pub fn new(
        db: &mut Db,
        root: Node,
        definitions: impl IntoIterator<Item = BTreeMap<Substr, Vec<(Node, Box<dyn Definition>)>>>,
        substitutions: impl IntoIterator<Item = (BTreeMap<Node, Node>, BTreeMap<Node, Ty>)>,
    ) -> Self {
        let mut scope = ScopeValues::default();
        for names in definitions {
            for (name, definitions) in names {
                for (node, definition) in definitions {
                    scope.names.entry(name.clone()).or_default().push(node);
                    scope.defined.insert(node, definition);
                }
            }
        }

        let substitutions = substitutions
            .into_iter()
            .map(|(nodes, parameters)| Substitutions {
                record_instantiated_parameters: false,
                nodes,
                parameters,
            })
            .collect::<Vec<_>>();

        let mut visitor = Visitor {
            current_node: root,
            current_definition: None,
            current_match: None,
            scopes: vec![(None, scope)],
            top_level_statements: Default::default(),
            constraints: Default::default(),
            substitutions,
        };

        // Register `definitions` in `root`
        let scope = visitor.pop_scope(db);
        visitor.scopes.push((None, scope));

        visitor
    }

    pub fn visit(&mut self, db: &mut Db, value: Box<dyn Visit>) -> Node {
        let node = value
            .downcast_ref::<VisitAs>()
            .map_or_else(|| db.node(), |value| value.node);

        if value.is_hidden() {
            db.hide(node);
        }

        self.visit_as(db, value, node);

        node
    }

    pub fn visit_as(&mut self, db: &mut Db, value: Box<dyn Visit>, node: Node) {
        let parent = mem::replace(&mut self.current_node, node);

        db.insert(node, Parent(parent));
        db.get_mut_or_default::<Children>(node).0.push(node);

        db.insert(node, Syntax(value.clone()));

        value.visit(db, node, self);

        self.current_node = parent;
    }

    #[must_use]
    pub fn visit_statements(
        &mut self,
        db: &mut Db,
        statements: impl IntoIterator<Item = Box<dyn Visit>>,
    ) -> Vec<(Node, Box<dyn Visit>)> {
        statements
            .into_iter()
            .map(|statement| {
                let node = db.node();

                for (node, definition) in statement.visit_definitions(db, node, self) {
                    self.define(db, node, definition);
                }

                (node, statement)
            })
            .collect()
    }

    pub fn top_level_statement(&mut self, node: Node, statement: Box<dyn Visit>) {
        self.top_level_statements.push((node, statement));
    }

    pub fn constraint(&mut self, db: &mut Db, constraint: impl Constraint) {
        if let Some(definition) = &mut self.current_definition {
            let mut constraint = Box::new(constraint) as Box<dyn Constraint>;
            if definition.within_constant_value {
                constraint = Box::new(GenericConstraint(constraint));
            }

            db.get_mut_or_default::<DefinitionConstraints>(definition.node)
                .0
                .push(constraint);
        } else {
            self.constraints.push(Box::new(constraint));
        }
    }

    pub fn substitutions(
        &mut self,
        nodes: BTreeMap<Node, Node>,
        parameters: BTreeMap<Node, Ty>,
    ) -> SubstitutionsKey {
        let key = SubstitutionsKey(self.substitutions.len());

        self.substitutions.push(Substitutions {
            record_instantiated_parameters: false,
            nodes,
            parameters,
        });

        key
    }

    pub fn record_instantiated_parameters(&mut self, substitutions: SubstitutionsKey) {
        self.substitutions[substitutions.0].record_instantiated_parameters = true;
    }

    pub fn codegen(&mut self, db: &mut Db, node: Node, value: impl CodegenValue) {
        db.insert(node, Codegen(Box::new(value)));
    }

    pub fn push_scope(&mut self, db: &mut Db, owner: Node) {
        let _ = self.push_scope_with_statements(db, owner, []);
    }

    #[must_use]
    pub fn push_scope_with_statements(
        &mut self,
        db: &mut Db,
        owner: impl Into<Option<Node>>,
        statements: impl IntoIterator<Item = Box<dyn Visit>>,
    ) -> Vec<(Node, Box<dyn Visit>)> {
        self.scopes.push((owner.into(), ScopeValues::default()));
        self.visit_statements(db, statements)
    }

    pub fn current_scope_mut(&mut self) -> &mut ScopeValues {
        &mut self.scopes.last_mut().unwrap().1
    }

    pub fn pop_scope(&mut self, db: &mut Db) -> ScopeValues {
        let (owner, scope) = self.scopes.pop().unwrap();

        for (name, definitions) in &scope.names {
            for &node in definitions {
                let definition = scope.defined.get(&node).unwrap();

                db.get_mut_or_default::<Scope>(self.current_node)
                    .definitions
                    .entry(name.clone())
                    .or_default()
                    .insert(node, definition.clone());
            }
        }

        if let Some(owner) = owner {
            db.insert(owner, Captures(scope.captured.clone()));
        }

        scope
    }

    pub fn peek_as<T: Definition + Clone>(&mut self, name: &Substr) -> Vec<(Node, T)> {
        self.peek_matching(name, |_, definition| {
            definition.downcast_ref::<T>().cloned()
        })
    }

    pub fn peek_matching<T>(
        &mut self,
        name: &Substr,
        mut f: impl FnMut(Node, &dyn Definition) -> Option<T>,
    ) -> Vec<(Node, T)> {
        self.scopes
            .iter()
            .rev()
            .find_map(|(_, scope)| {
                let matches = scope.peek_matching(name, &mut f);
                (!matches.is_empty()).then_some(matches)
            })
            .unwrap_or_default()
    }

    pub fn resolve_as<T: Definition + Clone>(
        &mut self,
        db: &mut Db,
        name: &Substr,
        node: Node,
    ) -> Option<(Node, T)> {
        self.resolve_matching(db, name, node, |_, definition| {
            definition.downcast_ref::<T>().cloned()
        })
    }

    pub fn resolve_matching<T>(
        &mut self,
        db: &mut Db,
        name: &Substr,
        node: Node,
        f: impl FnMut(Node, &dyn Definition) -> Option<T>,
    ) -> Option<(Node, T)> {
        let matches = self.peek_matching(name, f);

        let mut result = None;
        let mut definitions = BTreeSet::new();
        for (node, next) in matches {
            if result.is_none() {
                result = Some((node, next));
            }

            definitions.insert(node);
        }

        db.insert(
            node,
            Resolved {
                name: name.clone(),
                definitions,
            },
        );

        result
    }

    pub fn define(&mut self, db: &mut Db, node: Node, definition: Box<dyn Definition>) {
        if let Some(name) = definition.name() {
            self.current_scope_mut()
                .names
                .entry(name.clone())
                .or_default()
                .push(node);
        }

        self.current_scope_mut()
            .defined
            .insert(node, definition.clone());

        db.insert(node, Defined(definition));
    }

    pub fn capture(&mut self, node: Node) -> bool {
        let mut captured = false;
        for (_, scope) in self.scopes.iter_mut().rev() {
            if scope.defined.contains_key(&node) {
                break;
            }

            scope.captured.insert(node);
            captured = true;
        }

        captured
    }

    pub fn defining<T>(
        &mut self,
        db: &mut Db,
        node: Node,
        f: impl FnOnce(&mut Db, &mut Self) -> T,
    ) -> T {
        let existing_definition = self
            .current_definition
            .replace(CurrentDefinition::new(node));

        let definition_value = f(db, self);

        self.current_definition = existing_definition;

        definition_value
    }

    pub fn within_definition<T: Definition + Clone>(
        &mut self,
        db: &mut Db,
        node: Node,
        f: impl FnOnce(&mut Db, &mut Self, T),
    ) {
        let existing_definition = self
            .current_definition
            .replace(CurrentDefinition::new(node));

        let definition = self
            .scopes
            .iter_mut()
            .rev()
            .find_map(|(_, scope)| {
                scope
                    .defined
                    .get(&node)
                    .map(|definition| definition.downcast_ref::<T>().unwrap().clone())
            })
            .unwrap();

        f(db, self, definition);

        self.current_definition = existing_definition;
    }

    pub fn with_definition_flag<T>(
        &mut self,
        flag: fn(&mut CurrentDefinition) -> &mut bool,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let prev = mem::replace(flag(self.current_definition.as_mut().unwrap()), true);
        let result = f(self);
        *flag(self.current_definition.as_mut().unwrap()) = prev;
        result
    }

    pub fn matching<T>(
        &mut self,
        value: Node,
        flags: impl FnOnce(&mut CurrentMatch),
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let existing_match = self.current_match.as_ref();

        let mut current_match = CurrentMatch {
            root: existing_match.and_then(|m| m.root),
            arm: existing_match.and_then(|m| m.arm),
            value,
            allow_or: false,
            allow_set: false,
            mutable: true,
            path: existing_match.map(|m| m.path.clone()).unwrap_or_default(),
        };

        flags(&mut current_match);

        let existing_match = self.current_match.replace(current_match);

        let result = f(self);

        self.current_match = existing_match;

        result
    }

    pub fn visit_matching(
        &mut self,
        db: &mut Db,
        pattern: Box<dyn Visit>,
        segment: impl Into<Option<MatchPathSegment>>,
    ) -> (Node, Node) {
        let temporary = db.node();

        let pattern = self.matching(
            temporary,
            |m| m.allow_or = true,
            |visitor| {
                let current_match = visitor.current_match.as_mut().unwrap();

                if current_match.root.is_none() {
                    current_match.root = Some(temporary);
                }

                if let Some(segment) = segment.into() {
                    current_match.path.0.push(segment);
                }

                visitor.visit(db, pattern)
            },
        );

        (pattern, temporary)
    }

    pub fn set_matches(&mut self, db: &mut Db, terminal: Option<MatchPathSegment>) -> Node {
        let current_match = self.current_match.as_ref().unwrap();

        db.insert(
            self.current_node,
            Matches {
                value: current_match.root.unwrap(),
                arm: current_match.arm,
                path: terminal.map(|segment| {
                    let mut path = current_match.path.clone();
                    path.0.push(segment);
                    path
                }),
            },
        );

        current_match.value
    }

    pub fn finish(mut self, db: &mut Db) -> VisitResult {
        let mut result = VisitResult::default();

        for (statement_node, statement) in mem::take(&mut self.top_level_statements) {
            self.visit_as(db, statement, statement_node);
            result.top_level_statements.push(statement_node);
        }

        result.constraints.append(&mut self.constraints);
        result.substitutions.append(&mut self.substitutions);

        for &name in UTILITIES {
            let name = Substr::from(name);

            let utilities = self.peek_matching(&name, |_, _| Some(()));

            if let [(utility, _)] = utilities.as_slice() {
                result.utilities.insert(name, *utility);
            }
        }

        let scope = self.pop_scope(db);
        for (name, nodes) in scope.names {
            for node in nodes {
                let definition = scope.defined.get(&node).unwrap().clone();

                result
                    .definitions
                    .entry(name.clone())
                    .or_default()
                    .push((node, definition));
            }
        }

        result
    }
}
