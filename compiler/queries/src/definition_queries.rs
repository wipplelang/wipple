use crate::{Comments, comments_without_links};
use levenshtein::levenshtein;
use std::{
    collections::{BTreeMap, BTreeSet},
    ops::ControlFlow,
};
use wipple_core::{
    db::{Db, Node},
    facts::{Parent, Syntax},
    span::Str,
    visit::{
        Resolved, Scope,
        definitions::{
            ConstantDefinition, Defined, Definition, InstanceDefinition, TraitDefinition,
            TypeDefinition, VariableDefinition,
        },
    },
};

pub fn definitions(db: &Db, node: Node) -> Option<&BTreeSet<Node>> {
    db.get::<Resolved>(node)
        .map(|resolved| &resolved.definitions)
}

pub fn references(db: &Db, node: Node) -> Vec<Node> {
    let mut references = Vec::new();
    db.for_each_fact::<Resolved, ()>(&mut |_, other, resolved| {
        if resolved.definitions.contains(&node) {
            references.push(other);
        }

        ControlFlow::Continue(())
    });

    references
}

pub fn unresolved(db: &Db, node: Node) -> Option<(Str, Option<(Node, Str)>)> {
    let resolved = db.get::<Resolved>(node)?;

    if !resolved.definitions.is_empty() {
        return None;
    }

    let mut suggestion = None;
    db.for_each_fact::<Defined, ()>(&mut |_, node, Defined(definition)| {
        if let Some(name) = definition.name() {
            let distance = levenshtein(&resolved.name, &name);

            if distance < 3
                && suggestion
                    .as_ref()
                    .is_none_or(|(_, _, current)| distance < *current)
            {
                suggestion = Some((node, name.clone(), distance));
            }
        }

        ControlFlow::Continue(())
    });

    let suggestion = suggestion.map(|(node, name, _)| (node, name));

    Some((resolved.name.clone(), suggestion))
}

pub fn ambiguous(db: &Db, node: Node) -> Option<(Node, Str, &BTreeSet<Node>)> {
    let resolved = db.get::<Resolved>(node)?;
    (resolved.definitions.len() > 1).then_some((node, resolved.name.clone(), &resolved.definitions))
}

#[derive(Debug, Clone)]
pub struct Documentation {
    pub name: Option<Str>,
    pub kind: &'static str,
    pub declaration: String,
    pub comments: Comments,
}

pub fn documentation(db: &Db, node: Node) -> Option<Documentation> {
    let Defined(definition) = db.get(node)?;
    let source = &db.ast(&db.get::<Syntax>(node)?.0).span(db).source;

    let kind = if definition.downcast_ref::<VariableDefinition>().is_some() {
        "variable"
    } else if definition.downcast_ref::<ConstantDefinition>().is_some() {
        "constant"
    } else if definition.downcast_ref::<TypeDefinition>().is_some() {
        "type"
    } else if definition.downcast_ref::<TraitDefinition>().is_some() {
        "trait"
    } else if definition.downcast_ref::<InstanceDefinition>().is_some() {
        "instance"
    } else {
        return None;
    };

    let comments = comments_without_links(db, node)?;

    if comments.comments.is_empty() {
        return None;
    }

    let instance_declarations = references(db, node)
        .into_iter()
        .filter(|&reference| {
            db.get(reference)
                .and_then(|Defined(definition)| definition.downcast_ref::<InstanceDefinition>())
                .is_some()
        })
        .filter_map(|reference| {
            let source = &db.ast(&db.get::<Syntax>(reference)?.0).span(db).source;
            Some(format!("\n{source}"))
        })
        .collect::<String>();

    Some(Documentation {
        name: definition.name(),
        kind,
        declaration: format!("{source}{instance_declarations}"),
        comments,
    })
}

pub fn scopes(db: &Db, node: Node) -> Vec<&BTreeMap<Str, BTreeMap<Node, Box<dyn Definition>>>> {
    std::iter::successors(Some(node), |&node| {
        db.get(node).map(|Parent(parent)| *parent)
    })
    .filter_map(|node| db.get(node).map(|Scope { definitions }| definitions))
    .collect()
}
