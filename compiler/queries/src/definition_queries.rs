use crate::{Comments, comments_without_links};
use levenshtein::levenshtein;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use wipple_core::{
    arcstr::Substr,
    db::{Db, Node},
    facts::{Parent, Syntax},
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
    db.collect_facts::<Resolved>()
        .into_iter()
        .filter(|(_, resolved)| resolved.definitions.contains(&node))
        .map(|(other, _)| other)
        .collect()
}

pub fn unresolved(db: &Db, node: Node) -> Option<(&Substr, Option<(Node, Substr)>)> {
    let resolved = db.get::<Resolved>(node)?;

    if !resolved.definitions.is_empty() {
        return None;
    }

    let suggestion = db
        .collect_facts::<Defined>()
        .into_iter()
        .filter_map(|(node, Defined(definition))| {
            let name = definition.name()?;
            let distance = levenshtein(&resolved.name, name);

            (distance < 3).then_some((node, name.clone(), distance))
        })
        .min_by_key(|(_, _, distance)| *distance)
        .map(|(node, name, _)| (node, name));

    Some((&resolved.name, suggestion))
}

pub fn ambiguous(db: &Db, node: Node) -> Option<(Node, Substr, &BTreeSet<Node>)> {
    let resolved = db.get::<Resolved>(node)?;
    (resolved.definitions.len() > 1).then_some((node, resolved.name.clone(), &resolved.definitions))
}

#[derive(Debug, Clone)]
pub struct Documentation {
    pub name: Option<Substr>,
    pub kind: &'static str,
    pub declaration: String,
    pub comments: Comments,
}

pub fn documentation(db: &Db, node: Node) -> Option<Documentation> {
    let Defined(definition) = db.get(node)?;
    let source = &db.get::<Syntax>(node)?.span().source;

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
        .filter_map(|reference| {
            let Defined(definition) = db.get(reference)?;
            let definition = definition.downcast_ref::<InstanceDefinition>()?;
            let source = &db.get::<Syntax>(reference)?.span().source;

            let default = definition.attributes.default.then_some("[default] ");
            let error = definition.attributes.error.then_some("[error] ");

            Some(format!(
                "\n{}{}{}",
                default.unwrap_or_default(),
                error.unwrap_or_default(),
                source
            ))
        })
        .collect::<String>();

    Some(Documentation {
        name: definition.name().cloned(),
        kind,
        declaration: format!("{source}{instance_declarations}"),
        comments,
    })
}

pub fn scopes(db: &Db, node: Node) -> Vec<&HashMap<Substr, BTreeMap<Node, Box<dyn Definition>>>> {
    std::iter::successors(Some(node), |&node| {
        db.get(node).map(|Parent(parent)| *parent)
    })
    .filter_map(|node| db.get(node).map(|Scope { definitions }| definitions))
    .collect()
}
