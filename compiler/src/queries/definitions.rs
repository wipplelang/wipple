use crate::{
    database::NodeRef,
    queries::{QueriedComments, QueryCtx, comments, find_for},
    visit::{Defined, Definition, Resolved},
};
use levenshtein::levenshtein;

pub fn definitions(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(Vec<NodeRef>)) {
    let Some(Resolved { definitions, .. }) = ctx.db.get(&ctx.node) else {
        return;
    };

    f(definitions);
}

pub fn references(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(NodeRef)) {
    for (other, Resolved { definitions, .. }) in ctx.db.iter() {
        if definitions.contains(&ctx.node) {
            f(other);
        }
    }
}

pub fn unresolved(ctx: &QueryCtx<'_>, f: &mut dyn FnMut((String, Option<NodeRef>))) {
    let Some(Resolved { name, definitions }) = ctx.db.get(&ctx.node) else {
        return;
    };

    if definitions.is_empty() {
        let suggestion = ctx
            .db
            .iter::<Defined>()
            .filter_map(|(node, Defined(definition))| Some((node, definition.name()?.to_string())))
            .map(|(node, definition)| (node, levenshtein(&name, &definition)))
            .min_by_key(|(_, distance)| *distance)
            .filter(|(_, distance)| *distance <= 3)
            .map(|(node, _)| node);

        f((name, suggestion));
    }
}

pub fn ambiguous(ctx: &QueryCtx<'_>, f: &mut dyn FnMut((NodeRef, String, Vec<NodeRef>))) {
    let Some(Resolved { name, definitions }) = ctx.db.get(&ctx.node) else {
        return;
    };

    if definitions.len() > 1 {
        f((ctx.node.clone(), name, definitions));
    }
}

#[derive(Debug)]
pub struct QueriedDocumentation {
    pub name: Option<String>,
    pub kind: Option<&'static str>,
    pub declaration: String,
    pub comments: QueriedComments,
}

pub fn documentation(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(QueriedDocumentation)) {
    let Some(Defined(definition)) = ctx.db.get(&ctx.node) else {
        return;
    };

    let Some(comments) = find_for(ctx.db, &definition.node(), comments) else {
        return;
    };

    let kind = match definition {
        Definition::Variable(..) => Some("variable"),
        Definition::Constant(..) => Some("constant"),
        Definition::Type(..) => Some("type"),
        Definition::Trait(..) => Some("trait"),
        Definition::Instance(..) => Some("instance"),
        Definition::TypeParameter(..) => None,
        Definition::MarkerConstructor(..) => None,
        Definition::StructureConstructor(..) => None,
        Definition::VariantConstructor(..) => None,
    };

    f(QueriedDocumentation {
        name: definition.name().map(|s| s.to_string()),
        kind,
        declaration: ctx.db.span(&definition.node()).as_definition_source(),
        comments,
    })
}
