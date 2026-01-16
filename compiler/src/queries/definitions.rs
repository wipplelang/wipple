use crate::{
    database::NodeRef,
    queries::{QueriedComments, QueryCtx, comments, find_for},
    visit::{Defined, Resolved},
};

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

pub fn unresolved(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(String)) {
    let Some(Resolved { name, definitions }) = ctx.db.get(&ctx.node) else {
        return;
    };

    if definitions.is_empty() {
        f(name);
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

    f(QueriedDocumentation {
        name: definition.name().map(|s| s.to_string()),
        declaration: ctx.db.span(&definition.node()).as_definition_source(),
        comments,
    })
}
