use crate::{
    nodes::{IsType, TypeParameterNode},
    queries::QueryCtx,
    typecheck::{ConstructedTypeTag, Typed},
    visit::{Defined, Definition, Resolved},
};

pub fn highlight_type(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(())) {
    if ctx.db.contains::<IsType>(&ctx.node)
        && ctx.node.downcast_ref::<TypeParameterNode>().is_none()
    {
        f(());
    }
}

pub fn highlight_trait(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(())) {
    let definition_node = ctx
        .db
        .get::<Resolved>(&ctx.node)
        .and_then(|Resolved { definitions, .. }| definitions.into_iter().next())
        .unwrap_or_else(|| ctx.node.clone());

    if let Some(Defined(Definition::Trait(_))) = ctx.db.get(&definition_node) {
        f(());
    }
}

pub fn highlight_function(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(())) {
    highlight_tag(ctx, |tag| matches!(tag, ConstructedTypeTag::Function), f);
}

pub fn highlight_type_parameter(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(())) {
    highlight_tag(
        ctx,
        |tag| matches!(tag, ConstructedTypeTag::Parameter(_)),
        f,
    );
}

fn highlight_tag(
    ctx: &QueryCtx<'_>,
    filter: impl Fn(&ConstructedTypeTag) -> bool,
    f: &mut dyn FnMut(()),
) {
    if ctx.db.get::<Resolved>(&ctx.node).is_none() {
        return;
    }

    if ctx.db.get::<Defined>(&ctx.node).is_some() {
        return;
    }

    let Some(Typed { group: Some(group) }) = ctx.db.get(&ctx.node) else {
        return;
    };

    if group.types.first().is_some_and(|ty| filter(&ty.tag)) {
        f(());
    }
}
