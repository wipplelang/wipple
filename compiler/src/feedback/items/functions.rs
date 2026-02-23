use crate::{
    database::NodeRef,
    feedback::{FeedbackCtx, FeedbackRank, RegisteredFeedback},
    nodes::CallExpressionNode,
    queries,
    typecheck::{ConstructedTypeTag, Type},
};

pub fn register(ctx: &mut FeedbackCtx) {
    #[derive(Debug)]
    struct MissingInputsData {
        function: NodeRef,
        nodes: Vec<NodeRef>,
        inputs: Vec<Type>,
    }

    ctx.register(RegisteredFeedback::new(
        "missing-inputs",
        FeedbackRank::Syntax,
        |ctx, f| {
            queries::conflicting_types(ctx, &mut |data| {
                let nodes = [&data.from]
                    .into_iter()
                    .chain(&data.nodes)
                    .cloned()
                    .collect::<Vec<_>>();

                let Some(function) = with_call(ctx, &nodes, |call| call.function.clone()) else {
                    return;
                };

                let [actual, expected] = &data.types[..] else {
                    return;
                };

                if !matches!(expected.tag, ConstructedTypeTag::Function)
                    || !matches!(actual.tag, ConstructedTypeTag::Function)
                {
                    return;
                }

                if actual.children.len() < expected.children.len() {
                    f(MissingInputsData {
                        function,
                        nodes,
                        inputs: expected.children[actual.children.len()..].to_vec(),
                    });
                }
            });
        },
        |data| {
            let related = data
                .nodes
                .iter()
                .filter(|&node| *node != data.function)
                .cloned()
                .collect();

            (data.function.clone(), related)
        },
        |w, data| {
            w.write_node(&data.function);
            w.write_string(" is missing ");

            w.write_list("and", 0, |list| {
                for input in &data.inputs {
                    list.add(move |w| match input {
                        Type::Node(_) => w.write_string("an input"),
                        Type::Constructed(_) => {
                            w.write_string("a ");
                            w.write_type(input.clone());
                        }
                    });
                }
            });
            w.write_string(".");

            w.write_break();
            w.write_string("Try adding ");
            if data.inputs.len() == 1 {
                w.write_string("this input");
            } else {
                w.write_string("these inputs");
            }
            w.write_string(", or double-check your parentheses.");
        },
    ));

    #[derive(Debug)]
    struct ExtraInputsData {
        function: NodeRef,
        nodes: Vec<NodeRef>,
        actual: usize,
        expected: usize,
    }

    ctx.register(RegisteredFeedback::new(
        "extra-input",
        FeedbackRank::Syntax,
        |ctx, f| {
            queries::conflicting_types(ctx, &mut |data| {
                let nodes = [&data.from]
                    .into_iter()
                    .chain(&data.nodes)
                    .cloned()
                    .collect::<Vec<_>>();

                let Some(function) = with_call(ctx, &nodes, |call| call.function.clone()) else {
                    return;
                };

                let [actual, expected] = &data.types[..] else {
                    return;
                };

                if !matches!(expected.tag, ConstructedTypeTag::Function)
                    || !matches!(actual.tag, ConstructedTypeTag::Function)
                {
                    return;
                }

                if actual.children.len() > expected.children.len() {
                    f(ExtraInputsData {
                        function,
                        nodes,
                        actual: actual.children.len().saturating_sub(1),
                        expected: expected.children.len().saturating_sub(1),
                    });
                }
            });
        },
        |data| {
            let related = data
                .nodes
                .iter()
                .filter(|&node| *node != data.function)
                .cloned()
                .collect();

            (data.function.clone(), related)
        },
        |w, data| {
            w.write_node(&data.function);
            w.write_string(" only needs ");
            w.write_number(data.expected, "input", "inputs");
            w.write_string(".");

            w.write_break();
            w.write_string("Try removing ");

            let extra = data.actual.saturating_sub(data.expected);
            if extra == 1 {
                w.write_string("the extra input");
            } else {
                w.write_string(&format!("{extra} extra inputs"));
            }

            w.write_string(" here.");
        },
    ));

    #[derive(Debug)]
    struct NotAFunctionData {
        function: NodeRef,
        nodes: Vec<NodeRef>,
    }

    ctx.register(RegisteredFeedback::new(
        "not-a-function",
        FeedbackRank::Conflicts,
        |ctx, f| {
            queries::conflicting_types(ctx, &mut |data| {
                let nodes = [&data.from]
                    .into_iter()
                    .chain(&data.nodes)
                    .cloned()
                    .collect::<Vec<_>>();

                let Some(function) = with_call(ctx, &nodes, |call| call.function.clone()) else {
                    return;
                };

                let [actual, expected] = &data.types[..] else {
                    return;
                };

                if matches!(actual.tag, ConstructedTypeTag::Function)
                    != matches!(expected.tag, ConstructedTypeTag::Function)
                {
                    f(NotAFunctionData { function, nodes });
                }
            });
        },
        |data| {
            let related = data
                .nodes
                .iter()
                .filter(|&node| *node != data.function)
                .cloned()
                .collect();

            (data.function.clone(), related)
        },
        |w, data| {
            w.write_node(&data.function);
            w.write_string(" is not a function.");

            w.write_break();
            w.write_string("Double-check your parentheses.");
        },
    ));
}

fn with_call<'a, T>(
    ctx: &queries::QueryCtx<'_>,
    nodes: impl IntoIterator<Item = &'a NodeRef>,
    f: impl FnOnce(&CallExpressionNode) -> T,
) -> Option<T> {
    for node in nodes {
        if let Some(parent) = ctx.db.parent(node)
            && let Some(call) = parent.downcast_ref::<CallExpressionNode>()
        {
            return Some(f(call));
        }
    }

    None
}
