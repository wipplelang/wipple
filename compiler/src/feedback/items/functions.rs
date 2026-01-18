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
        |writer, data| {
            writer.write_node(&data.function);
            writer.write_string(" is missing ");

            writer.write_list("and", 0, |list| {
                for input in &data.inputs {
                    list.add(move |writer| match input {
                        Type::Node(_) => writer.write_string("an input"),
                        Type::Constructed(_) => {
                            writer.write_string("a ");
                            writer.write_type(input.clone());
                        }
                    });
                }
            });
            writer.write_string(".");

            writer.write_break();
            writer.write_string("Try adding ");
            if data.inputs.len() == 1 {
                writer.write_string("this input");
            } else {
                writer.write_string("these inputs");
            }
            writer.write_string(", or double-check your parentheses.");
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
        |writer, data| {
            writer.write_node(&data.function);
            writer.write_string(" only needs ");
            writer.write_number(data.expected, "input", "inputs");
            writer.write_string(".");

            writer.write_break();
            writer.write_string("Try removing ");

            let extra = data.actual.saturating_sub(data.expected);
            if extra == 1 {
                writer.write_string("the extra input");
            } else {
                writer.write_string(&format!("{extra} extra inputs"));
            }

            writer.write_string(" here.");
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
        |writer, data| {
            writer.write_node(&data.function);
            writer.write_string(" is not a function.");

            writer.write_break();
            writer.write_string("Double-check your parentheses.");
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
