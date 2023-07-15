const echo = (input, _api) => {
    if (!("Expression" in input)) {
        throw new Error("`echo` plugin may only be used in expression position");
    }

    const { inputs } = input.Expression;

    if (inputs.length !== 1) {
        throw new Error("`echo` expects 1 input");
    }

    return { expr: inputs[0] };
};

const type = (input, _api) => {
    if (!("Expression" in input)) {
        throw new Error("`type` plugin may only be used in expression position");
    }

    const { id, span, inputs } = input.Expression;

    if (inputs.length !== 1) {
        throw new Error("`type` expects 1 input");
    }

    const expr = inputs[0];

    return {
        expr: {
            id,
            span,
            kind: {
                Text: `\`${JSON.stringify(expr.ty)}\``,
            },
        },
    };
};

export { echo, type };
