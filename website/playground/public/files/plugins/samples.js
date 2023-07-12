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

export { echo };
