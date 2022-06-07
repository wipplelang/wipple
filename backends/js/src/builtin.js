if (!globalThis.$wippleExternals) {
    globalThis.$wippleExternals = {};
}

globalThis.$wippleExternals.builtin = {
    crash: (msg) => {
        console.error(`fatal error: ${msg}`);
        process.exit(1);
    },
    show: console.log,
    format: (text, inputs) => {
        if (text.length === 0) {
            return "";
        } else {
            const text = text.split("_");
            const last = text.pop();

            return (
                text.map((part, index) => {
                    const value = inputs[index];
                    return part + value;
                }) + last
            );
        }
    },
    "number-to-text": (n) => n.toString(),
    add: (a, b) => a + b,
    subtract: (a, b) => a - b,
    multiply: (a, b) => a * b,
    divide: (a, b) => {
        if (b === 0) {
            $wippleExternals.builtin.crash("division by zero is undefined");
        }

        return a / b;
    },
    power: (a, b) => {
        if (a === 0 && b === 0) {
            $wippleExternals.builtin.crash("raising zero to the power of zero is undefined");
        }

        return Math.pow(a, b);
    },
    "number-equality": (a, b) => ({
        $wippleVariant: a === b ? 1 : 0,
        $wippleValues: [],
    }),
    "text-equality": (a, b) => ({
        $wippleVariant: a === b ? 1 : 0,
        $wippleValues: [],
    }),
    "number-less-than": (a, b) => ({
        $wippleVariant: a < b ? 1 : 0,
        $wippleValues: [],
    }),
    "number-greater-than": (a, b) => ({
        $wippleVariant: a > b ? 1 : 0,
        $wippleValues: [],
    }),
    "make-mutable": (x) => ({ $wippleMutable: x }),
    "get-mutable": (m) => m.$wippleMutable,
    "set-mutable": (m, x) => {
        m.$wippleMutable = x;
    },
};
