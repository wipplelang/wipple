const variant = (index, values) => {
    values[variant] = index;
    return values;
};

variant.toString = () => "<variant>";

const fromBoolean = (value) => (value ? variant(1, []) : variant(0, []));

const fromMaybe = (value) => (value !== undefined ? variant(1, [value]) : variant(0, []));

const toMaybe = (value) => {
    switch (value[variant]) {
        case 0: {
            return undefined;
        }
        case 1: {
            return value[0];
        }
        default: {
            throw new Error("expected maybe");
        }
    }
};

const fromOrdering = (ordering) => variant(ordering + 1, []);

const isValidListIndex = (index, list, includeEnd = false) =>
    index === Math.floor(index) &&
    index >= 0 &&
    (includeEnd ? index <= list.length : index < list.length);

export default (env) => ({
    variant,

    debug: (value) => {
        env.debug?.(value);
        return value;
    },

    trace: (trace) => {
        env.trace?.(trace);
    },

    crash: (message) => {
        throw new Error(message);
    },

    prompt: (message, validate) => {
        let input = env.prompt(message);

        let validated;
        do {
            validated = toMaybe(validate(input));
            input = env.validatePrompt(validated !== undefined);
        } while (validated === undefined);

        return validated;
    },

    external: (func, input) => {
        return env[func](input);
    },

    numberToString: (number) => {
        return number.toString();
    },

    stringToNumber: (string) => {
        if (string.toLowerCase() === "nan") {
            return fromMaybe(NaN);
        } else {
            const number = parseFloat(string);
            return isNaN(number) ? fromMaybe(undefined) : fromMaybe(number);
        }
    },

    addNumbers: (left, right) => {
        return left + right;
    },

    subtractNumbers: (left, right) => {
        return left - right;
    },

    multiplyNumbers: (left, right) => {
        return left * right;
    },

    divideNumbers: (left, right) => {
        return left / right;
    },

    remainderNumbers: (left, right) => {
        return left % right;
    },

    powerNumbers: (left, right) => {
        return Math.pow(left, right);
    },

    floorNumber: (number) => {
        return Math.floor(number);
    },

    ceilingNumber: (number) => {
        return Math.ceil(number);
    },

    sqrtNumber: (number) => {
        return Math.sqrt(number);
    },

    sin: (number) => {
        return Math.sin(number);
    },

    cos: (number) => {
        return Math.cos(number);
    },

    tan: (number) => {
        return Math.tan(number);
    },

    negateNumber: (number) => {
        return -number;
    },

    stringEquality: (left, right) => {
        return fromBoolean(left === right);
    },

    numberEquality: (left, right) => {
        return fromBoolean(left === right);
    },

    stringOrdering: (left, right) => {
        if (left < right) {
            return fromOrdering(-1);
        } else if (left === right) {
            return fromOrdering(0);
        } else {
            return fromOrdering(1);
        }
    },

    numberOrdering: (left, right) => {
        if (left < right) {
            return fromOrdering(-1);
        } else if (left === right) {
            return fromOrdering(0);
        } else {
            return fromOrdering(1);
        }
    },

    makeEmptyList: () => {
        return [];
    },

    listFirst: (list) => {
        return fromMaybe(list[0]);
    },

    listLast: (list) => {
        return fromMaybe(list[list.length - 1]);
    },

    listInitial: (list) => {
        return fromMaybe(list.length > 0 ? list.slice(0, -1) : undefined);
    },

    listTail: (list) => {
        return fromMaybe(list.length > 0 ? list.slice(1) : undefined);
    },

    listNth: (list, index) => {
        return fromMaybe(isValidListIndex(index, list) ? list[index] : undefined);
    },

    listAppend: (list, element) => {
        return [...list, element];
    },

    listPrepend: (element, list) => {
        return [element, ...list];
    },

    listInsertAt: (list, index, element) => {
        return fromMaybe(
            isValidListIndex(index, list, true)
                ? [...list.slice(0, index), element, ...list.slice(index)]
                : undefined,
        );
    },

    listRemoveAt: (list, index) => {
        return fromMaybe(
            isValidListIndex(index, list)
                ? [...list.slice(0, index), ...list.slice(index + 1)]
                : undefined,
        );
    },

    listCount: (list) => {
        return list.length;
    },

    listSlice: (list, start, end) => {
        return fromMaybe(
            isValidListIndex(start, list) && isValidListIndex(end, list, true) && start <= end
                ? list.slice(start, end)
                : undefined,
        );
    },

    stringCharacters: (string) => {
        return string.split("");
    },

    randomNumber: (min, max) => {
        return Math.random() * (max - min) + min;
    },

    nan: () => {
        return NaN;
    },

    hashString: (string) => {
        // https://gist.github.com/jlevy/c246006675becc446360a798e2b2d781
        let hash = 0;
        for (const char of string) {
            hash = (hash << 5) - hash + char;
        }
        return hash >>> 0;
    },

    numberAsExternal: (number) => {
        return number;
    },

    stringAsExternal: (string) => {
        return string;
    },

    unitAsExternal: () => {
        return [];
    },

    tuple2AsExternal: (a, b) => {
        return [a, b];
    },

    tuple3AsExternal: (a, b, c) => {
        return [a, b, c];
    },

    numberFromExternal: (value) => {
        return value;
    },

    stringFromExternal: (value) => {
        return value;
    },

    tuple2FromExternal: (value) => {
        return value;
    },

    tuple3FromExternal: (value) => {
        return value;
    },
});
