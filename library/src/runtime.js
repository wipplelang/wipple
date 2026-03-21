const fromMaybe = (value) => (value !== undefined ? [value, 1] : [null, 0]);

const isValidListIndex = (index, list, includeEnd = false) =>
    index === Math.floor(index) &&
    index >= 0 &&
    (includeEnd ? index <= list.length : index < list.length);

export default (env) => ({
    debug: (value) => {
        env.debug?.(value);
        return value;
    },

    trace: (trace) => {
        env.trace?.(trace);
    },

    number: (n) => {
        return n;
    },

    "number-equal-condition": (a, b) => {
        return a === b;
    },

    crash: (message) => {
        throw new Error(message);
    },

    prompt: (message, validate) => {
        let input = env.prompt(message);

        let validated;
        do {
            validated = validate(input);
            input = env.validatePrompt(validated !== undefined);
        } while (validated === undefined);

        return fromMaybe(validated);
    },

    external: (func, input) => {
        return env[func](input);
    },

    "number-to-string": (number) => {
        return number.toString();
    },

    "string-to-number": (string) => {
        if (string.toLowerCase() === "nan") {
            return fromMaybe(NaN);
        } else {
            const number = parseFloat(string);
            return fromMaybe(isNaN(number) ? undefined : number);
        }
    },

    "add-numbers": (left, right) => {
        return left + right;
    },

    "subtract-numbers": (left, right) => {
        return left - right;
    },

    "multiply-numbers": (left, right) => {
        return left * right;
    },

    "divide-numbers": (left, right) => {
        return left / right;
    },

    "remainder-numbers": (left, right) => {
        return left % right;
    },

    "power-numbers": (left, right) => {
        return Math.pow(left, right);
    },

    "floor-number": (number) => {
        return Math.floor(number);
    },

    "ceiling-number": (number) => {
        return Math.ceil(number);
    },

    "sqrt-number": (number) => {
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

    "negate-number": (number) => {
        return -number;
    },

    "string-equality": (left, right) => {
        return left === right ? 1 : 0;
    },

    "number-equality": (left, right) => {
        return left === right ? 1 : 0;
    },

    "string-ordering": (left, right) => {
        if (left < right) {
            return -1;
        } else if (left === right) {
            return 0;
        } else {
            return 1;
        }
    },

    "number-ordering": (left, right) => {
        if (left < right) {
            return -1;
        } else if (left === right) {
            return 0;
        } else {
            return 1;
        }
    },

    "make-empty-list": () => {
        return [];
    },

    "list-first": (list) => {
        return fromMaybe(list.length > 0 ? list[0] : undefined);
    },

    "list-last": (list) => {
        return fromMaybe(list.length > 0 ? list[list.length - 1] : undefined);
    },

    "list-initial": (list) => {
        return fromMaybe(list.length > 0 ? list.slice(0, -1) : undefined);
    },

    "list-tail": (list) => {
        return fromMaybe(list.length > 0 ? list.slice(1) : undefined);
    },

    "list-nth": (list, index) => {
        return fromMaybe(isValidListIndex(index, list) ? list[index] : undefined);
    },

    "list-append": (list, element) => {
        return [...list, element];
    },

    "list-prepend": (element, list) => {
        return [element, ...list];
    },

    "list-insert-at": (list, index, element) => {
        return fromMaybe(
            isValidListIndex(index, list, true)
                ? [...list.slice(0, index), element, ...list.slice(index)]
                : undefined,
        );
    },

    "list-remove-at": (list, index) => {
        return fromMaybe(
            isValidListIndex(index, list)
                ? [...list.slice(0, index), ...list.slice(index + 1)]
                : undefined,
        );
    },

    "list-count": (list) => {
        return list.length;
    },

    "list-slice": (list, start, end) => {
        return fromMaybe(
            isValidListIndex(start, list) && isValidListIndex(end, list, true) && start <= end
                ? list.slice(start, end)
                : undefined,
        );
    },

    "string-characters": (string) => {
        return string.split("");
    },

    "random-number": (min, max) => {
        return Math.random() * (max - min) + min;
    },

    nan: () => {
        return NaN;
    },

    "hash-string": (string) => {
        // https://gist.github.com/jlevy/c246006675becc446360a798e2b2d781
        let hash = 0;
        for (const char of string) {
            hash = (hash << 5) - hash + char;
        }
        return hash >>> 0;
    },

    "number-as-external": (number) => {
        return JSON.stringify(number);
    },

    "string-as-external": (string) => {
        return JSON.stringify(string);
    },

    "unit-as-external": () => {
        return JSON.stringify([]);
    },

    "list-as-external": (list) => {
        return JSON.stringify(list.map((element) => JSON.parse(element)));
    },

    "number-from-external": (value) => {
        return JSON.parse(value);
    },

    "string-from-external": (value) => {
        return JSON.parse(value);
    },

    "list-from-external": (value) => {
        return JSON.parse(value).map((element) => JSON.stringify(element));
    },
});
