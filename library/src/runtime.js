// @ts-nocheck

export default (env) => {
    let memory;

    return {
        init: (mem) => {
            memory = mem;
        },

        debug: (value) => {
            env.debug?.(value);
            return value;
        },

        trace: (trace) => {
            env.trace?.(trace);
        },

        bridge: (value) => {
            return value;
        },

        "bridge-unit": () => {
            return undefined;
        },

        "make-string": (ptr, len) => {
            return new TextDecoder("utf8").decode(new Uint8Array(memory, ptr, len));
        },

        "string-count": (string) => {
            return string.length;
        },

        concat: (a, b) => {
            return a + b;
        },

        prompt: (message) => {
            return env.prompt(message);
        },

        "prompt-result": (success) => {
            env.promptResult(success === 1);
        },

        external: (func, input) => {
            return env[func](input);
        },

        "number-to-string": (number) => {
            return number.toString();
        },

        "string-to-number": (string) => {
            return parseFloat(string);
        },

        rem: (left, right) => {
            return left % right;
        },

        pow: (left, right) => {
            return Math.pow(left, right);
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

        "list-count": (list) => {
            return list.length;
        },

        "list-first": (list) => {
            return list[0];
        },

        "list-last": (list) => {
            return list[list.length - 1];
        },

        "list-initial": (list) => {
            return list.slice(0, -1);
        },

        "list-tail": (list) => {
            return list.slice(1);
        },

        "list-nth": (list, index) => {
            return list[index];
        },

        "list-append": (list, element) => {
            return [...list, element];
        },

        "list-prepend": (element, list) => {
            return [element, ...list];
        },

        "list-insert-at": (list, index, element) => {
            return [...list.slice(0, index), element, ...list.slice(index)];
        },

        "list-remove-at": (list, index) => {
            return [...list.slice(0, index), ...list.slice(index + 1)];
        },

        "list-count": (list) => {
            return list.length;
        },

        "list-slice": (list, start, end) => {
            return list.slice(start, end);
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

        "is-nan": (number) => {
            return isNaN(number) ? 1 : 0;
        },

        "hash-string": (string) => {
            // https://gist.github.com/jlevy/c246006675becc446360a798e2b2d781
            let hash = 0;
            for (const char of string) {
                hash = (hash << 5) - hash + char;
            }
            return hash >>> 0;
        },
    };
};
