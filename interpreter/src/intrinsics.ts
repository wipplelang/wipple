import { Decimal } from "decimal.js";
import type { TaskLocals, Context, TaskGroup, TypedValue } from "./index.js";

export type Intrinsic = (
    inputs: TypedValue[],
    context: Context,
    task: TaskLocals,
) => Promise<TypedValue>;

export const intrinsics: Record<string, Intrinsic> = {
    debug: async ([value]) => {
        console.error(JSON.stringify(value, null, 4));
        return value;
    },
    crash: async ([message], context) => {
        if (message.type !== "text") {
            throw context.error("expected text");
        }

        throw context.error(message.value);
    },
    display: async ([message], context) => {
        await new Promise<void>((resolve) => {
            context.io({
                type: "display",
                message: textToJs(message, context),
                completion: resolve,
            });
        });

        return unit;
    },
    prompt: async ([message, validate], context, task) => {
        let value: TypedValue | undefined;
        do {
            await new Promise<void>((resolve) => {
                context.io({
                    type: "prompt",
                    message: textToJs(message, context),
                    validate: async (input) => {
                        value = maybeToJs(
                            await context.call(validate, [jsToText(input, context)], task),
                            context,
                        );

                        const valid = value != null;

                        if (valid) {
                            resolve();
                        }

                        return valid;
                    },
                });
            });
        } while (value == null);

        return value;
    },
    choice: async ([message, options], context) => {
        const choices = listToJs(options, context).map((option) => textToJs(option, context));

        const index = await new Promise<number>((resolve) => {
            context.io({
                type: "choice",
                message: textToJs(message, context),
                choices,
                completion: resolve,
            });
        });

        return jsToNumber(new Decimal(index), context);
    },
    "runtime-message": async ([message, value], context, task) => {
        const messageValue = textToJs(message, context);
        const serializedValue = serialize(value, context, task);

        return await new Promise<TypedValue>(async (resolve) => {
            context.io({
                type: "ui",
                message: messageValue,
                value: serializedValue,
                completion: (value) => resolve(deserialize(value, context)),
            });
        });
    },
    "with-continuation": async ([callback], context) => {
        const taskLocals: TaskLocals = {};

        const task = async (resolve: (value: TypedValue) => void) => {
            await context.call(
                callback,
                [
                    jsToFunction(async (result) => {
                        resolve(result);
                        return unit;
                    }),
                ],
                taskLocals,
            );
        };

        return await new Promise(task);
    },
    "with-task-group": async ([callback], context, task) => {
        const taskGroup: TaskGroup = [];
        await context.call(callback, [jsToTaskGroup(taskGroup, context)], task);

        await Promise.all(taskGroup.map((task) => task()));

        return unit;
    },
    "begin-task-group": async ([], context) => {
        return jsToTaskGroup([], context);
    },
    "end-task-group": async ([taskGroup], context) => {
        if (taskGroup.type !== "taskGroup") {
            throw context.error("expected task group");
        }

        await Promise.all(taskGroup.value.map((task) => task()));

        return unit;
    },
    task: async ([taskGroup, callback], context) => {
        if (taskGroup.type !== "taskGroup") {
            throw context.error("expected task group");
        }

        const taskLocals: TaskLocals = {};
        const task = async () => {
            await context.do(callback, taskLocals);
        };

        taskGroup.value.push(task);

        return unit;
    },
    "task-local-key": async ([], context) => {
        return jsToTaskLocalKey(Symbol(), context);
    },
    "set-task-local": async ([keyValue, value], context, task) => {
        const key = taskLocalKeyToJs(keyValue, context);
        task[key] = value;
        return unit;
    },
    "task-local": async ([keyValue], context, task) => {
        const key = taskLocalKeyToJs(keyValue, context);
        return key in task ? jsToSome(task[key], context) : jsToNone(context);
    },
    "in-background": async ([callback], context) => {
        const taskLocals: TaskLocals = {};
        const task = async () => {
            await context.do(callback, taskLocals);
        };

        task();

        return unit;
    },
    delay: async ([duration], context) => {
        if (duration.type !== "number") {
            throw context.error("expected number");
        }

        await new Promise<void>((resolve) => {
            context.io({
                type: "sleep",
                ms: numberToJs(duration, context).toNumber(),
                completion: resolve,
            });
        });

        return unit;
    },
    "number-to-text": async ([number], context) => {
        const jsNumber = numberToJs(number, context);

        return jsToText(jsNumber.isFinite() ? jsNumber.toString() : "undefined", context);
    },
    "text-to-number": async ([text], context) => {
        const input = textToJs(text, context);
        if (input === "undefined") {
            return jsToSome(jsToNumber(new Decimal(NaN), context), context);
        }

        let number: Decimal;
        try {
            number = new Decimal(input);
        } catch {
            return jsToNone(context);
        }

        return jsToSome(jsToNumber(number, context), context);
    },
    "add-number": async ([left, right], context) => {
        return jsToNumber(numberToJs(left, context).add(numberToJs(right, context)), context);
    },
    "subtract-number": async ([left, right], context) => {
        return jsToNumber(numberToJs(left, context).sub(numberToJs(right, context)), context);
    },
    "multiply-number": async ([left, right], context) => {
        return jsToNumber(numberToJs(left, context).mul(numberToJs(right, context)), context);
    },
    "divide-number": async ([left, right], context) => {
        return jsToNumber(numberToJs(left, context).div(numberToJs(right, context)), context);
    },
    "remainder-number": async ([left, right], context) => {
        return jsToNumber(numberToJs(left, context).mod(numberToJs(right, context)), context);
    },
    "power-number": async ([left, right], context) => {
        return jsToNumber(numberToJs(left, context).pow(numberToJs(right, context)), context);
    },
    "floor-number": async ([number], context) => {
        return jsToNumber(numberToJs(number, context).floor(), context);
    },
    "ceiling-number": async ([number], context) => {
        return jsToNumber(numberToJs(number, context).ceil(), context);
    },
    "sqrt-number": async ([number], context) => {
        return jsToNumber(numberToJs(number, context).sqrt(), context);
    },
    sin: async ([number], context) => {
        return jsToNumber(numberToJs(number, context).sin(), context);
    },
    cos: async ([number], context) => {
        return jsToNumber(numberToJs(number, context).cos(), context);
    },
    tan: async ([number], context) => {
        return jsToNumber(numberToJs(number, context).tan(), context);
    },
    "negate-number": async ([number], context) => {
        return jsToNumber(numberToJs(number, context).neg(), context);
    },
    "text-equality": async ([left, right], context) => {
        if (left.type !== "text" || right.type !== "text") {
            throw context.error("expected text");
        }

        return jsToBoolean(left.value === right.value, context);
    },
    "number-equality": async ([left, right], context) => {
        const leftNumber = numberToJs(left, context);
        const rightNumber = numberToJs(right, context);

        return jsToBoolean(
            (!leftNumber.isFinite() && !rightNumber.isFinite()) || leftNumber.eq(rightNumber),
            context,
        );
    },
    "text-ordering": async ([left, right], context) => {
        if (left.type !== "text" || right.type !== "text") {
            throw context.error("expected text");
        }

        const ordering = left.value.localeCompare(right.value);

        if (ordering < 0) {
            return jsToIsLessThan(context);
        } else if (ordering > 0) {
            return jsToIsGreaterThan(context);
        } else {
            return jsToIsEqual(context);
        }
    },
    "number-ordering": async ([left, right], context) => {
        const leftNumber = numberToJs(left, context);
        const rightNumber = numberToJs(right, context);

        if (leftNumber.isNaN() && rightNumber.isNaN()) {
            return jsToIsEqual(context);
        }

        if (leftNumber.lt(rightNumber)) {
            return jsToIsLessThan(context);
        } else if (leftNumber.gt(rightNumber)) {
            return jsToIsGreaterThan(context);
        } else {
            return jsToIsEqual(context);
        }
    },
    "make-reference": async ([value], context) => {
        return jsToReference(value, context);
    },
    "get-reference": async ([reference], context) => {
        return referenceToJs(reference, context).current;
    },
    "set-reference": async ([reference, value], context) => {
        referenceToJs(reference, context).current = value;
        return unit;
    },
    "make-empty-list": async ([], context) => {
        return jsToList([], context);
    },
    "list-first": async ([list], context) => {
        const listValue = listToJs(list, context);
        return listValue.length > 0 ? jsToSome(listValue[0], context) : jsToNone(context);
    },
    "list-last": async ([list], context) => {
        const listValue = listToJs(list, context);
        return listValue.length > 0
            ? jsToSome(listValue[listValue.length - 1], context)
            : jsToNone(context);
    },
    "list-initial": async ([list], context) => {
        const listValue = listToJs(list, context);
        return listValue.length > 0
            ? jsToSome(jsToList(listValue.slice(0, listValue.length - 1), context), context)
            : jsToNone(context);
    },
    "list-tail": async ([list], context) => {
        const listValue = listToJs(list, context);
        return listValue.length > 0
            ? jsToSome(jsToList(listValue.slice(1), context), context)
            : jsToNone(context);
    },
    "list-nth": async ([list, position], context) => {
        const listValue = listToJs(list, context);
        const positionValue = numberToJs(position, context);

        if (!positionValue.isInt()) {
            throw context.error("list position must be a whole number");
        }

        const index = positionValue.toNumber();

        return index >= 0 && index < listValue.length
            ? jsToSome(listValue[index], context)
            : jsToNone(context);
    },
    "list-append": async ([list, element], context) => {
        const listValue = listToJs(list, context);
        return jsToList([...listValue, element], context);
    },
    "list-prepend": async ([list, element], context) => {
        const listValue = listToJs(list, context);
        return jsToList([element, ...listValue], context);
    },
    "list-insert-at": async ([list, position, element], context) => {
        const listValue = listToJs(list, context);
        const positionValue = numberToJs(position, context);

        if (!positionValue.isInt()) {
            throw context.error("list position must be a whole number");
        }

        const index = positionValue.toNumber();

        return index >= 0 && index <= listValue.length
            ? jsToSome(
                  jsToList(
                      [...listValue.slice(0, index), element, ...listValue.slice(index)],
                      context,
                  ),
                  context,
              )
            : jsToNone(context);
    },
    "list-remove-at": async ([list, position], context) => {
        const listValue = listToJs(list, context);
        const positionValue = numberToJs(position, context);

        if (!positionValue.isInt()) {
            throw context.error("list position must be a whole number");
        }

        const index = positionValue.toNumber();

        return index >= 0 && index <= listValue.length
            ? jsToSome(
                  jsToList([...listValue.slice(0, index), ...listValue.slice(index + 1)], context),
                  context,
              )
            : jsToNone(context);
    },
    "list-count": async ([list], context) => {
        return jsToNumber(new Decimal(listToJs(list, context).length), context);
    },
    "list-slice": async ([list, start, end], context) => {
        const listValue = listToJs(list, context);
        const startValue = numberToJs(start, context);
        const endValue = numberToJs(end, context);

        if (!startValue.isInt()) {
            throw context.error("list start position must be a whole number");
        }

        if (!endValue.isInt()) {
            throw context.error("list end position must be a whole number");
        }

        const startIndex = startValue.toNumber();
        const endIndex = endValue.toNumber();

        return jsToList(listValue.slice(startIndex, endIndex), context);
    },
    "text-characters": async ([text], context) => {
        const textValue = textToJs(text, context);

        return jsToList(
            [...textValue].map((character) => jsToText(character, context)),
            context,
        );
    },
    "random-number": async ([min, max], context) => {
        const minValue = numberToJs(min, context);
        const maxValue = numberToJs(max, context);

        if (minValue.gt(maxValue)) {
            throw context.error("min must be less than or equal to max");
        }

        const sd = Math.max(minValue.sd(), maxValue.sd()) - 1;
        const random = minValue.add(maxValue.sub(minValue).mul(Decimal.random()));

        return jsToNumber(sd === 0 ? random.floor() : random.toSignificantDigits(sd), context);
    },
    "undefined-number": async ([], context) => {
        return jsToNumber(new Decimal(NaN), context);
    },
    "make-hasher": async ([], context) => {
        return jsToHasher(context);
    },
    "hash-number": async ([hasher, number], context) => {
        const _hasherValue = hasherToJs(hasher, context);
        const numberValue = numberToJs(number, context);

        const hashValue = hash(numberValue.toNumber().toString());

        return jsToNumber(new Decimal(hashValue), context);
    },
    "hash-text": async ([hasher, text], context) => {
        const _hasherValue = hasherToJs(hasher, context);
        const textValue = textToJs(text, context);

        const hashValue = hash(textValue);

        return jsToNumber(new Decimal(hashValue), context);
    },
};

const unit: TypedValue = {
    type: "tuple",
    values: [],
};

const jsToText = (string: string, _context: Context): TypedValue => ({
    type: "text",
    value: string,
});

const textToJs = (value: TypedValue, context: Context): string => {
    if (value.type !== "text") {
        throw context.error("expected text");
    }

    return value.value;
};

const jsToNumber = (number: Decimal, _context: Context): TypedValue => ({
    type: "number",
    value: number.isFinite() ? number : undefined,
});

const numberToJs = (value: TypedValue, context: Context): Decimal => {
    if (value.type !== "number") {
        throw context.error("expected number");
    }

    return value.value != null ? value.value : new Decimal(NaN);
};

const jsToBoolean = (boolean: boolean, _context: Context): TypedValue => ({
    type: "variant",
    variant: boolean ? 1 : 0,
    values: [],
});

const booleanToJs = (value: TypedValue, context: Context): boolean => {
    if (value.type !== "variant") {
        throw context.error("expected variant");
    }

    switch (value.variant) {
        case 1:
            return true;
        case 0:
            return false;
        default:
            throw context.error("expected boolean");
    }
};

const jsToNone = (_context: Context): TypedValue => ({
    type: "variant",
    variant: 0,
    values: [],
});

const jsToSome = (value: TypedValue, _context: Context): TypedValue => ({
    type: "variant",
    variant: 1,
    values: [value],
});

const jsToIsLessThan = (_context: Context): TypedValue => ({
    type: "variant",
    variant: 0,
    values: [],
});

const jsToIsEqual = (_context: Context): TypedValue => ({
    type: "variant",
    variant: 1,
    values: [],
});

const jsToIsGreaterThan = (_context: Context): TypedValue => ({
    type: "variant",
    variant: 2,
    values: [],
});

const maybeToJs = (value: TypedValue, context: Context): TypedValue | undefined => {
    if (value.type !== "variant") {
        throw context.error("expected variant");
    }

    switch (value.variant) {
        case 1:
            return value.values[0];
        case 0:
            return undefined;
        default:
            throw context.error("expected maybe");
    }
};

const jsToList = (values: TypedValue[], _context: Context): TypedValue => ({
    type: "list",
    values,
});

const listToJs = (value: TypedValue, context: Context): TypedValue[] => {
    if (value.type !== "list") {
        throw context.error("expected list");
    }

    return value.values;
};

const jsToTuple = (values: TypedValue[], _context: Context): TypedValue => ({
    type: "tuple",
    values,
});

const tupleToJs = (value: TypedValue, context: Context): TypedValue[] => {
    if (value.type !== "tuple") {
        throw context.error("expected tuple");
    }

    return value.values;
};

const jsToFunction = (func: (...inputs: TypedValue[]) => Promise<TypedValue>): TypedValue => ({
    type: "nativeFunction",
    value: func,
});

const jsToTaskGroup = (taskGroup: TaskGroup, _context: Context): TypedValue => ({
    type: "taskGroup",
    value: taskGroup,
});

const jsToTaskLocalKey = (key: symbol, _context: Context): TypedValue => ({
    type: "taskLocalKey",
    value: key,
});

const taskLocalKeyToJs = (value: TypedValue, context: Context): symbol => {
    if (value.type !== "taskLocalKey") {
        throw context.error("expected task local key");
    }

    return value.value;
};

const jsToReference = (value: TypedValue, _context: Context): TypedValue => ({
    type: "reference",
    value: { current: value },
});

const referenceToJs = (value: TypedValue, context: Context): { current: TypedValue } => {
    if (value.type !== "reference") {
        throw context.error("expected reference");
    }

    return value.value;
};

const jsToHasher = (_context: Context): TypedValue => ({
    type: "hasher",
});

const hasherToJs = (value: TypedValue, context: Context): {} => {
    if (value.type !== "hasher") {
        throw context.error("expected hasher");
    }

    return value;
};

interface FunctionHandle {
    func: (inputs: TypedValue[]) => Promise<TypedValue>;
    context: Context;
    task: any;
}

const functions: FunctionHandle[] = [];

export const callFunction = async (func: any, inputs: any[]) => {
    if (!("$wippleFunction" in func)) {
        throw new Error("expected function");
    }

    const index = func.$wippleFunction;
    const handle = functions[index];

    const output = await handle.func(
        inputs.map((input, index) => deserialize(input, handle.context)),
    );

    return serialize(output, handle.context, handle.task);
};

const serialize = (value: TypedValue, context: Context, task: any): any => {
    switch (value.type) {
        case "number":
            return numberToJs(value, context).toNumber();
        case "text":
            return textToJs(value, context);
        case "function": {
            const index = functions.length;
            functions.push({
                func: (inputs) => context.call(value, inputs, task),
                context,
                task,
            });

            return { $wippleFunction: index };
        }
        case "list":
            return listToJs(value, context).map((value) => serialize(value, context, task));
        case "tuple":
            return tupleToJs(value, context).map((value) => serialize(value, context, task));
        default:
            throw new Error("cannot serialize value");
    }
};

const deserialize = (value: any, context: Context): TypedValue => {
    if (value == null) {
        return unit;
    } else if (typeof value === "number") {
        return jsToNumber(new Decimal(value), context);
    } else if (typeof value === "string") {
        return jsToText(value, context);
    } else if (typeof value === "function") {
        return jsToFunction(async (...inputs) => {
            return deserialize(await value(...inputs), context);
        });
    } else if (typeof value === "boolean") {
        return jsToBoolean(value, context);
    } else if (Array.isArray(value)) {
        return jsToTuple(
            value.map((value) => deserialize(value, context)),
            context,
        );
    } else if (typeof value === "object" && "$wippleFunction" in value) {
        const index = value.$wippleFunction;
        const handle = functions[index];
        return jsToFunction(async (...inputs) => {
            return deserialize(await handle.func(inputs), context);
        });
    } else {
        throw new Error("cannot deserialize value");
    }
};

// https://stackoverflow.com/a/8831937/5569234
const hash = (str: string) => {
    let hash = 0;
    for (let i = 0, len = str.length; i < len; i++) {
        let chr = str.charCodeAt(i);
        hash = (hash << 5) - hash + chr;
        hash |= 0; // Convert to 32bit integer
    }
    return hash;
};
