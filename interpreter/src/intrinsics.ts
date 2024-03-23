import { Decimal } from "decimal.js";
import type { Context, TaskGroup, TypeDescriptor, TypedValue } from "./index.js";

export type Intrinsic = (
    inputs: TypedValue[],
    expectedTypeDescriptor: TypeDescriptor,
    context: Context,
) => Promise<TypedValue>;

export const intrinsics: Record<string, Intrinsic> = {
    debug: async ([value], _expectedTypeDescriptor, _context) => {
        console.error(value);
        return value;
    },
    crash: async ([message], _expectedTypeDescriptor, context) => {
        if (message.type !== "text") {
            throw context.error("expected text");
        }

        throw context.error(message.value);
    },
    display: async ([message], _expectedTypeDescriptor, context) => {
        await new Promise<void>((resolve) => {
            context.io({
                type: "display",
                message: textToJs(message, context),
                completion: resolve,
            });
        });

        return unit;
    },
    prompt: async ([message, validate], expectedTypeDescriptor, context) => {
        let value: TypedValue | undefined;
        do {
            await new Promise<void>((resolve) => {
                context.io({
                    type: "prompt",
                    message: textToJs(message, context),
                    validate: async (input) => {
                        value = maybeToJs(
                            await context.call(validate, [
                                jsToText(expectedTypeDescriptor, input, context),
                            ]),
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
    choice: async ([message, options], expectedTypeDescriptor, context) => {
        const choices = listToJs(options, context).map((option) => textToJs(option, context));

        const index = await new Promise<number>((resolve) => {
            context.io({
                type: "choice",
                message: textToJs(message, context),
                choices,
                completion: resolve,
            });
        });

        return jsToNumber(expectedTypeDescriptor, new Decimal(index), context);
    },
    "runtime-message": async ([message, value], expectedTypeDescriptor, context) => {
        const messageValue = textToJs(message, context);
        const serializedValue = serialize(value, context);

        return await new Promise<TypedValue>(async (resolve) => {
            context.io({
                type: "ui",
                message: messageValue,
                value: serializedValue,
                completion: (value) => resolve(deserialize(value, expectedTypeDescriptor, context)),
            });
        });
    },
    "with-continuation": async ([callback], _expectedTypeDescriptor, context) => {
        if (callback.typeDescriptor.type !== "function") {
            throw context.error("expected function");
        }

        if (callback.typeDescriptor.value[0].length !== 1) {
            throw context.error("expected function with one input");
        }

        const callbackTypeDescriptor = callback.typeDescriptor.value[0][0];

        if (callbackTypeDescriptor.type !== "function") {
            throw context.error("expected function");
        }

        const typeDescriptor: TypeDescriptor = {
            type: "function",
            value: [callbackTypeDescriptor.value[0], unit.typeDescriptor],
        };

        return await new Promise<TypedValue>(async (resolve) => {
            await context.call(callback, [
                jsToFunction(typeDescriptor, async (result) => {
                    resolve(result);
                    return unit;
                }),
            ]);
        });
    },
    "with-task-group": async ([callback], _expectedTypeDescriptor, context) => {
        if (callback.typeDescriptor.type !== "function") {
            throw context.error("expected function");
        }

        if (callback.typeDescriptor.value[0].length !== 1) {
            throw context.error("expected function with one input");
        }

        const taskGroupTypeDescriptor = callback.typeDescriptor.value[0][0];

        const taskGroup: TaskGroup = [];
        await context.call(callback, [jsToTaskGroup(taskGroupTypeDescriptor, taskGroup, context)]);

        await Promise.all(taskGroup.map((task) => task()));

        return unit;
    },
    task: async ([taskGroup, callback], _expectedTypeDescriptor, context) => {
        if (taskGroup.type !== "taskGroup") {
            throw context.error("expected task group");
        }

        taskGroup.value.push(async () => {
            await context.do(callback);
        });

        return unit;
    },
    "in-background": async ([callback], _expectedTypeDescriptor, context) => {
        (async () => {
            await context.do(callback);
        })();

        return unit;
    },
    delay: async ([duration], _expectedTypeDescriptor, context) => {
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
    "number-to-text": async ([number], expectedTypeDescriptor, context) => {
        return jsToText(expectedTypeDescriptor, numberToJs(number, context).toString(), context);
    },
    "text-to-number": async ([text], expectedTypeDescriptor, context) => {
        if (expectedTypeDescriptor.type !== "named") {
            throw context.error("expected result to be a named type");
        }

        const numberTypeDescriptor = expectedTypeDescriptor.value[1][0];

        const input = textToJs(text, context);
        if (input === "undefined") {
            return jsToSome(
                expectedTypeDescriptor,
                jsToNumber(numberTypeDescriptor, new Decimal(NaN), context),
                context,
            );
        }

        let number: Decimal;
        try {
            number = new Decimal(input);
        } catch {
            return jsToNone(expectedTypeDescriptor, context);
        }

        return jsToSome(
            expectedTypeDescriptor,
            jsToNumber(numberTypeDescriptor, number, context),
            context,
        );
    },
    "add-number": async ([left, right], expectedTypeDescriptor, context) => {
        return jsToNumber(
            expectedTypeDescriptor,
            numberToJs(left, context).add(numberToJs(right, context)),
            context,
        );
    },
    "subtract-number": async ([left, right], expectedTypeDescriptor, context) => {
        return jsToNumber(
            expectedTypeDescriptor,
            numberToJs(left, context).sub(numberToJs(right, context)),
            context,
        );
    },
    "multiply-number": async ([left, right], expectedTypeDescriptor, context) => {
        return jsToNumber(
            expectedTypeDescriptor,
            numberToJs(left, context).mul(numberToJs(right, context)),
            context,
        );
    },
    "divide-number": async ([left, right], expectedTypeDescriptor, context) => {
        return jsToNumber(
            expectedTypeDescriptor,
            numberToJs(left, context).div(numberToJs(right, context)),
            context,
        );
    },
    "remainder-number": async ([left, right], expectedTypeDescriptor, context) => {
        return jsToNumber(
            expectedTypeDescriptor,
            numberToJs(left, context).mod(numberToJs(right, context)),
            context,
        );
    },
    "power-number": async ([left, right], expectedTypeDescriptor, context) => {
        return jsToNumber(
            expectedTypeDescriptor,
            numberToJs(left, context).pow(numberToJs(right, context)),
            context,
        );
    },
    "floor-number": async ([number], expectedTypeDescriptor, context) => {
        return jsToNumber(expectedTypeDescriptor, numberToJs(number, context).floor(), context);
    },
    "ceiling-number": async ([number], expectedTypeDescriptor, context) => {
        return jsToNumber(expectedTypeDescriptor, numberToJs(number, context).ceil(), context);
    },
    "sqrt-number": async ([number], expectedTypeDescriptor, context) => {
        return jsToNumber(expectedTypeDescriptor, numberToJs(number, context).sqrt(), context);
    },
    sin: async ([number], expectedTypeDescriptor, context) => {
        return jsToNumber(expectedTypeDescriptor, numberToJs(number, context).sin(), context);
    },
    cos: async ([number], expectedTypeDescriptor, context) => {
        return jsToNumber(expectedTypeDescriptor, numberToJs(number, context).cos(), context);
    },
    tan: async ([number], expectedTypeDescriptor, context) => {
        return jsToNumber(expectedTypeDescriptor, numberToJs(number, context).tan(), context);
    },
    "negate-number": async ([number], expectedTypeDescriptor, context) => {
        return jsToNumber(expectedTypeDescriptor, numberToJs(number, context).neg(), context);
    },
    "text-equality": async ([left, right], expectedTypeDescriptor, context) => {
        if (left.type !== "text" || right.type !== "text") {
            throw context.error("expected text");
        }

        return jsToBoolean(expectedTypeDescriptor, left.value === right.value, context);
    },
    "number-equality": async ([left, right], expectedTypeDescriptor, context) => {
        const leftNumber = numberToJs(left, context);
        const rightNumber = numberToJs(right, context);

        return jsToBoolean(
            expectedTypeDescriptor,
            (leftNumber.isNaN() && rightNumber.isNaN()) || leftNumber.eq(rightNumber),
            context,
        );
    },
    "text-ordering": async ([left, right], expectedTypeDescriptor, context) => {
        if (left.type !== "text" || right.type !== "text") {
            throw context.error("expected text");
        }

        const ordering = left.value.localeCompare(right.value);

        if (ordering < 0) {
            return jsToIsLessThan(expectedTypeDescriptor, context);
        } else if (ordering > 0) {
            return jsToIsGreaterThan(expectedTypeDescriptor, context);
        } else {
            return jsToIsEqual(expectedTypeDescriptor, context);
        }
    },
    "number-ordering": async ([left, right], expectedTypeDescriptor, context) => {
        const leftNumber = numberToJs(left, context);
        const rightNumber = numberToJs(right, context);

        if (leftNumber.isNaN() && rightNumber.isNaN()) {
            return jsToIsEqual(expectedTypeDescriptor, context);
        }

        if (leftNumber.lt(rightNumber)) {
            return jsToIsLessThan(expectedTypeDescriptor, context);
        } else if (leftNumber.gt(rightNumber)) {
            return jsToIsGreaterThan(expectedTypeDescriptor, context);
        } else {
            return jsToIsEqual(expectedTypeDescriptor, context);
        }
    },
    "make-reference": async ([value], expectedTypeDescriptor, context) => {
        return jsToReference(expectedTypeDescriptor, value, context);
    },
    "get-reference": async ([reference], _expectedTypeDescriptor, context) => {
        return referenceToJs(reference, context).current;
    },
    "set-reference": async ([reference, value], _expectedTypeDescriptor, context) => {
        referenceToJs(reference, context).current = value;
        return unit;
    },
    "make-empty-list": async ([], expectedTypeDescriptor, context) => {
        return jsToList(expectedTypeDescriptor, [], context);
    },
    "list-first": async ([list], expectedTypeDescriptor, context) => {
        const listValue = listToJs(list, context);
        return listValue.length > 0
            ? jsToSome(expectedTypeDescriptor, listValue[0], context)
            : jsToNone(expectedTypeDescriptor, context);
    },
    "list-last": async ([list], expectedTypeDescriptor, context) => {
        const listValue = listToJs(list, context);
        return listValue.length > 0
            ? jsToSome(expectedTypeDescriptor, listValue[listValue.length - 1], context)
            : jsToNone(expectedTypeDescriptor, context);
    },
    "list-initial": async ([list], expectedTypeDescriptor, context) => {
        const listValue = listToJs(list, context);
        return jsToList(expectedTypeDescriptor, listValue.slice(0, listValue.length - 1), context);
    },
    "list-tail": async ([list], expectedTypeDescriptor, context) => {
        const listValue = listToJs(list, context);
        return jsToList(expectedTypeDescriptor, listValue.slice(1), context);
    },
    "list-nth": async ([list, position], expectedTypeDescriptor, context) => {
        const listValue = listToJs(list, context);
        const positionValue = numberToJs(position, context);

        if (!positionValue.isInt()) {
            throw context.error("list position must be a whole number");
        }

        const index = positionValue.toNumber();

        return index >= 0 && index < listValue.length
            ? jsToSome(expectedTypeDescriptor, listValue[index], context)
            : jsToNone(expectedTypeDescriptor, context);
    },
    "list-append": async ([list, element], expectedTypeDescriptor, context) => {
        const listValue = listToJs(list, context);
        return jsToList(expectedTypeDescriptor, [...listValue, element], context);
    },
    "list-prepend": async ([list, element], expectedTypeDescriptor, context) => {
        const listValue = listToJs(list, context);
        return jsToList(expectedTypeDescriptor, [element, ...listValue], context);
    },
    "list-insert-at": async ([list, position, element], expectedTypeDescriptor, context) => {
        if (expectedTypeDescriptor.type !== "named") {
            throw context.error("expected result to be a named type");
        }

        const listTypeDescriptor = expectedTypeDescriptor.value[1][0];

        const listValue = listToJs(list, context);
        const positionValue = numberToJs(position, context);

        if (!positionValue.isInt()) {
            throw context.error("list position must be a whole number");
        }

        const index = positionValue.toNumber();

        return index >= 0 && index <= listValue.length
            ? jsToSome(
                  expectedTypeDescriptor,
                  jsToList(
                      listTypeDescriptor,
                      [...listValue.slice(0, index), element, ...listValue.slice(index)],
                      context,
                  ),
                  context,
              )
            : jsToNone(expectedTypeDescriptor, context);
    },
    "list-remove-at": async ([list, position], expectedTypeDescriptor, context) => {
        if (expectedTypeDescriptor.type !== "named") {
            throw context.error("expected result to be a named type");
        }

        const listTypeDescriptor = expectedTypeDescriptor.value[1][0];

        const listValue = listToJs(list, context);
        const positionValue = numberToJs(position, context);

        if (!positionValue.isInt()) {
            throw context.error("list position must be a whole number");
        }

        const index = positionValue.toNumber();

        return index >= 0 && index <= listValue.length
            ? jsToSome(
                  expectedTypeDescriptor,
                  jsToList(
                      listTypeDescriptor,
                      [...listValue.slice(0, index), ...listValue.slice(index + 1)],
                      context,
                  ),
                  context,
              )
            : jsToNone(expectedTypeDescriptor, context);
    },
    "list-count": async ([list], expectedTypeDescriptor, context) => {
        return jsToNumber(
            expectedTypeDescriptor,
            new Decimal(listToJs(list, context).length),
            context,
        );
    },
    "list-slice": async ([list, start, end], expectedTypeDescriptor, context) => {
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

        return jsToList(expectedTypeDescriptor, listValue.slice(startIndex, endIndex), context);
    },
    "text-characters": async ([text], expectedTypeDescriptor, context) => {
        const textValue = textToJs(text, context);

        if (expectedTypeDescriptor.type !== "named") {
            throw context.error("expected result to be a named type");
        }

        const textTypeDescriptor = expectedTypeDescriptor.value[1][0];

        return jsToList(
            expectedTypeDescriptor,
            [...textValue].map((character) => jsToText(textTypeDescriptor, character, context)),
            context,
        );
    },
    "random-number": async ([min, max], expectedTypeDescriptor, context) => {
        const minValue = numberToJs(min, context);
        const maxValue = numberToJs(max, context);

        if (minValue.gt(maxValue)) {
            throw context.error("min must be less than or equal to max");
        }

        const sd = Math.max(minValue.sd(), maxValue.sd()) - 1;
        const random = minValue.add(maxValue.sub(minValue).mul(Decimal.random()));

        return jsToNumber(
            expectedTypeDescriptor,
            sd === 0 ? random.floor() : random.toSignificantDigits(sd),
            context,
        );
    },
    "undefined-number": async ([], expectedTypeDescriptor, context) => {
        return jsToNumber(expectedTypeDescriptor, new Decimal(NaN), context);
    },
    "make-hasher": async ([], expectedTypeDescriptor, context) => {
        return jsToHasher(expectedTypeDescriptor, context);
    },
    "hash-number": async ([hasher, number], expectedTypeDescriptor, context) => {
        const _hasherValue = hasherToJs(hasher, context);
        const numberValue = numberToJs(number, context);

        const hashValue = hash(numberValue.toNumber().toString());

        return jsToNumber(expectedTypeDescriptor, new Decimal(hashValue), context);
    },
    "hash-text": async ([hasher, text], expectedTypeDescriptor, context) => {
        const _hasherValue = hasherToJs(hasher, context);
        const textValue = textToJs(text, context);

        const hashValue = hash(textValue);

        return jsToNumber(expectedTypeDescriptor, new Decimal(hashValue), context);
    },
};

const unit: TypedValue = {
    type: "tuple",
    typeDescriptor: {
        type: "tuple",
        value: [],
    },
    values: [],
};

const jsToText = (
    typeDescriptor: TypeDescriptor,
    string: string,
    _context: Context,
): TypedValue => ({
    type: "text",
    typeDescriptor,
    value: string,
});

const textToJs = (value: TypedValue, context: Context): string => {
    if (value.type !== "text") {
        throw context.error("expected text");
    }

    return value.value;
};

const jsToNumber = (
    typeDescriptor: TypeDescriptor,
    number: Decimal,
    _context: Context,
): TypedValue => ({
    type: "number",
    typeDescriptor,
    value: number,
});

const numberToJs = (value: TypedValue, context: Context): Decimal => {
    if (value.type !== "number") {
        throw context.error("expected number");
    }

    return value.value;
};

const jsToBoolean = (
    typeDescriptor: TypeDescriptor,
    boolean: boolean,
    context: Context,
): TypedValue => ({
    typeDescriptor,
    type: "variant",
    variant: boolean
        ? context.executable.intrinsicVariants.true
        : context.executable.intrinsicVariants.false,
    values: [],
});

const booleanToJs = (value: TypedValue, context: Context): boolean => {
    if (value.type !== "variant") {
        throw context.error("expected variant");
    }

    switch (value.variant) {
        case context.executable.intrinsicVariants.true:
            return true;
        case context.executable.intrinsicVariants.false:
            return false;
        default:
            throw context.error("expected boolean");
    }
};

const jsToSome = (
    typeDescriptor: TypeDescriptor,
    value: TypedValue,
    context: Context,
): TypedValue => ({
    typeDescriptor,
    type: "variant",
    variant: context.executable.intrinsicVariants.some,
    values: [value],
});

const jsToNone = (typeDescriptor: TypeDescriptor, context: Context): TypedValue => ({
    typeDescriptor,
    type: "variant",
    variant: context.executable.intrinsicVariants.none,
    values: [],
});

const jsToIsLessThan = (typeDescriptor: TypeDescriptor, context: Context): TypedValue => ({
    typeDescriptor,
    type: "variant",
    variant: context.executable.intrinsicVariants["is-less-than"],
    values: [],
});

const jsToIsEqual = (typeDescriptor: TypeDescriptor, context: Context): TypedValue => ({
    typeDescriptor,
    type: "variant",
    variant: context.executable.intrinsicVariants["is-equal-to"],
    values: [],
});

const jsToIsGreaterThan = (typeDescriptor: TypeDescriptor, context: Context): TypedValue => ({
    typeDescriptor,
    type: "variant",
    variant: context.executable.intrinsicVariants["is-greater-than"],
    values: [],
});

const maybeToJs = (value: TypedValue, context: Context): TypedValue | undefined => {
    if (value.type !== "variant") {
        throw context.error("expected variant");
    }

    switch (value.variant) {
        case context.executable.intrinsicVariants.some:
            return value.values[0];
        case context.executable.intrinsicVariants.none:
            return undefined;
        default:
            throw context.error("expected maybe");
    }
};

const jsToList = (
    typeDescriptor: TypeDescriptor,
    values: TypedValue[],
    _context: Context,
): TypedValue => ({
    typeDescriptor,
    type: "list",
    values,
});

const listToJs = (value: TypedValue, context: Context): TypedValue[] => {
    if (value.type !== "list") {
        throw context.error("expected list");
    }

    return value.values;
};

const jsToTuple = (
    typeDescriptor: TypeDescriptor,
    values: TypedValue[],
    _context: Context,
): TypedValue => ({
    typeDescriptor,
    type: "tuple",
    values,
});

const tupleToJs = (value: TypedValue, context: Context): TypedValue[] => {
    if (value.type !== "tuple") {
        throw context.error("expected tuple");
    }

    return value.values;
};

const jsToFunction = (
    typeDescriptor: TypeDescriptor,
    func: (...inputs: TypedValue[]) => Promise<TypedValue>,
): TypedValue => ({
    type: "nativeFunction",
    typeDescriptor,
    value: func,
});

const jsToTaskGroup = (
    typeDescriptor: TypeDescriptor,
    taskGroup: TaskGroup,
    _context: Context,
): TypedValue => ({
    type: "taskGroup",
    typeDescriptor,
    value: taskGroup,
});

const jsToReference = (
    typeDescriptor: TypeDescriptor,
    value: TypedValue,
    _context: Context,
): TypedValue => ({
    type: "reference",
    typeDescriptor,
    value: { current: value },
});

const referenceToJs = (value: TypedValue, context: Context): { current: TypedValue } => {
    if (value.type !== "reference") {
        throw context.error("expected reference");
    }

    return value.value;
};

const jsToHasher = (typeDescriptor: TypeDescriptor, _context: Context): TypedValue => ({
    type: "hasher",
    typeDescriptor,
});

const hasherToJs = (value: TypedValue, context: Context): {} => {
    if (value.type !== "hasher") {
        throw context.error("expected hasher");
    }

    return value;
};

const randomInteger = () => Math.floor(Math.random() * Number.MAX_SAFE_INTEGER);

const functions: ((inputs: TypedValue[]) => Promise<TypedValue>)[] = [];

const serialize = (value: TypedValue, context: Context): any => {
    switch (value.type) {
        case "number":
            return numberToJs(value, context).toNumber();
        case "text":
            return textToJs(value, context);
        case "function": {
            const index = functions.length;
            functions.push((inputs) => context.call(value, inputs));
            return { $wippleFunction: index };
        }
        case "list":
            return listToJs(value, context).map((value) => serialize(value, context));
        case "tuple":
            return tupleToJs(value, context).map((value) => serialize(value, context));
        default:
            throw new Error("cannot serialize value");
    }
};

const deserialize = (value: any, typeDescriptor: TypeDescriptor, context: Context): TypedValue => {
    if (value == null) {
        return unit;
    } else if (typeof value === "number") {
        return jsToNumber(typeDescriptor, new Decimal(value), context);
    } else if (typeof value === "string") {
        return jsToText(typeDescriptor, value, context);
    } else if (typeof value === "function") {
        return jsToFunction(typeDescriptor, async (...inputs) => {
            return deserialize(await value(...inputs), typeDescriptor, context);
        });
    } else if (typeof value === "boolean") {
        return jsToBoolean(typeDescriptor, value, context);
    } else if (Array.isArray(value)) {
        return jsToTuple(
            typeDescriptor,
            value.map((value) => deserialize(value, typeDescriptor, context)),
            context,
        );
    } else if (typeof value === "object" && "$wippleFunction" in value) {
        const func = functions[value.$wippleFunction];
        return jsToFunction(typeDescriptor, async (...inputs) => {
            return deserialize(await func(inputs), typeDescriptor, context);
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
