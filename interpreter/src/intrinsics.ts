import { Decimal } from "decimal.js";
import type { Context, TaskGroup, TypeDescriptor, TypedValue } from "./index.js";
// @ts-ignore
import { hash, hash_uint } from "siphash";

export type Intrinsic = (inputs: TypedValue[], context: Context) => Promise<TypedValue>;

export const intrinsics: Record<string, Intrinsic> = {
    crash: async ([message], context) => {
        if (message.type !== "text") {
            throw context.error("expected text");
        }

        throw context.error(`error: ${message.value}`);
    },
    evaluate: async ([lazy], context) => {
        return await context.evaluate(lazy);
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
    prompt: async ([message, validate], context) => {
        let value: TypedValue | undefined;
        do {
            await new Promise<void>((resolve) => {
                context.io({
                    type: "prompt",
                    message: textToJs(message, context),
                    validate: async (input) => {
                        value = maybeToJs(
                            await context.call(validate, jsToText(input, context)),
                            context
                        );

                        return value != null;
                    },
                });

                resolve();
            });
        } while (!value);

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
    "with-ui": async ([url, callback], context) => {
        return await new Promise<TypedValue>(async (resolve) => {
            context.io({
                type: "ui",
                url: textToJs(url, context),
                completion: async (onMessage) => {
                    const result = await context.call(callback, jsToUiHandle(onMessage, context));
                    resolve(result);
                },
            });
        });
    },
    "message-ui": async ([handle, message, value], context) => {
        const sendMessage = uiHandleToJs(handle, context);
        const messageName = textToJs(message, context);

        return await sendMessage(messageName, value);
    },
    "with-continuation": async ([callback], context) => {
        if (callback.typeDescriptor.type !== "function") {
            throw context.error("expected function");
        }

        const callbackTypeDescriptor = callback.typeDescriptor.value[0];

        if (callbackTypeDescriptor.type !== "function") {
            throw context.error("expected function");
        }

        const inputTypeDescriptor = callbackTypeDescriptor.value[0];

        return await new Promise<TypedValue>(async (resolve) => {
            await context.call(
                callback,
                jsToFunction(inputTypeDescriptor, unit.typeDescriptor, async (result) => {
                    resolve(result);
                    return unit;
                })
            );
        });
    },
    "with-task-group": async ([callback], context) => {
        const taskGroup: TaskGroup = [];
        await context.call(callback, jsToTaskGroup(taskGroup, context));

        await Promise.all(taskGroup.map((task) => task()));

        return unit;
    },
    task: async ([taskGroup, callback], context) => {
        if (taskGroup.type !== "taskGroup") {
            throw context.error("expected task group");
        }

        taskGroup.value.push(async () => {
            await context.call(callback, unit);
        });

        return unit;
    },
    "in-background": async ([callback], context) => {
        (async () => {
            await context.call(callback, unit);
        })();

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
        return jsToText(numberToJs(number, context).toString(), context);
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
    "sin-number": async ([number], context) => {
        return jsToNumber(numberToJs(number, context).sin(), context);
    },
    "cos-number": async ([number], context) => {
        return jsToNumber(numberToJs(number, context).cos(), context);
    },
    "tan-number": async ([number], context) => {
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
            (leftNumber.isNaN() && rightNumber.isNaN()) || leftNumber.eq(rightNumber),
            context
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
        return jsToList(listValue.slice(0, listValue.length - 1), context);
    },
    "list-tail": async ([list], context) => {
        const listValue = listToJs(list, context);
        return jsToList(listValue.slice(1), context);
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

        return jsToList(
            [...listValue.slice(0, index), element, ...listValue.slice(index)],
            context
        );
    },
    "list-remove-at": async ([list, position], context) => {
        const listValue = listToJs(list, context);
        const positionValue = numberToJs(position, context);

        if (!positionValue.isInt()) {
            throw context.error("list position must be a whole number");
        }

        const index = positionValue.toNumber();

        return jsToList([...listValue.slice(0, index), ...listValue.slice(index + 1)], context);
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
            context
        );
    },
    "random-number": async ([min, max], context) => {
        const minValue = numberToJs(min, context);
        const maxValue = numberToJs(max, context);

        if (minValue.gt(maxValue)) {
            throw context.error("min must be less than or equal to max");
        }

        return jsToNumber(
            minValue.add(
                maxValue
                    .sub(minValue)
                    .mul(Decimal.random())
                    .toSignificantDigits(Math.max(minValue.sd(), maxValue.sd()))
            ),
            context
        );
    },
    "undefined-number": async ([], context) => {
        return jsToNumber(new Decimal(NaN), context);
    },
    "make-hasher": async ([], context) => {
        return jsToHasher(
            [Math.random(), Math.random(), Math.random(), Math.random()],
            Math.random(),
            context
        );
    },
    "hash-into-hasher": async ([hasher, message], context) => {
        const hasherValue = hasherToJs(hasher, context);
        const messageValue = numberToJs(message, context);

        const messageBytes = new Uint8Array(new Float64Array(messageValue.toNumber()).buffer);

        return jsToHasher(
            hasherValue.key,
            hash(hash_uint(hasherValue.key, hasherValue.hash), messageBytes),
            context
        );
    },
    "value-of-hasher": async ([hasher], context) => {
        const hasherValue = hasherToJs(hasher, context);
        return jsToNumber(new Decimal(hasherValue.hash), context);
    },
    "hash-text": async ([text], context) => {
        const textValue = textToJs(text, context);

        return jsToNumber(
            new Decimal(
                hash([Math.random(), Math.random(), Math.random(), Math.random()], textValue)
            ),
            context
        );
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

const jsToText = (string: string, context: Context): TypedValue => ({
    type: "text",
    typeDescriptor: context.executable.intrinsicTypeDescriptors.text,
    value: string,
});

const textToJs = (value: TypedValue, context: Context): string => {
    if (value.type !== "text") {
        throw context.error("expected text");
    }

    return value.value;
};

const jsToNumber = (number: Decimal, context: Context): TypedValue => ({
    type: "number",
    typeDescriptor: context.executable.intrinsicTypeDescriptors.number,
    value: number,
});

const numberToJs = (value: TypedValue, context: Context): Decimal => {
    if (value.type !== "number") {
        throw context.error("expected number");
    }

    return value.value;
};

const jsToBoolean = (boolean: boolean, context: Context): TypedValue => ({
    typeDescriptor: context.executable.intrinsicTypeDescriptors.boolean,
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

const jsToSome = (value: TypedValue, context: Context): TypedValue => ({
    typeDescriptor: context.executable.intrinsicTypeDescriptors.maybe,
    type: "variant",
    variant: context.executable.intrinsicVariants.some,
    values: [value],
});

const jsToNone = (context: Context): TypedValue => ({
    typeDescriptor: context.executable.intrinsicTypeDescriptors.maybe,
    type: "variant",
    variant: context.executable.intrinsicVariants.none,
    values: [],
});

const jsToIsLessThan = (context: Context): TypedValue => ({
    typeDescriptor: context.executable.intrinsicTypeDescriptors.ordering,
    type: "variant",
    variant: context.executable.intrinsicVariants["is-less-than"],
    values: [],
});

const jsToIsEqual = (context: Context): TypedValue => ({
    typeDescriptor: context.executable.intrinsicTypeDescriptors.ordering,
    type: "variant",
    variant: context.executable.intrinsicVariants["is-equal"],
    values: [],
});

const jsToIsGreaterThan = (context: Context): TypedValue => ({
    typeDescriptor: context.executable.intrinsicTypeDescriptors.ordering,
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

const jsToList = (values: TypedValue[], context: Context): TypedValue => ({
    typeDescriptor: context.executable.intrinsicTypeDescriptors.list,
    type: "list",
    values,
});

const listToJs = (value: TypedValue, context: Context): TypedValue[] => {
    if (value.type !== "list") {
        throw context.error("expected list");
    }

    return value.values;
};

const jsToFunction = (
    inputTypeDescriptor: TypeDescriptor,
    outputTypeDescriptor: TypeDescriptor,
    func: (input: TypedValue) => Promise<TypedValue>
): TypedValue => ({
    type: "nativeFunction",
    typeDescriptor: {
        type: "function",
        value: [inputTypeDescriptor, outputTypeDescriptor],
    },
    value: func,
});

const jsToUiHandle = (
    sendMessage: (message: string, value: TypedValue) => Promise<TypedValue>,
    context: Context
): TypedValue => ({
    typeDescriptor: context.executable.intrinsicTypeDescriptors.uiHandle,
    type: "uiHandle",
    onMessage: sendMessage,
});

const uiHandleToJs = (value: TypedValue, context: Context) => {
    if (value.type !== "uiHandle") {
        throw context.error("expected ui handle");
    }

    return value.onMessage;
};

const jsToTaskGroup = (taskGroup: TaskGroup, context: Context): TypedValue => ({
    type: "taskGroup",
    typeDescriptor: context.executable.intrinsicTypeDescriptors.taskGroup,
    value: taskGroup,
});

const jsToReference = (value: TypedValue, context: Context): TypedValue => ({
    type: "reference",
    typeDescriptor: context.executable.intrinsicTypeDescriptors.reference,
    value: { current: value },
});

const referenceToJs = (value: TypedValue, context: Context): { current: TypedValue } => {
    if (value.type !== "reference") {
        throw context.error("expected reference");
    }

    return value.value;
};

const jsToHasher = (key: number[], hash: number, context: Context): TypedValue => ({
    type: "hasher",
    typeDescriptor: context.executable.intrinsicTypeDescriptors.hasher,
    key,
    hash,
});

const hasherToJs = (value: TypedValue, context: Context): { key: number[]; hash: number } => {
    if (value.type !== "hasher") {
        throw context.error("expected hasher");
    }

    return value;
};
