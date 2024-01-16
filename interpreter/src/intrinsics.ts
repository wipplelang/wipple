import { Decimal } from "decimal.js";
import type { Context, TaskGroup, TypeDescriptor, TypedValue } from "./index.js";

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
    "number-equality": async ([left, right], context) => {
        if (left.type !== "number" || right.type !== "number") {
            throw context.error("expected numbers");
        }

        return jsToBoolean(left.value.eq(right.value), context);
    },
    "text-equality": async ([left, right], context) => {
        if (left.type !== "text" || right.type !== "text") {
            throw context.error("expected text");
        }

        return jsToBoolean(left.value === right.value, context);
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
