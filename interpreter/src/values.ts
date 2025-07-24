import Decimal from "decimal.js";
import { Interpreter, InterpreterError, Value } from "./index.ts";
import stringHash from "string-hash";

export const marker = (): Value => ({ type: "marker" });

export const fromText = (text: string): Value => ({
    type: "text",
    value: text,
});

export const toText = (value: Value) => {
    if (value.type !== "text") {
        throw new InterpreterError("expected text");
    }

    return value.value;
};

export const fromNumber = (number: Decimal | undefined): Value => ({
    type: "number",
    value: number,
});

export const toNumber = (value: Value) => {
    if (value.type !== "number") {
        throw new InterpreterError("expected number");
    }

    return value.value;
};

export const fromBoolean = (boolean: boolean): Value => ({
    type: "variant",
    variant: boolean ? 1 : 0,
    values: [],
});

export const none = (): Value => ({
    type: "variant",
    variant: 0,
    values: [],
});

export const fromSome = (value: Value): Value => ({
    type: "variant",
    variant: 1,
    values: [value],
});

export const fromMaybe = (value: Value | undefined): Value =>
    value !== undefined ? fromSome(value) : none();

export const toMaybe = (value: Value) => {
    if (value.type !== "variant") {
        throw new InterpreterError("expected variant");
    }

    switch (value.variant) {
        case 0: {
            return undefined;
        }
        case 1: {
            return value.values[0];
        }
        default: {
            throw new InterpreterError("expected maybe");
        }
    }
};

export const isLessThan = (): Value => ({
    type: "variant",
    variant: 0,
    values: [],
});

export const isEqual = (): Value => ({
    type: "variant",
    variant: 1,
    values: [],
});

export const isGreaterThan = (): Value => ({
    type: "variant",
    variant: 2,
    values: [],
});

export const fromOrdering = (ordering: -1 | 0 | 1): Value => ({
    type: "variant",
    variant: ordering + 1,
    values: [],
});

export const fromList = (elements: Value[]): Value => ({
    type: "list",
    elements,
});

export const toList = (value: Value) => {
    if (value.type !== "list") {
        throw new InterpreterError("expected list");
    }

    return value.elements;
};

export const toListIndex = (value: Value, list: Value[]) => {
    const index = toNumber(value);

    return index?.isInt() && index.gte(0) && index.lt(list.length) ? index.toNumber() : undefined;
};

export const fromTuple = (elements: Value[]): Value => ({
    type: "tuple",
    elements,
});

export const toTuple = (value: Value) => {
    if (value.type !== "tuple") {
        throw new InterpreterError("expected tuple");
    }

    return value.elements;
};

export const fromFunction = (
    func: (inputs: Value[], interpreter: Interpreter) => Promise<Value>
): Value => ({
    type: "jsFunction",
    function: func,
});

export const hasher = (): Value => ({
    type: "hasher",
    hash: (value) => {
        if (typeof value === "undefined") {
            return 0;
        } else if (typeof value === "string") {
            return stringHash(value);
        } else if (value instanceof Decimal) {
            return stringHash(value.toString());
        } else {
            value satisfies never;
            throw new InterpreterError("unhashable value");
        }
    },
});

export const toHasher = (value: Value) => {
    if (value.type !== "hasher") {
        throw new InterpreterError("expected hasher");
    }

    return value.hash;
};

export const fromJs = (value: unknown): Value => {
    if (value === undefined || value === null) {
        return marker();
    } else if (typeof value === "number") {
        return fromNumber(new Decimal(value));
    } else if (typeof value === "string") {
        return fromText(value);
    } else if (typeof value === "function") {
        return fromFunction(async (inputs, interpreter) =>
            fromJs(await value(...inputs.map((value) => toJs(value, interpreter))))
        );
    } else if (typeof value === "boolean") {
        return fromBoolean(value);
    } else if (Array.isArray(value)) {
        return fromTuple(value.map(fromJs));
    } else {
        throw new Error("cannot convert JS value to Wipple");
    }
};

export const toJs = (value: Value, interpreter: Interpreter): unknown => {
    switch (value.type) {
        case "marker":
            return undefined;
        case "number":
            return toNumber(value)?.toNumber() ?? NaN;
        case "text":
            return toText(value);
        case "function": {
            return async (...inputs: unknown[]) =>
                toJs(await interpreter.call(value, inputs.map(fromJs)), interpreter);
        }
        case "list":
            return toList(value).map((value) => toJs(value, interpreter));
        case "tuple":
            return toTuple(value).map((value) => toJs(value, interpreter));
        default:
            throw new Error("cannot convert Wipple value to JS");
    }
};
