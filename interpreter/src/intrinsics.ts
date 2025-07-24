import Decimal from "decimal.js";
import * as values from "./values.ts";
import { Interpreter, InterpreterError, Value } from "./index.ts";

const intrinsics: Record<string, (args: Value[], interpreter: Interpreter) => Promise<Value>> = {
    debug: async ([value], interpreter) => {
        interpreter.debug?.(value);
        return value;
    },

    crash: async ([message]) => {
        throw new InterpreterError(values.toText(message));
    },

    display: async ([message], interpreter) => {
        interpreter.runtime("display", values.toText(message));
        return values.marker();
    },

    prompt: async ([message, validate], interpreter) => {
        let value: Value | undefined;
        await interpreter.runtime(
            "prompt",
            values.toText(message),
            interpreter.proxy(async (input: string) => {
                value = values.toMaybe(await interpreter.call(validate, [values.fromText(input)]));
                return value !== undefined;
            })
        );

        if (value === undefined) {
            throw new InterpreterError("'prompt' did not validate input");
        }

        return value;
    },

    "runtime-message": async ([message, value], interpreter) => {
        const result = await interpreter.runtime(
            values.toText(message),
            values.toJs(value, interpreter)
        );

        return values.fromJs(result);
    },

    "number-to-text": async ([value]) => {
        const number = values.toNumber(value);

        return values.fromText(number !== undefined ? number.toString() : "undefined");
    },

    "text-to-number": async ([value]) => {
        const text = values.toText(value);

        if (text === "undefined") {
            return values.fromNumber(undefined);
        } else {
            let decimal: Value | undefined;
            try {
                decimal = values.fromNumber(Decimal(text));
            } catch {
                decimal = undefined;
            }

            return values.fromMaybe(decimal);
        }
    },

    "add-number": async ([leftValue, rightValue]) => {
        const left = values.toNumber(leftValue);
        const right = values.toNumber(rightValue);

        return left !== undefined && right !== undefined
            ? values.fromNumber(left.add(right))
            : values.fromNumber(undefined);
    },

    "subtract-number": async ([leftValue, rightValue]) => {
        const left = values.toNumber(leftValue);
        const right = values.toNumber(rightValue);

        return left !== undefined && right !== undefined
            ? values.fromNumber(left.sub(right))
            : values.fromNumber(undefined);
    },

    "multiply-number": async ([leftValue, rightValue]) => {
        const left = values.toNumber(leftValue);
        const right = values.toNumber(rightValue);

        return left !== undefined && right !== undefined
            ? values.fromNumber(left.mul(right))
            : values.fromNumber(undefined);
    },

    "divide-number": async ([leftValue, rightValue]) => {
        const left = values.toNumber(leftValue);
        const right = values.toNumber(rightValue);

        return left !== undefined && right !== undefined && !right.isZero()
            ? values.fromNumber(left.div(right))
            : values.fromNumber(undefined);
    },

    "remainder-number": async ([leftValue, rightValue]) => {
        const left = values.toNumber(leftValue);
        const right = values.toNumber(rightValue);

        return left !== undefined && right !== undefined && !right.isZero()
            ? values.fromNumber(left.mod(right))
            : values.fromNumber(undefined);
    },

    "power-number": async ([leftValue, rightValue]) => {
        const left = values.toNumber(leftValue);
        const right = values.toNumber(rightValue);

        return left !== undefined && right !== undefined
            ? values.fromNumber(left.pow(right))
            : values.fromNumber(undefined);
    },

    "floor-number": async ([value]) => {
        const number = values.toNumber(value);
        return number !== undefined
            ? values.fromNumber(number.floor())
            : values.fromNumber(undefined);
    },

    "ceiling-number": async ([value]) => {
        const number = values.toNumber(value);
        return number !== undefined
            ? values.fromNumber(number.ceil())
            : values.fromNumber(undefined);
    },

    "sqrt-number": async ([value]) => {
        const number = values.toNumber(value);
        return number !== undefined && !number.isNegative()
            ? values.fromNumber(number.sqrt())
            : values.fromNumber(undefined);
    },

    sin: async ([value]) => {
        const number = values.toNumber(value);
        return number !== undefined
            ? values.fromNumber(Decimal.sin(number))
            : values.fromNumber(undefined);
    },

    cos: async ([value]) => {
        const number = values.toNumber(value);
        return number !== undefined
            ? values.fromNumber(Decimal.cos(number))
            : values.fromNumber(undefined);
    },

    tan: async ([value]) => {
        const number = values.toNumber(value);
        return number !== undefined
            ? values.fromNumber(Decimal.tan(number))
            : values.fromNumber(undefined);
    },

    "negate-number": async ([value]) => {
        const number = values.toNumber(value);
        return number !== undefined
            ? values.fromNumber(number.neg())
            : values.fromNumber(undefined);
    },

    "text-equality": async ([leftValue, rightValue]) => {
        const left = values.toText(leftValue);
        const right = values.toText(rightValue);

        return values.fromBoolean(left === right);
    },

    "number-equality": async ([leftValue, rightValue]) => {
        const left = values.toNumber(leftValue);
        const right = values.toNumber(rightValue);

        return values.fromBoolean(
            (left === undefined && right === undefined) ||
                (left !== undefined && right !== undefined && left.eq(right))
        );
    },

    "text-ordering": async ([leftValue, rightValue]) => {
        const left = values.toText(leftValue);
        const right = values.toText(rightValue);

        if (left < right) {
            return values.fromOrdering(-1);
        } else if (left === right) {
            return values.fromOrdering(0);
        } else {
            return values.fromOrdering(1);
        }
    },

    "number-ordering": async ([leftValue, rightValue]) => {
        const left = values.toNumber(leftValue);
        const right = values.toNumber(rightValue);

        if (left === undefined) {
            return right === undefined ? values.fromOrdering(0) : values.fromOrdering(-1);
        } else if (right === undefined) {
            return values.fromOrdering(1);
        } else {
            return values.fromOrdering(left.cmp(right) as -1 | 0 | 1);
        }
    },

    "make-empty-list": async () => {
        return values.fromList([]);
    },

    "list-first": async ([list]) => {
        const elements = values.toList(list);
        return values.fromMaybe(elements[0]);
    },

    "list-last": async ([list]) => {
        const elements = values.toList(list);
        return values.fromMaybe(elements[elements.length - 1]);
    },

    "list-initial": async ([list]) => {
        const elements = values.toList(list);

        return values.fromMaybe(
            elements.length > 0 ? values.fromList(elements.slice(0, -1)) : undefined
        );
    },

    "list-tail": async ([list]) => {
        const elements = values.toList(list);

        return values.fromMaybe(
            elements.length > 0 ? values.fromList(elements.slice(1)) : undefined
        );
    },

    "list-nth": async ([list, index]) => {
        const elements = values.toList(list);
        const indexValue = values.toListIndex(index, elements);

        return values.fromMaybe(indexValue !== undefined ? elements[indexValue] : undefined);
    },

    "list-append": async ([list, element]) => {
        const elements = values.toList(list);
        return values.fromList([...elements, element]);
    },

    "list-prepend": async ([element, list]) => {
        const elements = values.toList(list);
        return values.fromList([element, ...elements]);
    },

    "list-insert-at": async ([list, index, element]) => {
        const elements = values.toList(list);
        const indexValue = values.toListIndex(index, elements);

        return values.fromMaybe(
            indexValue !== undefined
                ? values.fromList([
                      ...elements.slice(0, indexValue),
                      element,
                      ...elements.slice(indexValue),
                  ])
                : undefined
        );
    },

    "list-remove-at": async ([list, index]) => {
        const elements = values.toList(list);
        const indexValue = values.toListIndex(index, elements);

        return values.fromMaybe(
            indexValue !== undefined
                ? values.fromList([
                      ...elements.slice(0, indexValue),
                      ...elements.slice(indexValue + 1),
                  ])
                : undefined
        );
    },

    "list-count": async ([list]) => {
        const elements = values.toList(list);
        return values.fromNumber(Decimal(elements.length));
    },

    "list-slice": async ([list, start, end]) => {
        const elements = values.toList(list);
        const startValue = values.toListIndex(start, elements);
        const endValue = values.toListIndex(end, elements);

        return values.fromMaybe(
            startValue !== undefined && endValue !== undefined && startValue <= endValue
                ? values.fromList(elements.slice(startValue, endValue))
                : undefined
        );
    },

    "text-characters": async ([text]) => {
        const textValue = values.toText(text);
        return values.fromList(textValue.split("").map(values.fromText));
    },

    "random-number": async ([min, max]) => {
        const minValue = values.toNumber(min);
        const maxValue = values.toNumber(max);

        if (minValue === undefined || maxValue === undefined) {
            return values.fromNumber(undefined);
        }

        if (minValue.gt(maxValue)) {
            return values.fromNumber(minValue);
        }

        const sd = Math.max(minValue.sd(), maxValue.sd()) - 1;
        const random = minValue.add(maxValue.sub(minValue).mul(Decimal.random()));

        return values.fromNumber(sd === 0 ? random.floor() : random.toSignificantDigits(sd));
    },

    "undefined-number": async () => {
        return values.fromNumber(undefined);
    },

    "make-hasher": async () => {
        return values.hasher();
    },

    "hash-number": async ([hasher, number]) => {
        const hash = values.toHasher(hasher);
        const value = values.toNumber(number);
        return values.fromNumber(Decimal(hash(value)));
    },

    "hash-text": async ([hasher, text]) => {
        const hash = values.toHasher(hasher);
        const value = values.toText(text);
        return values.fromNumber(Decimal(hash(value)));
    },
};

export default intrinsics;
