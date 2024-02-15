import { Decimal } from "decimal.js";
import { intrinsics } from "./intrinsics.js";
import { produce } from "immer";

export interface Executable {
    items: Record<string, Item>;
    instances: Record<string, string[]>;
    intrinsicVariants: Record<string, string>;
    code: Item[];
}

export interface Item {
    parameters: string[];
    typeDescriptor: TypeDescriptor;
    ir: Instruction[][];
}

export type TypeDescriptor =
    | { type: "parameter"; value: string }
    | { type: "named"; value: [string, TypeDescriptor[]] }
    | { type: "function"; value: [TypeDescriptor, TypeDescriptor] }
    | { type: "tuple"; value: TypeDescriptor[] }
    | { type: "deferred"; value: TypeDescriptor };

export type Instruction =
    | { type: "copy"; value: undefined }
    | { type: "drop"; value: undefined }
    | { type: "initialize"; value: number }
    | { type: "field"; value: string }
    | { type: "element"; value: number }
    | { type: "variable"; value: number }
    | { type: "call"; value: undefined }
    | { type: "tuple"; value: number }
    | { type: "typed"; value: [TypeDescriptor, TypedInstruction] }
    | { type: "jumpIfNot"; value: [string, number] }
    | { type: "end"; value: undefined }
    | { type: "return"; value: undefined }
    | { type: "jump"; value: number }
    | { type: "tailCall"; value: undefined }
    | { type: "unreachable"; value: undefined };

export type TypedInstruction =
    | { type: "intrinsic"; value: [string, number] }
    | { type: "text"; value: string }
    | { type: "number"; value: string }
    | { type: "format"; value: [string[], string] }
    | { type: "structure"; value: string[] }
    | { type: "variant"; value: [string, number] }
    | { type: "function"; value: [number[], string, number] }
    | { type: "deferred"; value: [number[], string, number] }
    | { type: "constant"; value: [string, TypeDescriptor[]] }
    | { type: "instance"; value: string };

export type TypedValue = Value & { typeDescriptor: TypeDescriptor };

type Value =
    | {
          type: "number";
          value: Decimal;
      }
    | {
          type: "text";
          value: string;
      }
    | {
          type: "function";
          scope: Record<number, TypedValue>;
          path: string;
          ir: Instruction[][];
          label: number;
          substitutions: Record<string, TypeDescriptor>;
          tailCallable: boolean;
      }
    | {
          type: "nativeFunction";
          value: (value: TypedValue) => Promise<TypedValue>;
      }
    | {
          type: "deferred";
          scope: Record<number, TypedValue>;
          path: string;
          ir: Instruction[][];
          label: number;
          substitutions: Record<string, TypeDescriptor>;
      }
    | {
          type: "variant";
          variant: string;
          values: TypedValue[];
      }
    | {
          type: "reference";
          value: { current: TypedValue };
      }
    | {
          type: "list";
          values: TypedValue[];
      }
    | {
          type: "structure";
          fields: Record<string, TypedValue>;
      }
    | {
          type: "tuple";
          values: TypedValue[];
      }
    | {
          type: "uiHandle";
          onMessage: (message: string, value: TypedValue) => Promise<TypedValue>;
      }
    | {
          type: "taskGroup";
          value: TaskGroup;
      }
    | {
          type: "hasher";
          key: number[];
      };

export interface Context {
    executable: Executable;
    topLevel: Item[];
    initializedItems: Record<string, [Record<string, TypeDescriptor>, TypedValue][]>;
    debug: boolean;
    io: (request: IoRequest) => void;
    call: (func: TypedValue, input: TypedValue) => Promise<TypedValue>;
    produce: (deferred: TypedValue) => Promise<TypedValue>;
    getItem: (
        path: string,
        substitutions: TypeDescriptor[] | Record<string, TypeDescriptor>,
        typeDescriptor: TypeDescriptor,
    ) => Promise<TypedValue>;
    error: (
        options:
            | string
            | {
                  label: number;
                  instruction: Instruction;
                  message: string;
                  stack: TypedValue[];
                  value?: TypedValue;
              },
    ) => Error;
}

export type IoRequest =
    | {
          type: "display";
          message: string;
          completion: () => void;
      }
    | {
          type: "prompt";
          message: string;
          validate: (message: string) => Promise<boolean>;
      }
    | {
          type: "choice";
          message: string;
          choices: string[];
          completion: (index: number) => void;
      }
    | {
          type: "ui";
          url: string;
          completion: (
              sendMessage: (message: string, value: TypedValue) => Promise<TypedValue>,
          ) => Promise<void>;
      }
    | {
          type: "sleep";
          ms: number;
          completion: () => void;
      };

export type TaskGroup = (() => Promise<void>)[];

export class InterpreterError extends Error {}

export const evaluate = async (
    executable: Executable,
    options: {
        debug: boolean;
        io: (request: IoRequest) => Promise<void>;
    },
) => {
    const context: Context = {
        executable,
        topLevel: executable.code,
        initializedItems: {},
        io: options.io,
        debug: options.debug,
        call: async (func, input) => {
            switch (func.type) {
                case "function": {
                    return (await evaluateItem(
                        func.path,
                        func.ir,
                        func.label,
                        [input],
                        { ...func.scope },
                        func.substitutions,
                        context,
                    ))!;
                }
                case "nativeFunction": {
                    return await func.value(input);
                }
                default:
                    throw new InterpreterError("expected function");
            }
        },
        produce: async (defer) => {
            if (defer.type !== "deferred") {
                throw new InterpreterError("expected defer value");
            }

            return (await evaluateItem(
                defer.path,
                defer.ir,
                defer.label,
                [],
                { ...defer.scope },
                defer.substitutions,
                context,
            ))!;
        },
        getItem: async (path, parametersOrSubstitutions, typeDescriptor) => {
            const item = context.executable.items[path];

            const substitutions = Array.isArray(parametersOrSubstitutions)
                ? Object.fromEntries(
                      item.parameters.map(
                          (parameter, index) =>
                              [parameter, parametersOrSubstitutions[index]] as const,
                      ),
                  )
                : parametersOrSubstitutions;

            for (const [substitutions, value] of context.initializedItems[path] ?? []) {
                if (!unify(typeDescriptor, value.typeDescriptor, { ...substitutions })) {
                    continue;
                }

                return value;
            }

            if (context.debug) {
                console.error("## initializing constant:", { path, typeDescriptor, substitutions });
            }

            const value = (await evaluateItem(path, item.ir, 0, [], {}, substitutions, context))!;
            (context.initializedItems[path] ??= []).push([substitutions, value]);

            return value;
        },
        error: (options) => {
            if (typeof options === "string") {
                return new InterpreterError(options);
            } else {
                const { label, instruction, message, stack, value } = options;

                return new InterpreterError(
                    `${label}: ${JSON.stringify(instruction)}: ${message}\n${
                        value ? `while evaluating ${JSON.stringify(value, null, 4)}\n` : ""
                    }stack: ${JSON.stringify(stack, null, 4)}`,
                );
            }
        },
    };

    for (const item of executable.code) {
        await evaluateItem("top-level", item.ir, 0, [], {}, {}, context);
    }
};

const evaluateItem = async (
    path: string,
    item: Instruction[][],
    label: number,
    stack: TypedValue[],
    scope: Record<number, TypedValue>,
    substitutions: Record<string, TypeDescriptor>,
    context: Context,
) => {
    outer: while (true) {
        const instructions = item[label];

        for (const instruction of instructions) {
            const error = (message: string, value?: TypedValue) =>
                context.error({ label, instruction, message, stack, value });

            const peek = () => {
                const value = stack[stack.length - 1];
                if (value == null) {
                    throw error("stack is empty");
                }

                return value;
            };

            const pop = () => {
                const value = stack.pop();
                if (value == null) {
                    console.log("STACK LENGTH:", stack.length);
                    throw error("stack is empty");
                }

                return value;
            };

            if (context.debug) {
                console.error("## evaluating:", {
                    path,
                    label,
                    instruction,
                    stack,
                    scope,
                    substitutions,
                });
            }

            switch (instruction.type) {
                case "copy": {
                    const value = peek();
                    stack.push(value);
                    break;
                }
                case "drop": {
                    pop();
                    break;
                }
                case "initialize": {
                    const value = peek();
                    scope[instruction.value] = value;
                    break;
                }
                case "field": {
                    const value = peek();
                    if (value.type !== "structure") {
                        throw error("expected structure", value);
                    }

                    const field = value.fields[instruction.value]!;
                    stack.push(field);

                    break;
                }
                case "element": {
                    const value = peek();
                    if (value.type !== "variant" && value.type !== "tuple") {
                        throw error("expected variant or tuple", value);
                    }

                    const element = value.values[instruction.value]!;
                    stack.push(element);

                    break;
                }
                case "variable": {
                    const value = scope[instruction.value];
                    stack.push(value);
                    break;
                }
                case "call": {
                    const input = pop();
                    const func = pop();

                    const result = await context.call(func, input);
                    stack.push(result);

                    break;
                }
                case "tuple": {
                    const elements: TypedValue[] = [];
                    for (let i = 0; i < instruction.value; i++) {
                        elements.push(pop());
                    }

                    elements.reverse();

                    stack.push({
                        typeDescriptor: {
                            type: "tuple",
                            value: elements.map((element) => element.typeDescriptor),
                        },
                        type: "tuple",
                        values: elements,
                    });

                    break;
                }
                case "typed": {
                    const typeDescriptor = produce(instruction.value[0], (typeDescriptor) =>
                        substituteTypeDescriptor(typeDescriptor, substitutions),
                    );

                    switch (instruction.value[1].type) {
                        case "intrinsic": {
                            const intrinsic = intrinsics[instruction.value[1].value[0]];
                            if (!intrinsic) {
                                throw error(`unknown intrinsic '${instruction.value[0]}'`);
                            }

                            const inputs: TypedValue[] = [];
                            for (let i = 0; i < instruction.value[1].value[1]; i++) {
                                inputs.push(pop());
                            }

                            const result = await intrinsic(
                                inputs.reverse(),
                                typeDescriptor,
                                context,
                            );

                            stack.push(result);

                            break;
                        }
                        case "number": {
                            stack.push({
                                typeDescriptor,
                                type: "number",
                                value: new Decimal(instruction.value[1].value),
                            });

                            break;
                        }
                        case "text": {
                            stack.push({
                                typeDescriptor,
                                type: "text",
                                value: instruction.value[1].value,
                            });

                            break;
                        }
                        case "format": {
                            const inputs = instruction.value[1].value[0].map(() => pop());

                            let result = "";
                            for (const segment of instruction.value[1].value[0]) {
                                const value = inputs.pop()!;
                                if (value.type !== "text") {
                                    throw error("expected text", value);
                                }

                                result += segment + value.value;
                            }

                            result += instruction.value[1].value[1];

                            stack.push({
                                typeDescriptor,
                                type: "text",
                                value: result,
                            });

                            break;
                        }
                        case "structure": {
                            const elements: TypedValue[] = [];
                            for (const _field of instruction.value[1].value) {
                                elements.push(pop());
                            }

                            const fields: Record<string, TypedValue> = {};
                            for (const field of instruction.value[1].value) {
                                fields[field] = elements.pop()!;
                            }

                            stack.push({
                                typeDescriptor,
                                type: "structure",
                                fields,
                            });

                            break;
                        }
                        case "variant": {
                            const elements: TypedValue[] = [];
                            for (let i = 0; i < instruction.value[1].value[1]; i++) {
                                elements.push(pop());
                            }

                            stack.push({
                                typeDescriptor,
                                type: "variant",
                                variant: instruction.value[1].value[0],
                                values: elements.reverse(),
                            });

                            break;
                        }
                        case "function": {
                            const path = instruction.value[1].value[1];

                            stack.push({
                                typeDescriptor,
                                type: "function",
                                path,
                                ir: path ? context.executable.items[path].ir : item,
                                label: instruction.value[1].value[2],
                                substitutions,
                                scope,
                                // A function is only tall callable if it has no captures
                                tailCallable: instruction.value[1].value[0].length === 0,
                            });

                            break;
                        }
                        case "deferred": {
                            const path = instruction.value[1].value[1];

                            stack.push({
                                type: "deferred",
                                typeDescriptor,
                                path,
                                ir: path ? context.executable.items[path].ir : item,
                                label: instruction.value[1].value[2],
                                substitutions,
                                scope,
                            });

                            break;
                        }
                        case "constant": {
                            const [path, parameters] = instruction.value[1].value;

                            const value = await context.getItem(
                                path,
                                parameters.map((parameter) =>
                                    substituteTypeDescriptor(parameter, substitutions),
                                ),
                                typeDescriptor,
                            );

                            stack.push(value);
                            break;
                        }
                        case "instance": {
                            const [path, substitutions] = findInstance(
                                instruction.value[1].value,
                                typeDescriptor,
                                context.executable,
                            );

                            const value = await context.getItem(
                                path,
                                substitutions,
                                typeDescriptor,
                            );

                            stack.push(value);

                            break;
                        }
                        default: {
                            instruction.value[1] satisfies never;
                            throw error("unknown instruction");
                        }
                    }

                    break;
                }
                case "jumpIfNot": {
                    const value = peek();
                    if (value.type !== "variant") {
                        throw error("expected variant", value);
                    }

                    if (!value.variant) {
                        throw error("variant is undefined", value);
                    }

                    if (value.variant !== instruction.value[0]) {
                        label = instruction.value[1];
                        continue outer;
                    }

                    break;
                }
                case "end": {
                    return undefined;
                }
                case "return": {
                    return pop();
                }
                case "jump": {
                    label = instruction.value;
                    continue outer;
                }
                case "tailCall": {
                    const input = pop();
                    const func = pop();

                    if (func.type === "function" && func.tailCallable) {
                        item = func.ir;
                        label = func.label;
                        scope = {};
                        substitutions = func.substitutions;
                        stack.push(input);
                        continue outer;
                    } else {
                        return await context.call(func, input);
                    }
                }
                case "unreachable": {
                    throw error("evaluated unreachable instruction");
                }
                default: {
                    instruction satisfies never;
                    throw error("unknown instruction");
                }
            }
        }
    }
};

const findInstance = (trait: string, typeDescriptor: TypeDescriptor, executable: Executable) => {
    for (const path of executable.instances[trait] ?? []) {
        const instance = executable.items[path];
        const substitutions: Record<string, TypeDescriptor> = {};
        if (unify(typeDescriptor, instance.typeDescriptor, substitutions)) {
            return [path, substitutions] as const;
        }
    }

    throw new InterpreterError(
        `no instance found for trait ${trait} with type descriptor ${JSON.stringify(
            typeDescriptor,
        )}`,
    );
};

const unify = (
    left: TypeDescriptor,
    right: TypeDescriptor,
    substitutions: Record<string, TypeDescriptor>,
): boolean => {
    switch (right.type) {
        case "parameter":
            if (substitutions[right.value]) {
                return unify(left, substitutions[right.value], substitutions);
            } else {
                if (left.type === "parameter") {
                    if (left.value !== right.value) {
                        return false;
                    }
                }

                substitutions[right.value] = left;
                return true;
            }
        case "function":
            return (
                left.type === "function" &&
                unify(left.value[0], right.value[0], substitutions) &&
                unify(left.value[1], right.value[1], substitutions)
            );
        case "named":
            return (
                left.type === "named" &&
                left.value[0] === right.value[0] &&
                left.value[1].length === right.value[1].length &&
                left.value[1].every((typeDescriptor, index) =>
                    unify(typeDescriptor, right.value[1][index], substitutions),
                )
            );
        case "tuple":
            return (
                left.type === "tuple" &&
                left.value.length === right.value.length &&
                left.value.every((typeDescriptor, index) =>
                    unify(typeDescriptor, right.value[index], substitutions),
                )
            );
        case "deferred":
            // Coercions are done at compile time, so at runtime `defer` only
            // unifies with `defer`
            return left.type === "deferred" && unify(left.value, right.value, substitutions);
        default:
            right satisfies never;
            throw new InterpreterError(`unknown type descriptor ${JSON.stringify(right)}`);
    }
};

const substituteTypeDescriptor = (
    typeDescriptor: TypeDescriptor,
    substitutions: Record<string, TypeDescriptor>,
): TypeDescriptor => {
    switch (typeDescriptor.type) {
        case "function":
            return {
                type: "function",
                value: [
                    substituteTypeDescriptor(typeDescriptor.value[0], substitutions),
                    substituteTypeDescriptor(typeDescriptor.value[1], substitutions),
                ],
            };
        case "parameter":
            if (!substitutions[typeDescriptor.value]) {
                throw new InterpreterError(
                    `no substitution for type parameter ${typeDescriptor.value}`,
                );
            }

            return substitutions[typeDescriptor.value];
        case "named":
            return {
                type: "named",
                value: [
                    typeDescriptor.value[0],
                    typeDescriptor.value[1].map((typeDescriptor) =>
                        substituteTypeDescriptor(typeDescriptor, substitutions),
                    ),
                ],
            };
        case "tuple":
            return {
                type: "tuple",
                value: typeDescriptor.value.map((typeDescriptor) =>
                    substituteTypeDescriptor(typeDescriptor, substitutions),
                ),
            };
        case "deferred":
            return {
                type: "deferred",
                value: substituteTypeDescriptor(typeDescriptor.value, substitutions),
            };
        default:
            typeDescriptor satisfies never;
            throw new InterpreterError(`unknown type descriptor ${JSON.stringify(typeDescriptor)}`);
    }
};
