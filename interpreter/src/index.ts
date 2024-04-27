import { Decimal } from "decimal.js";
import { intrinsics, callFunction } from "./intrinsics.js";
import { produce } from "immer";
import type * as compiler from "wipple-compiler";

export { callFunction };

Decimal.set({ modulo: Decimal.EUCLID });

export type Executable = compiler.linker_Executable;
export type Item = compiler.linker_LinkedItem;
export type TypeDescriptor = compiler.codegen_TypeDescriptor;
export type Instruction = compiler.codegen_Instruction;
export type TypedInstruction = compiler.codegen_TypedInstruction;

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
          scope: Record<number, { current: TypedValue }>;
          path: string;
          ir: Instruction[][];
          label: number;
          substitutions: Record<string, TypeDescriptor>;
      }
    | {
          type: "nativeFunction";
          value: (...inputs: TypedValue[]) => Promise<TypedValue>;
      }
    | {
          type: "block";
          scope: Record<number, { current: TypedValue }>;
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
          type: "marker";
      }
    | {
          type: "wrapper";
          value: TypedValue;
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
          type: "taskGroup";
          value: TaskGroup;
      }
    | {
          type: "hasher";
      };

export interface Context {
    executable: Executable;
    topLevel: Item[];
    debug: boolean;
    gc: () => void;
    io: (request: IoRequest) => void;
    call: (func: TypedValue, inputs: TypedValue[], task: any) => Promise<TypedValue>;
    do: (block: TypedValue, task: any) => Promise<TypedValue>;
    getItem: (
        path: string,
        substitutions: TypeDescriptor[] | Record<string, TypeDescriptor>,
        typeDescriptor: TypeDescriptor,
        task: any,
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
    onceCache: [TypedValue, () => Promise<void>][];
    taskLocals: Map<any, TypedValue[]>;
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
          message: string;
          value: any;
          completion: (value: any) => void;
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
        gc: () => void;
        io: (request: IoRequest) => Promise<void>;
    },
) => {
    const context: Context = {
        executable,
        topLevel: executable.code,
        gc: options.gc,
        io: options.io,
        debug: options.debug,
        call: async (func, inputs, task) => {
            switch (func.type) {
                case "function": {
                    const result = (await evaluateItem(
                        func.path,
                        func.ir,
                        func.label,
                        [...inputs].reverse(),
                        { ...func.scope },
                        func.substitutions,
                        task,
                        context,
                    ))!;

                    context.gc();

                    return result;
                }
                case "nativeFunction": {
                    return await func.value(...inputs);
                }
                default:
                    throw new InterpreterError("expected function");
            }
        },
        do: async (block, task) => {
            if (block.type !== "block") {
                throw new InterpreterError("expected block value");
            }

            const result = (await evaluateItem(
                block.path,
                block.ir,
                block.label,
                [],
                { ...block.scope },
                block.substitutions,
                task,
                context,
            ))!;

            context.gc();

            return result;
        },
        getItem: async (path, parametersOrSubstitutions, typeDescriptor, task) => {
            const item = context.executable.items[path];

            const substitutions = Array.isArray(parametersOrSubstitutions)
                ? Object.fromEntries(
                      item.parameters.map(
                          (parameter, index) =>
                              [parameter, parametersOrSubstitutions[index]] as const,
                      ),
                  )
                : parametersOrSubstitutions;

            if (context.debug) {
                console.error("## initializing constant:", { path, typeDescriptor, substitutions });
            }

            const result = (await evaluateItem(
                path,
                item.ir,
                0,
                [],
                {},
                substitutions,
                task,
                context,
            ))!;

            context.gc();

            return result;
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
        onceCache: [],
        taskLocals: new Map(),
    };

    for (const item of executable.code) {
        const task = () => {}; // marker
        const block = (await evaluateItem("top-level", item.ir, 0, [], {}, {}, task, context))!;
        await context.do(block, task);
    }

    for (const [_value, cleanup] of context.onceCache) {
        await cleanup();
    }
};

const evaluateItem = async (
    path: string,
    item: Instruction[][],
    label: number,
    stack: TypedValue[],
    scope: Record<number, { current: TypedValue }>,
    substitutions: Record<string, TypeDescriptor>,
    task: any,
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
                    scope[instruction.value] = { current: value };
                    break;
                }
                case "mutate": {
                    const value = pop();
                    scope[instruction.value].current = value;
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
                case "unwrap": {
                    const value = peek();
                    if (value.type !== "wrapper") {
                        throw error("expected wrapper", value);
                    }

                    const wrapped = value.value;
                    stack.push(wrapped);

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
                    const value = scope[instruction.value].current;
                    stack.push(value);
                    break;
                }
                case "call": {
                    const inputs: TypedValue[] = [];
                    for (let i = 0; i < instruction.value; i++) {
                        inputs.push(pop());
                    }

                    inputs.reverse();

                    const func = pop();

                    const result = await context.call(func, inputs, task);
                    stack.push(result);

                    break;
                }
                case "do": {
                    const block = pop();

                    const result = await context.do(block, task);
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
                                task,
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
                        case "marker": {
                            stack.push({
                                typeDescriptor,
                                type: "marker",
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
                        case "wrapper": {
                            const value = pop();
                            stack.push({
                                typeDescriptor,
                                type: "wrapper",
                                value,
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
                            });

                            break;
                        }
                        case "block": {
                            const path = instruction.value[1].value[1];

                            stack.push({
                                type: "block",
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
                                task,
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
                                task,
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
                case "return": {
                    return pop();
                }
                case "jump": {
                    label = instruction.value;
                    continue outer;
                }
                case "tailCall": {
                    const inputs: TypedValue[] = [];
                    for (let i = 0; i < instruction.value; i++) {
                        inputs.push(pop());
                    }

                    const func = pop();

                    if (func.type === "function") {
                        item = func.ir;
                        label = func.label;
                        stack.push(...inputs);
                        scope = { ...func.scope };
                        substitutions = func.substitutions;
                        context.gc();
                        continue outer;
                    } else {
                        inputs.reverse();
                        return await context.call(func, inputs, task);
                    }
                }
                case "tailDo": {
                    const block = pop();

                    if (block.type !== "block") {
                        throw error("expected block", block);
                    }

                    item = block.ir;
                    label = block.label;
                    scope = { ...block.scope };
                    substitutions = block.substitutions;
                    context.gc();
                    continue outer;
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
    for (const instances of [
        executable.instances[trait] ?? [],
        executable.defaultInstances[trait] ?? [],
    ]) {
        for (const path of instances) {
            const instance = executable.items[path];
            const substitutions: Record<string, TypeDescriptor> = {};
            if (unify(typeDescriptor, instance.typeDescriptor, substitutions)) {
                return [path, substitutions] as const;
            }
        }
    }

    throw new InterpreterError(
        `no instance found for trait ${trait} with type descriptor ${JSON.stringify(
            typeDescriptor,
        )}`,
    );
};

export const unify = (
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
                left.value[0].length === right.value[0].length &&
                left.value[0].every((typeDescriptor, index) =>
                    unify(typeDescriptor, right.value[0][index], substitutions),
                ) &&
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
        case "block":
            return left.type === "block" && unify(left.value, right.value, substitutions);
        case "intrinsic":
            return left.type === "intrinsic";
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
                    typeDescriptor.value[0].map((typeDescriptor) =>
                        substituteTypeDescriptor(typeDescriptor, substitutions),
                    ),
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
        case "block":
            return {
                type: "block",
                value: substituteTypeDescriptor(typeDescriptor.value, substitutions),
            };
        case "intrinsic": {
            return { type: "intrinsic" };
        }
        default:
            typeDescriptor satisfies never;
            throw new InterpreterError(`unknown type descriptor ${JSON.stringify(typeDescriptor)}`);
    }
};
