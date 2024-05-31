import { Decimal } from "decimal.js";
import { intrinsics, callFunction } from "./intrinsics.js";
import type * as compiler from "wipple-compiler";
import { Mutex } from "async-mutex";

export { callFunction };

Decimal.set({ modulo: Decimal.EUCLID });

export type Executable = compiler.linker_Executable;
export type Item = compiler.linker_LinkedItem;
export type TypeDescriptor = compiler.ir_TypeDescriptor;
export type Instruction = compiler.ir_Instruction;
export type TypedInstruction = compiler.ir_TypedInstruction;

export type TypedValue = Value;

type Value =
    | {
          type: "number";
          value: Decimal | undefined;
      }
    | {
          type: "text";
          value: string;
      }
    | {
          type: "function";
          path: string;
          substitutions: Record<string, TypeDescriptor>;
          captures: { current: TypedValue }[];
      }
    | {
          type: "nativeFunction";
          value: (...inputs: TypedValue[]) => Promise<TypedValue>;
      }
    | {
          type: "variant";
          variant: number;
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
          fields: TypedValue[];
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
          type: "taskLocalKey";
          value: symbol;
      }
    | {
          type: "hasher";
      };

export interface Context {
    executable: Executable;
    debug: boolean;
    gc: () => void;
    io: (request: IoRequest) => void;
    call: (func: TypedValue, inputs: TypedValue[], task: TaskLocals) => Promise<TypedValue>;
    do: (block: TypedValue, task: TaskLocals) => Promise<TypedValue>;
    itemCache: Record<string, [Mutex, TypedValue | undefined]>;
    getItem: (
        path: string,
        substitutions: TypeDescriptor[] | Record<string, TypeDescriptor>,
        typeDescriptor: TypeDescriptor,
        task: TaskLocals,
    ) => Promise<TypedValue>;
    error: (
        options:
            | string
            | {
                  instruction: Instruction;
                  message: string;
                  stack: TypedValue[];
                  value?: TypedValue;
              },
    ) => Error;
    backgroundTasks: Promise<void>[];
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

export type TaskLocals = Record<symbol, TypedValue>;

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
        gc: options.gc,
        io: options.io,
        debug: options.debug,
        call: async (func, inputs, task) => {
            switch (func.type) {
                case "function": {
                    const result = (await evaluateItem(
                        func.path,
                        executable.items[func.path].ir,
                        [...inputs].reverse(),
                        [...func.captures],
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
            if (block.type !== "function") {
                throw new InterpreterError("expected function value");
            }

            const result = (await evaluateItem(
                block.path,
                executable.items[block.path].ir,
                [],
                [...block.captures],
                block.substitutions,
                task,
                context,
            ))!;

            context.gc();

            return result;
        },
        itemCache: {},
        getItem: async (path, parametersOrSubstitutions, typeDescriptor, task) => {
            const item = context.executable.items[path];
            if (!item) {
                throw new InterpreterError(`missing item: ${path}`);
            }

            let mutex: Mutex | undefined;
            if (item.evaluateOnce) {
                if (context.itemCache[path]) {
                    const [mutex] = context.itemCache[path];
                    return await mutex.runExclusive(() => {
                        const value = context.itemCache[path][1];
                        if (!value) {
                            throw new InterpreterError(`missing cached value for ${path}`);
                        }

                        return value;
                    });
                } else {
                    mutex = new Mutex();
                    context.itemCache[path] = [mutex, undefined];
                }
            }

            await mutex?.acquire();

            const substitutions = Array.isArray(parametersOrSubstitutions)
                ? Object.fromEntries(
                      item.parameters.map(
                          (parameter, index) =>
                              [parameter, parametersOrSubstitutions[index]] as const,
                      ),
                  )
                : parametersOrSubstitutions;

            if (context.debug) {
                console.error("## initializing constant:", {
                    path,
                    typeDescriptor,
                    substitutions,
                    item,
                });
            }

            const result = (await evaluateItem(
                path,
                item.ir,
                [],
                [],
                substitutions,
                task,
                context,
            ))!;

            context.gc();

            mutex?.release();

            if (item.evaluateOnce) {
                context.itemCache[path][1] = result;
            }

            return result;
        },
        error: (options) => {
            if (typeof options === "string") {
                return new InterpreterError(options);
            } else {
                const { instruction, message, stack, value } = options;

                return new InterpreterError(
                    `${JSON.stringify(instruction)}: ${message}\n${
                        value ? `while evaluating ${JSON.stringify(value, null, 4)}\n` : ""
                    }stack: ${JSON.stringify(stack, null, 4)}`,
                );
            }
        },
        backgroundTasks: [],
        taskLocals: new Map(),
    };

    for (const path of executable.entrypoints) {
        const entrypoint = executable.items[path];

        const task: TaskLocals = {};

        if (context.debug) {
            console.error("## evaluating entrypoint block");
        }

        const block = (await evaluateItem("top-level", entrypoint.ir, [], [], {}, task, context))!;

        if (context.debug) {
            console.error("## executing entrypoint block");
        }

        await context.do(block, task);
    }

    await Promise.all(context.backgroundTasks);
};

const evaluateItem = async (
    path: string,
    instructions: Instruction[],
    stack: TypedValue[],
    scope: { current: TypedValue }[],
    substitutions: Record<string, TypeDescriptor>,
    task: TaskLocals,
    context: Context,
) => {
    let instruction: Instruction | undefined;
    let blocks = [[...instructions]];
    while (true) {
        while ((instruction = blocks[blocks.length - 1]?.shift())) {
            const error = (message: string, value?: TypedValue) =>
                context.error({ instruction: instruction!, message, stack, value });

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

            const break_ = (n: number) => {
                for (let i = 0; i <= n; i++) {
                    if (!blocks.pop()) {
                        throw error("ran out of blocks");
                    }
                }
            };

            if (context.debug) {
                console.error(
                    "## evaluating:",
                    JSON.stringify({ path, instruction, stack, scope, substitutions }, null, 4),
                );
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
                case "variantElement": {
                    const value = peek();
                    if (value.type !== "variant") {
                        throw error("expected variant", value);
                    }

                    const element = value.values[instruction.value]!;
                    stack.push(element);

                    break;
                }
                case "tupleElement": {
                    const value = peek();
                    if (value.type !== "tuple") {
                        throw error("expected tuple", value);
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
                        type: "tuple",
                        values: elements,
                    });

                    break;
                }
                case "typed": {
                    const typedInstruction = instruction;
                    const getTypeDescriptor = () =>
                        substituteTypeDescriptor(typedInstruction.value[0], substitutions);

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

                            const result = await intrinsic(inputs.reverse(), context, task);

                            stack.push(result);

                            break;
                        }
                        case "number": {
                            stack.push({
                                type: "number",
                                value: new Decimal(instruction.value[1].value),
                            });

                            break;
                        }
                        case "text": {
                            stack.push({
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
                                type: "text",
                                value: result,
                            });

                            break;
                        }
                        case "marker": {
                            stack.push({
                                type: "marker",
                            });

                            break;
                        }
                        case "structure": {
                            const elements: TypedValue[] = [];
                            for (const _field of instruction.value[1].value) {
                                elements.push(pop());
                            }

                            const fields: TypedValue[] = new Array(
                                instruction.value[1].value.length,
                            );

                            for (const index of instruction.value[1].value) {
                                fields[index] = elements.pop()!;
                            }

                            stack.push({
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
                                type: "variant",
                                variant: instruction.value[1].value[0],
                                values: elements.reverse(),
                            });

                            break;
                        }
                        case "wrapper": {
                            const value = pop();
                            stack.push({
                                type: "wrapper",
                                value,
                            });

                            break;
                        }
                        case "function": {
                            const [captureList, path] = instruction.value[1].value;

                            const captures = captureList.map((index) => scope[index]);

                            stack.push({
                                type: "function",
                                path,
                                substitutions,
                                captures,
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
                                getTypeDescriptor(),
                                task,
                            );

                            stack.push(value);
                            break;
                        }
                        case "instance": {
                            const typeDescriptor = getTypeDescriptor();

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
                case "block": {
                    blocks.push([...instruction.value]);
                    break;
                }
                case "break": {
                    break_(instruction.value);
                    break;
                }
                case "breakIfNot": {
                    const value = peek();
                    if (value.type !== "variant") {
                        throw error("expected variant", value);
                    }

                    if (value.variant !== instruction.value[0]) {
                        break_(instruction.value[1]);
                    }

                    break;
                }
                case "return": {
                    return pop();
                }
                case "tailCall": {
                    const inputs: TypedValue[] = [];
                    for (let i = 0; i < instruction.value; i++) {
                        inputs.push(pop());
                    }

                    const func = pop();

                    if (stack.length !== 0) {
                        throw error("stack is not empty");
                    }

                    if (func.type === "function") {
                        path = func.path;
                        blocks = [[...context.executable.items[func.path].ir]];
                        stack.push(...inputs);
                        scope = [...func.captures];
                        substitutions = func.substitutions;
                        context.gc();
                        continue;
                    } else {
                        inputs.reverse();
                        return await context.call(func, inputs, task);
                    }
                }
                case "tailDo": {
                    const block = pop();

                    if (stack.length !== 0) {
                        throw error("stack is not empty");
                    }

                    if (block.type !== "function") {
                        throw error("expected function", block);
                    }

                    path = block.path;
                    blocks = [[...context.executable.items[block.path].ir]];
                    scope = [...block.captures];
                    substitutions = block.substitutions;
                    context.gc();
                    continue;
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

        if (!blocks.pop()) {
            break;
        }
    }

    throw context.error("ran out of instructions");
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
