import Decimal from "decimal.js";
import type * as ir from "../ir.d.ts";
import intrinsics from "./intrinsics.ts";
import { freeze } from "immer";

Decimal.set({
    precision: 96,
    modulo: Decimal.EUCLID,
});

export interface Variable {
    value: Value;
}

export type Value =
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
          path: ir.Path;
          substitutions: Record<ir.Path, ir.TypeDescriptor>;
          bounds: ir.Path[];
          captures: Variable[];
      }
    | {
          type: "jsFunction";
          function: (args: Value[], interpreter: Interpreter) => Promise<Value>;
      }
    | {
          type: "variant";
          variant: number;
          values: Value[];
      }
    | {
          type: "list";
          elements: Value[];
      }
    | {
          type: "marker";
      }
    | {
          type: "wrapper";
          value: Value;
      }
    | {
          type: "structure";
          fields: Value[];
      }
    | {
          type: "tuple";
          elements: Value[];
      }
    | {
          type: "hasher";
          hash: (value: string | Decimal | undefined) => number;
      };

export type Runtime = (message: string, ...args: unknown[]) => Promise<unknown>;

export class InterpreterError extends Error {}

export class Interpreter {
    executable: ir.Executable;
    runtime: Runtime;
    debug: typeof console.log | undefined;
    proxy: <T>(value: T) => T;

    constructor(options: {
        executable: ir.Executable;
        runtime: Runtime;
        debug?: typeof console.log;
        proxy?: <T>(value: T) => T;
    }) {
        this.executable = options.executable;
        this.runtime = options.runtime;
        this.debug = options.debug;
        this.proxy = options.proxy ?? ((value) => value);
        freeze(this, true);
    }

    async run() {
        if (!this.executable.entrypoint) {
            this.debug?.("nothing to evaluate");
            return;
        }

        const entrypoint = this.executable.items[this.executable.entrypoint];

        this.debug?.("evaluating entrypoint block");

        const block = await this.evaluate(
            this.executable.entrypoint,
            entrypoint.ir,
            [],
            {},
            {},
            []
        );

        this.debug?.("evaluating entrypoint block");

        await this.call(block!, []);
    }

    async call(func: Value, inputs: Value[]) {
        switch (func.type) {
            case "function": {
                const instructions = this.executable.items[func.path].ir;

                const captures: Record<number, Variable> = {};
                func.captures.forEach((variable, index) => {
                    captures[index] = variable;
                });

                const value = await this.evaluate(
                    func.path,
                    instructions,
                    [...inputs].reverse(),
                    captures,
                    func.substitutions,
                    func.bounds
                );

                return value!;
            }
            case "jsFunction": {
                return func.function(inputs, this);
            }
            default: {
                throw new InterpreterError("expected function");
            }
        }
    }

    private async evaluateItem(
        path: ir.Path,
        substitutions: Record<ir.Path, ir.TypeDescriptor> | ir.TypeDescriptor[],
        bounds: ir.Path[]
    ) {
        const item = this.executable.items[path];

        if (Array.isArray(substitutions)) {
            substitutions = Object.fromEntries(
                item.parameters.map((parameter, index) => [
                    parameter,
                    (substitutions as ir.TypeDescriptor[])[index],
                ])
            );
        }

        // Any type parameters not mentioned in the item's type must be inserted
        // into the substitutions by resolving the bounds
        const resolvedBounds = item.bounds.map((bound) => {
            const parameters = [...bound.parameters].map((parameter) =>
                this.substituteTypeDescriptor(parameter, substitutions)
            );

            return this.findInstance(bound.traitPath, parameters, bounds, substitutions);
        });

        this.debug?.(
            `initializing constant ${path} with substitutions ${JSON.stringify(
                substitutions
            )} and bounds ${JSON.stringify(bounds)}`
        );

        return this.evaluate(path, item.ir, [], {}, substitutions, resolvedBounds);
    }

    private async evaluate(
        path: ir.Path,
        instructions: ir.Instruction[],
        stack: Value[],
        scope: Record<number, Variable>,
        substitutions: Record<ir.Path, ir.TypeDescriptor>,
        bounds: ir.Path[]
    ): Promise<Value | undefined> {
        freeze(substitutions, true);
        freeze(bounds, true);

        let blocks = [[...instructions]];

        const push = (value: Value) => {
            stack.push(value);
        };

        const peek = () => {
            if (stack.length === 0) {
                throw new InterpreterError("stack is empty");
            }

            return stack[stack.length - 1];
        };

        const tryPop = () => stack.pop();

        const pop = () => {
            if (stack.length === 0) {
                throw new InterpreterError("stack is empty");
            }

            return stack.pop()!;
        };

        const popN = (n: number) => {
            if (stack.length < n) {
                throw new InterpreterError("not enough values on the stack");
            }

            return stack.splice(stack.length - n, n);
        };

        const break_ = (n: number) => {
            for (let i = 0; i <= n; i++) {
                if (blocks.length === 0) {
                    throw new Error("ran out of blocks");
                }

                blocks.pop()!;
            }
        };

        while (true) {
            while (blocks.length > 0 && blocks[blocks.length - 1].length > 0) {
                const instruction = blocks[blocks.length - 1].shift()!;

                this.debug?.(`evaluating ${path}:`, { instruction, stack, scope, substitutions });

                switch (instruction.type) {
                    case "copy": {
                        const value = peek();
                        push(value);
                        break;
                    }
                    case "drop": {
                        pop();
                        break;
                    }
                    case "initialize": {
                        const variable = instruction.value;

                        const value = peek();

                        if (variable in scope) {
                            throw new Error(`variable ${variable} already initialized`);
                        }

                        scope[variable] = { value };

                        break;
                    }
                    case "field": {
                        const index = instruction.value;

                        const value = peek();

                        if (value.type !== "structure") {
                            throw new InterpreterError("expected structure");
                        }

                        push(value.fields[index]);

                        break;
                    }
                    case "variantElement": {
                        const index = instruction.value;

                        const value = peek();

                        if (value.type !== "variant") {
                            throw new InterpreterError("expected variant");
                        }

                        push(value.values[index]);

                        break;
                    }
                    case "tupleElement": {
                        const index = instruction.value;

                        const value = peek();

                        if (value.type !== "tuple") {
                            throw new InterpreterError("expected variant");
                        }

                        push(value.elements[index]);

                        break;
                    }
                    case "unwrap": {
                        const value = peek();

                        if (value.type !== "wrapper") {
                            throw new InterpreterError("expected variant");
                        }

                        push(value.value);

                        break;
                    }
                    case "variable": {
                        const variable = instruction.value;

                        push(scope[variable].value);

                        break;
                    }
                    case "call": {
                        const inputs = popN(instruction.value);

                        const func = pop();

                        const result = await this.call(func, inputs);

                        push(result!);

                        break;
                    }
                    case "do": {
                        const block = pop();

                        const result = await this.call(block, []);

                        push(result!);

                        break;
                    }
                    case "mutate": {
                        const variable = instruction.value;

                        const value = pop();

                        scope[variable].value = value;

                        break;
                    }
                    case "tuple": {
                        const elements = popN(instruction.value);

                        push({ type: "tuple", elements });

                        break;
                    }
                    case "intrinsic": {
                        const { name, inputs } = instruction.value;

                        const intrinsic = intrinsics[name];
                        if (!intrinsic) {
                            throw new InterpreterError(`unknown intrinsic '${name}'`);
                        }

                        const inputValues = popN(inputs);

                        const result = await intrinsic(inputValues, this);

                        push(result);

                        break;
                    }
                    case "text": {
                        const text = instruction.value;

                        push({ type: "text", value: text });

                        break;
                    }
                    case "number": {
                        const number = instruction.value;

                        push({ type: "number", value: Decimal(number).toSignificantDigits() });

                        break;
                    }
                    case "format": {
                        const { segments, trailing } = instruction.value;

                        const inputs = popN(segments.length);

                        let result = "";
                        segments.forEach((segment, index) => {
                            const value = inputs[index];
                            if (value.type !== "text") {
                                throw new InterpreterError("expected text");
                            }

                            result += segment + value.value;
                        });

                        result += trailing;

                        push({ type: "text", value: result });

                        break;
                    }
                    case "marker": {
                        push({ type: "marker" });

                        break;
                    }
                    case "structure": {
                        const fields = instruction.value;

                        const elements = popN(fields.length);

                        const fieldValues = new Array<Value>(fields.length);
                        fields.forEach((field, index) => {
                            fieldValues[field] = elements[index];
                        });

                        push({ type: "structure", fields: fieldValues });

                        break;
                    }
                    case "variant": {
                        const { variant, elements } = instruction.value;

                        const elementValues = popN(elements);

                        push({ type: "variant", variant, values: elementValues });

                        break;
                    }
                    case "wrapper": {
                        const value = pop();

                        push({ type: "wrapper", value });

                        break;
                    }
                    case "function": {
                        const { captures, path } = instruction.value;

                        const captureValues = captures.map((index) => scope[index]);

                        push({
                            type: "function",
                            path,
                            substitutions,
                            bounds,
                            captures: captureValues,
                        });

                        break;
                    }
                    case "constant": {
                        const { path, parameters } = instruction.value;

                        const substitutedParameters = parameters.map((parameter) =>
                            this.substituteTypeDescriptor(structuredClone(parameter), substitutions)
                        );

                        const value = await this.evaluateItem(path, substitutedParameters, bounds);

                        push(value!);

                        break;
                    }
                    case "instance": {
                        const { traitPath, parameters } = instruction.value;

                        const substitutedParameters = parameters.map((parameter) =>
                            this.substituteTypeDescriptor(structuredClone(parameter), substitutions)
                        );

                        const instanceSubstitutions: Record<ir.Path, ir.TypeDescriptor> = {};
                        const instancePath = this.findInstance(
                            traitPath,
                            substitutedParameters,
                            bounds,
                            instanceSubstitutions
                        );

                        const value = await this.evaluateItem(
                            instancePath,
                            instanceSubstitutions,
                            bounds
                        );

                        push(value!);

                        break;
                    }
                    case "block": {
                        const instructions = instruction.value;

                        blocks.push([...instructions]);

                        break;
                    }
                    case "break": {
                        const count = instruction.value;

                        break_(count);

                        break;
                    }
                    case "breakIfNot": {
                        const { variant: condition, count } = instruction.value;

                        const current = peek();
                        if (current.type !== "variant") {
                            throw new InterpreterError("expected variant");
                        }

                        if (current.variant !== condition) {
                            break_(count);
                        }

                        break;
                    }
                    case "return": {
                        return tryPop();
                    }
                    case "tailCall": {
                        const inputs = popN(instruction.value);

                        const func = pop();

                        if (stack.length > 0) {
                            throw new Error("stack is not empty for tail call");
                        }

                        if (func.type === "function") {
                            path = func.path;

                            blocks = [[...this.executable.items[path].ir]];

                            inputs.reverse();
                            stack = inputs;

                            scope = {};
                            func.captures.forEach((variable, index) => {
                                scope[index] = variable;
                            });

                            substitutions = func.substitutions;
                            bounds = func.bounds;

                            continue;
                        } else {
                            inputs.reverse();
                            return this.call(func, inputs);
                        }
                    }
                    case "tailDo": {
                        const block = pop();

                        if (block.type !== "function") {
                            throw new InterpreterError("expected function");
                        }

                        if (stack.length > 0) {
                            throw new Error("stack is not empty for tail call");
                        }

                        path = block.path;

                        blocks = [[...this.executable.items[path].ir]];

                        scope = {};
                        block.captures.forEach((variable, index) => {
                            scope[index] = variable;
                        });

                        substitutions = block.substitutions;
                        bounds = block.bounds;

                        continue;
                    }
                    case "unreachable": {
                        throw new InterpreterError("evaluated 'unreachable' instruction");
                    }
                    default: {
                        instruction satisfies never;
                        throw new InterpreterError("unknown instruction");
                    }
                }
            }

            if (blocks.pop() === undefined) {
                throw new InterpreterError(`ran out of instructions in ${path}`);
            }
        }
    }

    private findInstance(
        trait: ir.Path,
        parameters: ir.TypeDescriptor[],
        bounds: ir.Path[],
        substitutions: Record<ir.Path, ir.TypeDescriptor>
    ): ir.Path {
        const tryInstance = (instance: ir.Instance | undefined): boolean => {
            if (!instance) {
                return false;
            }

            const instanceSubstitutions: Record<ir.Path, ir.TypeDescriptor> = {};
            let unified = true;
            for (let index = 0; index < parameters.length; index++) {
                const typeDescriptor = parameters[index];
                const traitParameter = instance.traitParameters[index];

                if (
                    this.unify(typeDescriptor, traitParameter.typeDescriptor, instanceSubstitutions)
                ) {
                    instanceSubstitutions[traitParameter.path] = typeDescriptor;
                } else {
                    unified = false;
                    break;
                }
            }

            if (unified) {
                for (const [parameter, typeDescriptor] of Object.entries(instanceSubstitutions)) {
                    // Important: don't override existing parameters -- that would
                    // prevent recursive instances from being resolved properly
                    substitutions[parameter] ??= typeDescriptor;
                }
            }

            return unified;
        };

        for (const bound of bounds) {
            const instance =
                this.executable.instances[trait]?.[bound] ??
                this.executable.defaultInstances[trait]?.[bound];

            if (tryInstance(instance)) {
                return instance.path;
            }
        }

        for (const instance of [
            ...Object.values(this.executable.instances[trait] ?? {}),
            ...Object.values(this.executable.defaultInstances[trait] ?? {}),
        ]) {
            if (tryInstance(instance)) {
                return instance.path;
            }
        }

        throw new InterpreterError(
            `no instance found for trait ${trait} with parameters ${JSON.stringify(parameters)}`
        );
    }

    private unify(
        left: ir.TypeDescriptor,
        right: ir.TypeDescriptor,
        substitutions: Record<ir.Path, ir.TypeDescriptor>
    ): boolean {
        if (left.type === "equal") {
            return (
                this.unify(left.value.left, right, substitutions) &&
                this.unify(left.value.right, left.value.left, substitutions)
            );
        }

        if (right.type === "equal") {
            return (
                this.unify(left, right.value.left, substitutions) &&
                this.unify(right.value.right, right.value.left, substitutions)
            );
        }

        if (left.type === "parameter") {
            // This occurs when bounds are being resolved that involve type
            // parameters not mentioned in the item's type descriptor; no
            // value's type will ever contain a `Parameter`
            return true;
        }

        if (right.type === "parameter") {
            if (right.value in substitutions) {
                return this.unify(left, substitutions[right.value], substitutions);
            }

            substitutions[right.value] = left;
            return true;
        }

        if (left.type === "function") {
            return (
                right.type === "function" &&
                left.value.inputs.length === right.value.inputs.length &&
                left.value.inputs
                    .map((left, index) => [left, right.value.inputs[index]] as const)
                    .every(([left, right]) => this.unify(left, right, substitutions)) &&
                this.unify(left.value.output, right.value.output, substitutions)
            );
        }

        if (left.type === "named") {
            return (
                right.type === "named" &&
                left.value.path === right.value.path &&
                left.value.parameters.length === right.value.parameters.length &&
                left.value.parameters
                    .map((left, index) => [left, right.value.parameters[index]] as const)
                    .every(([left, right]) => this.unify(left, right, substitutions))
            );
        }

        if (left.type === "tuple") {
            return (
                right.type === "tuple" &&
                left.value.length === right.value.length &&
                left.value
                    .map((left, index) => [left, right.value[index]] as const)
                    .every(([left, right]) => this.unify(left, right, substitutions))
            );
        }

        if (left.type === "block") {
            return right.type === "block" && this.unify(left.value, right.value, substitutions);
        }

        if (left.type === "intrinsic") {
            return right.type === "intrinsic";
        }

        left satisfies never;
        return false;
    }

    private substituteTypeDescriptor(
        typeDescriptor: ir.TypeDescriptor,
        substitutions: Record<ir.Path, ir.TypeDescriptor>
    ): ir.TypeDescriptor {
        switch (typeDescriptor.type) {
            case "parameter": {
                const parameter = typeDescriptor.value;

                if (parameter in substitutions) {
                    typeDescriptor = substitutions[parameter];
                } else {
                    // This occurs when bounds are being resolved that involve type
                    // parameters not mentioned in the item's type descriptor; no
                    // value's type will ever contain a `Parameter`
                }

                break;
            }
            case "named": {
                typeDescriptor.value.parameters = typeDescriptor.value.parameters.map((parameter) =>
                    this.substituteTypeDescriptor(parameter, substitutions)
                );

                break;
            }
            case "function": {
                typeDescriptor.value.output = this.substituteTypeDescriptor(
                    typeDescriptor.value.output,
                    substitutions
                );

                break;
            }
            case "tuple": {
                typeDescriptor.value = typeDescriptor.value.map((element) =>
                    this.substituteTypeDescriptor(element, substitutions)
                );

                break;
            }
            case "block": {
                typeDescriptor.value = this.substituteTypeDescriptor(
                    typeDescriptor.value,
                    substitutions
                );

                break;
            }
            case "intrinsic": {
                break;
            }
            case "equal": {
                typeDescriptor.value.left = this.substituteTypeDescriptor(
                    typeDescriptor.value.left,
                    substitutions
                );

                typeDescriptor.value.right = this.substituteTypeDescriptor(
                    typeDescriptor.value.right,
                    substitutions
                );

                break;
            }
            default: {
                typeDescriptor satisfies never;
                throw new InterpreterError("unknown type descriptor");
            }
        }

        return typeDescriptor;
    }
}
