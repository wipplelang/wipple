// MARK: Core

const __wipple_variant = (index, values) => {
    values[__wipple_variant] = index;
    return values;
};

__wipple_variant.toString = () => "<variant>";

const __wipple_constant = async (func, types, substitutions) => {
    const result = await func(__wipple_substitute(types, substitutions));

    if (result === undefined) {
        throw new Error();
    }

    return result;
};

const __wipple_trait = async (instances, types, substitutions) => {
    types = __wipple_substitute(types, substitutions);

    for (const [func, instanceSubstitutions] of instances) {
        const copy = {};

        let unified = true;
        for (const parameter of Object.keys(instanceSubstitutions)) {
            if (!__wipple_unify(types[parameter], instanceSubstitutions[parameter], copy)) {
                unified = false;
                break;
            }
        }

        if (unified) {
            for (const [parameter, ty] of Object.entries(instanceSubstitutions)) {
                // Important: don't override existing parameters -- that would
                // prevent recursive instances from being resolved properly
                copy[parameter] ??= ty;
            }

            return await func(copy);
        }
    }

    throw new Error(`no instance found for types ${JSON.stringify(types)}`);
};

const __wipple_fromBoolean = (value) => (value ? __wipple_variant(1, []) : __wipple_variant(0, []));

const __wipple_fromMaybe = (value) =>
    value !== undefined ? __wipple_variant(1, [value]) : __wipple_variant(0, []);

const __wipple_toMaybe = (value) => {
    switch (value[__wipple_variant]) {
        case 0: {
            return undefined;
        }
        case 1: {
            return value[0];
        }
        default: {
            throw new Error("expected maybe");
        }
    }
};

const __wipple_fromOrdering = (ordering) => __wipple_variant(ordering + 1, []);

const __wipple_isValidListIndex = (index, list, includeEnd = false) =>
    index === Math.floor(index) &&
    index >= 0 &&
    (includeEnd ? index <= list.length : index < list.length);

const __wipple_substitute = (types, substitutions) => {
    const copy = structuredClone(substitutions);
    for (const [parameter, ty] of Object.entries(copy)) {
        copy[parameter] = __wipple_substituteType(ty, types);
    }
    return copy;
};

const __wipple_substituteType = (ty, substitutions) => {
    switch (ty.type) {
        case "parameter": {
            return substitutions[ty.name] ?? ty;
        }
        case "named": {
            return {
                type: "named",
                name: ty.name,
                parameters: ty.parameters.map((param) =>
                    __wipple_substituteType(param, substitutions),
                ),
            };
        }

        case "function": {
            return {
                type: "function",
                inputs: ty.inputs.map((input) => __wipple_substituteType(input, substitutions)),
                output: __wipple_substituteType(ty.output, substitutions),
            };
        }

        case "tuple": {
            return {
                type: "tuple",
                elements: ty.elements.map((element) =>
                    __wipple_substituteType(element, substitutions),
                ),
            };
        }

        case "block": {
            return {
                type: "block",
                output: __wipple_substituteType(ty.output, substitutions),
            };
        }
        default: {
            return ty;
        }
    }
};

const __wipple_unify = (left, right, substitutions) => {
    if (left.type === "parameter") {
        // This occurs when bounds are being resolved that involve type
        // parameters not mentioned in the item's type descriptor; no value's
        // type will ever contain a `parameter`
        return true;
    }

    if (right.type === "parameter") {
        if (right.name in substitutions) {
            return __wipple_unify(left, substitutions[right.name], substitutions);
        }

        substitutions[right.name] = left;
        return true;
    }

    if (left.type === "function") {
        return (
            right.type === "function" &&
            left.inputs.length === right.inputs.length &&
            left.inputs.every((leftInput, index) =>
                __wipple_unify(leftInput, right.inputs[index], substitutions),
            ) &&
            __wipple_unify(left.output, right.output, substitutions)
        );
    }

    if (left.type === "named") {
        return (
            right.type === "named" &&
            left.name === right.name &&
            left.parameters.length === right.parameters.length &&
            left.parameters.every((leftParameter, index) =>
                __wipple_unify(leftParameter, right.parameters[index], substitutions),
            )
        );
    }

    if (left.type === "tuple") {
        return (
            right.type === "tuple" &&
            left.elements.length === right.elements.length &&
            left.elements.every((leftElement, index) =>
                __wipple_unify(leftElement, right.elements[index], substitutions),
            )
        );
    }

    if (left.type === "block") {
        return right.type === "block" && __wipple_unify(left.output, right.output, substitutions);
    }

    return false;
};

// MARK: Runtime

const __wipple_runtime_debug = async (value) => {
    __wipple_env.debug?.(value);
    return value;
};

const __wipple_runtime_crash = async (message) => {
    throw new Error(message);
};

const __wipple_runtime_display = async (message) => {
    await __wipple_env.display(message);
    return [];
};

const __wipple_runtime_prompt = async (message, validate) => {
    let value;
    await __wipple_env.prompt(
        message,
        __wipple_proxy(async (input) => {
            const validated = await validate(input);
            value = __wipple_toMaybe(validated);
            return value !== undefined;
        }),
    );

    if (value === undefined) {
        throw new Error("'prompt' did not validate input");
    }

    return value;
};

const __wipple_runtime_external = async (func, input) => {
    return await __wipple_env[func](__wipple_proxy(input));
};

const __wipple_runtime_number_to_string = async (number) => {
    return number.toString();
};

const __wipple_runtime_string_to_number = async (string) => {
    if (string.toLowerCase() === "nan") {
        return __wipple_fromMaybe(NaN);
    } else {
        const number = parseFloat(string);
        return isNaN(number) ? none : __wipple_fromMaybe(number);
    }
};

const __wipple_runtime_add_numbers = async (left, right) => {
    return left + right;
};

const __wipple_runtime_subtract_numbers = async (left, right) => {
    return left - right;
};

const __wipple_runtime_multiply_numbers = async (left, right) => {
    return left * right;
};

const __wipple_runtime_divide_numbers = async (left, right) => {
    return left / right;
};

const __wipple_runtime_remainder_numbers = async (left, right) => {
    return left % right;
};

const __wipple_runtime_power_numbers = async (left, right) => {
    return Math.pow(left, right);
};

const __wipple_runtime_floor_number = async (number) => {
    return Math.floor(number);
};

const __wipple_runtime_ceiling_number = async (number) => {
    return Math.ceil(number);
};

const __wipple_runtime_sqrt_number = async (number) => {
    return Math.sqrt(number);
};

const __wipple_runtime_sin = async (number) => {
    return Math.sin(number);
};

const __wipple_runtime_cos = async (number) => {
    return Math.cos(number);
};

const __wipple_runtime_tan = async (number) => {
    return Math.tan(number);
};

const __wipple_runtime_negate_number = async (number) => {
    return -number;
};

const __wipple_runtime_string_equality = async (left, right) => {
    return __wipple_fromBoolean(left === right);
};

const __wipple_runtime_number_equality = async (left, right) => {
    return __wipple_fromBoolean(left === right);
};

const __wipple_runtime_string_ordering = async (left, right) => {
    if (left < right) {
        return __wipple_fromOrdering(-1);
    } else if (left === right) {
        return __wipple_fromOrdering(0);
    } else {
        return __wipple_fromOrdering(1);
    }
};

const __wipple_runtime_number_ordering = async (left, right) => {
    if (left < right) {
        return __wipple_fromOrdering(-1);
    } else if (left === right) {
        return __wipple_fromOrdering(0);
    } else {
        return __wipple_fromOrdering(1);
    }
};

const __wipple_runtime_make_empty_list = async () => {
    return [];
};

const __wipple_runtime_list_first = async (list) => {
    return __wipple_fromMaybe(list[0]);
};

const __wipple_runtime_list_last = async (list) => {
    return __wipple_fromMaybe(list[list.length - 1]);
};

const __wipple_runtime_list_initial = async (list) => {
    return __wipple_fromMaybe(list.length > 0 ? list.slice(0, -1) : undefined);
};

const __wipple_runtime_list_tail = async (list) => {
    return __wipple_fromMaybe(list.length > 0 ? list.slice(1) : undefined);
};

const __wipple_runtime_list_nth = async (list, index) => {
    return __wipple_fromMaybe(__wipple_isValidListIndex(index, list) ? list[index] : undefined);
};

const __wipple_runtime_list_append = async (list, element) => {
    return [...list, element];
};

const __wipple_runtime_list_prepend = async (element, list) => {
    return [element, ...list];
};

const __wipple_runtime_list_insert_at = async (list, index, element) => {
    return __wipple_fromMaybe(
        __wipple_isValidListIndex(index, list, true)
            ? [...list.slice(0, index), element, ...list.slice(index)]
            : undefined,
    );
};

const __wipple_runtime_list_remove_at = async (list, index) => {
    return __wipple_fromMaybe(
        __wipple_isValidListIndex(index, list)
            ? [...list.slice(0, index), ...list.slice(index + 1)]
            : undefined,
    );
};

const __wipple_runtime_list_count = async (list) => {
    return list.length;
};

const __wipple_runtime_list_slice = async (list, start, end) => {
    return __wipple_fromMaybe(
        __wipple_isValidListIndex(start, list) &&
            __wipple_isValidListIndex(end, list, true) &&
            start <= end
            ? list.slice(start, end)
            : undefined,
    );
};

const __wipple_runtime_string_characters = async (string) => {
    return string.split("");
};

const __wipple_runtime_random_number = async (min, max) => {
    return Math.random() * (max - min) + min;
};

const __wipple_runtime_nan = async () => {
    return NaN;
};

const __wipple_runtime_hash_string = async (string) => {
    // https://gist.github.com/jlevy/c246006675becc446360a798e2b2d781
    let hash = 0;
    for (const char of string) {
        hash = (hash << 5) - hash + char;
    }
    return hash >>> 0;
};
