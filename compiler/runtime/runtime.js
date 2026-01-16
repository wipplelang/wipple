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

    console.log({ instances, types, substitutions });

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
    switch (ty.__wipple_type) {
        case "parameter": {
            return substitutions[ty.__wipple_name] ?? ty;
        }
        case "named": {
            return {
                __wipple_type: "named",
                __wipple_name: ty.__wipple_name,
                __wipple_parameters: ty.__wipple_parameters.map((param) =>
                    __wipple_substituteType(param, substitutions),
                ),
            };
        }

        case "function": {
            return {
                __wipple_type: "function",
                __wipple_inputs: ty.__wipple_inputs.map((input) =>
                    __wipple_substituteType(input, substitutions),
                ),
                __wipple_output: __wipple_substituteType(ty.__wipple_output, substitutions),
            };
        }

        case "tuple": {
            return {
                __wipple_type: "tuple",
                __wipple_elements: ty.__wipple_elements.map((element) =>
                    __wipple_substituteType(element, substitutions),
                ),
            };
        }

        case "block": {
            return {
                __wipple_type: "block",
                __wipple_output: __wipple_substituteType(ty.__wipple_output, substitutions),
            };
        }
        default: {
            return ty;
        }
    }
};

const __wipple_unify = (left, right, substitutions) => {
    if (left.__wipple_type === "parameter") {
        // This occurs when bounds are being resolved that involve type
        // parameters not mentioned in the item's type descriptor; no value's
        // type will ever contain a `parameter`
        return true;
    }

    if (right.__wipple_type === "parameter") {
        if (right.__wipple_name in substitutions) {
            return __wipple_unify(left, substitutions[right.__wipple_name], substitutions);
        }

        substitutions[right.__wipple_name] = left;
        return true;
    }

    if (left.__wipple_type === "function") {
        return (
            right.__wipple_type === "function" &&
            left.__wipple_inputs.length === right.__wipple_inputs.length &&
            left.__wipple_inputs.every((leftInput, index) =>
                __wipple_unify(leftInput, right.__wipple_inputs[index], substitutions),
            ) &&
            __wipple_unify(left.__wipple_output, right.__wipple_output, substitutions)
        );
    }

    if (left.__wipple_type === "named") {
        return (
            right.__wipple_type === "named" &&
            left.__wipple_name === right.__wipple_name &&
            left.__wipple_parameters.length === right.__wipple_parameters.length &&
            left.__wipple_parameters.every((leftParameter, index) =>
                __wipple_unify(leftParameter, right.__wipple_parameters[index], substitutions),
            )
        );
    }

    if (left.__wipple_type === "tuple") {
        return (
            right.__wipple_type === "tuple" &&
            left.__wipple_elements.length === right.__wipple_elements.length &&
            left.__wipple_elements.every((leftElement, index) =>
                __wipple_unify(leftElement, right.__wipple_elements[index], substitutions),
            )
        );
    }

    if (left.__wipple_type === "block") {
        return (
            right.__wipple_type === "block" &&
            __wipple_unify(left.__wipple_output, right.__wipple_output, substitutions)
        );
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
        return isNaN(number) ? __wipple_fromMaybe(undefined) : __wipple_fromMaybe(number);
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
