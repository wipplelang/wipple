const __wipple_runtime_debug = (value) => {
    __wipple_env.debug?.(value);
    return value;
};

const __wipple_runtime_crash = (message) => {
    throw new Error(message);
};

const __wipple_runtime_display = (message) => {
    __wipple_env.display(message);
    return [];
};

const __wipple_runtime_prompt = (message, validate) => {
    let input = __wipple_env.prompt(message);

    let validated;
    do {
        validated = __wipple_toMaybe(validate(input));
        input = __wipple_env.validatePrompt(validated !== undefined);
    } while (validated === undefined);

    return validated;
};

const __wipple_runtime_external = (func, input) => {
    return __wipple_env[func](input);
};

const __wipple_runtime_number_to_string = (number) => {
    return number.toString();
};

const __wipple_runtime_string_to_number = (string) => {
    if (string.toLowerCase() === "nan") {
        return __wipple_fromMaybe(NaN);
    } else {
        const number = parseFloat(string);
        return isNaN(number) ? __wipple_fromMaybe(undefined) : __wipple_fromMaybe(number);
    }
};

const __wipple_runtime_add_numbers = (left, right) => {
    return left + right;
};

const __wipple_runtime_subtract_numbers = (left, right) => {
    return left - right;
};

const __wipple_runtime_multiply_numbers = (left, right) => {
    return left * right;
};

const __wipple_runtime_divide_numbers = (left, right) => {
    return left / right;
};

const __wipple_runtime_remainder_numbers = (left, right) => {
    return left % right;
};

const __wipple_runtime_power_numbers = (left, right) => {
    return Math.pow(left, right);
};

const __wipple_runtime_floor_number = (number) => {
    return Math.floor(number);
};

const __wipple_runtime_ceiling_number = (number) => {
    return Math.ceil(number);
};

const __wipple_runtime_sqrt_number = (number) => {
    return Math.sqrt(number);
};

const __wipple_runtime_sin = (number) => {
    return Math.sin(number);
};

const __wipple_runtime_cos = (number) => {
    return Math.cos(number);
};

const __wipple_runtime_tan = (number) => {
    return Math.tan(number);
};

const __wipple_runtime_negate_number = (number) => {
    return -number;
};

const __wipple_runtime_string_equality = (left, right) => {
    return __wipple_fromBoolean(left === right);
};

const __wipple_runtime_number_equality = (left, right) => {
    return __wipple_fromBoolean(left === right);
};

const __wipple_runtime_string_ordering = (left, right) => {
    if (left < right) {
        return __wipple_fromOrdering(-1);
    } else if (left === right) {
        return __wipple_fromOrdering(0);
    } else {
        return __wipple_fromOrdering(1);
    }
};

const __wipple_runtime_number_ordering = (left, right) => {
    if (left < right) {
        return __wipple_fromOrdering(-1);
    } else if (left === right) {
        return __wipple_fromOrdering(0);
    } else {
        return __wipple_fromOrdering(1);
    }
};

const __wipple_runtime_make_empty_list = () => {
    return [];
};

const __wipple_runtime_list_first = (list) => {
    return __wipple_fromMaybe(list[0]);
};

const __wipple_runtime_list_last = (list) => {
    return __wipple_fromMaybe(list[list.length - 1]);
};

const __wipple_runtime_list_initial = (list) => {
    return __wipple_fromMaybe(list.length > 0 ? list.slice(0, -1) : undefined);
};

const __wipple_runtime_list_tail = (list) => {
    return __wipple_fromMaybe(list.length > 0 ? list.slice(1) : undefined);
};

const __wipple_runtime_list_nth = (list, index) => {
    return __wipple_fromMaybe(__wipple_isValidListIndex(index, list) ? list[index] : undefined);
};

const __wipple_runtime_list_append = (list, element) => {
    return [...list, element];
};

const __wipple_runtime_list_prepend = (element, list) => {
    return [element, ...list];
};

const __wipple_runtime_list_insert_at = (list, index, element) => {
    return __wipple_fromMaybe(
        __wipple_isValidListIndex(index, list, true)
            ? [...list.slice(0, index), element, ...list.slice(index)]
            : undefined,
    );
};

const __wipple_runtime_list_remove_at = (list, index) => {
    return __wipple_fromMaybe(
        __wipple_isValidListIndex(index, list)
            ? [...list.slice(0, index), ...list.slice(index + 1)]
            : undefined,
    );
};

const __wipple_runtime_list_count = (list) => {
    return list.length;
};

const __wipple_runtime_list_slice = (list, start, end) => {
    return __wipple_fromMaybe(
        __wipple_isValidListIndex(start, list) &&
            __wipple_isValidListIndex(end, list, true) &&
            start <= end
            ? list.slice(start, end)
            : undefined,
    );
};

const __wipple_runtime_string_characters = (string) => {
    return string.split("");
};

const __wipple_runtime_random_number = (min, max) => {
    return Math.random() * (max - min) + min;
};

const __wipple_runtime_nan = () => {
    return NaN;
};

const __wipple_runtime_hash_string = (string) => {
    // https://gist.github.com/jlevy/c246006675becc446360a798e2b2d781
    let hash = 0;
    for (const char of string) {
        hash = (hash << 5) - hash + char;
    }
    return hash >>> 0;
};

const __wipple_runtime_number_as_external = (number) => {
    return number;
};

const __wipple_runtime_string_as_external = (string) => {
    return string;
};

const __wipple_runtime_unit_as_external = () => {
    return [];
};

const __wipple_runtime_tuple_2_as_external = (a, b) => {
    return [a, b];
};

const __wipple_runtime_tuple_3_as_external = (a, b, c) => {
    return [a, b, c];
};

const __wipple_runtime_number_from_external = (value) => {
    return value;
};

const __wipple_runtime_string_from_external = (value) => {
    return value;
};

const __wipple_runtime_tuple_2_from_external = (value) => {
    return value;
};

const __wipple_runtime_tuple_3_from_external = (value) => {
    return value;
};
