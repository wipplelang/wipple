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
