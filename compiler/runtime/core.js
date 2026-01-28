const __wipple_variant = (index, values) => {
    values[__wipple_variant] = index;
    return values;
};

__wipple_variant.toString = () => "<variant>";

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
