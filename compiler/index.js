import init from "./main/Cargo.toml";

const wasm = init();

export const compile = (sources, dependencies) =>
    JSON.parse(wasm.compile(JSON.stringify(sources), JSON.stringify(dependencies)));

export const link = (libraries) => {
    const result = wasm.link(JSON.stringify(libraries));
    if (result == null) {
        return null;
    }

    return JSON.parse(result);
};

export const format = (code) => wasm.format(code);

export const parseType = (code) => {
    const result = wasm.parse_type(code);
    if (result == null) {
        return null;
    }

    return JSON.parse(result);
};

export const parsedTypeFromCompiled = (type) =>
    JSON.parse(wasm.parsed_type_from_compiled(JSON.stringify(type)));

export const parsedTypesAreEqual = (left, right) =>
    wasm.parsed_types_are_equal(JSON.stringify(left), JSON.stringify(right));
