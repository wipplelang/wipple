import init from "./main/Cargo.toml";

const wasm = init();

export const compile = (sources, dependencies, entrypoint) =>
    JSON.parse(wasm.compile(JSON.stringify(sources), JSON.stringify(dependencies), entrypoint));

export const link = (libraries) => {
    const result = wasm.link(JSON.stringify(libraries));
    if (result == null) {
        return null;
    }

    return JSON.parse(result);
};

export const format = (code) => wasm.format(code);

export const listTypeParameters = (type) =>
    JSON.parse(wasm.list_type_parameters(JSON.stringify(type)));

export const resolveAttributeLikeTrait = (name, type, numberOfParameters, interface_) => {
    const result = wasm.resolve_attribute_like_trait(
        name,
        JSON.stringify(type),
        numberOfParameters,
        JSON.stringify(interface_),
    );

    if (result == null) {
        return null;
    }

    return JSON.parse(result);
};
