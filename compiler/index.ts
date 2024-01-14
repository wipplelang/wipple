import * as wasm from "./pkg/wipple_compiler.js";

export const compile = (sources: any[], dependencies: any[]) =>
    JSON.parse(wasm.compile(JSON.stringify(sources), JSON.stringify(dependencies)));

export const link = (libraries: any[]) => JSON.parse(wasm.link(JSON.stringify(libraries)));
