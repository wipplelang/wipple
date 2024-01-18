import * as wasm from "./pkg/wipple_compiler.js";

export const compile = (sources: any[], dependencies: any[]) =>
    JSON.parse(wasm.compile(JSON.stringify(sources), JSON.stringify(dependencies)));

export const link = (libraries: any[]) => JSON.parse(wasm.link(JSON.stringify(libraries)));

export const renderErrors = (errors: any[]) =>
    JSON.parse(wasm.renderErrors(JSON.stringify(errors)));

export const colorizeErrors = (errors: any[], sourceCodeForFile: (file: string) => string) =>
    JSON.parse(wasm.colorizeErrors(JSON.stringify(errors), sourceCodeForFile));
