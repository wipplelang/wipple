import * as wasm from "./pkg/wipple_compiler.js";

export const compile = (sources: any[], dependencies: any) =>
    JSON.parse(wasm.compile(JSON.stringify(sources), JSON.stringify(dependencies)));

export const link = (libraries: any[]) => JSON.parse(wasm.link(JSON.stringify(libraries)));

export const renderDiagnostics = (
    diagnostics: any[],
    interface_: any,
    library: any,
    sourceCodeForFile: (file: string) => string
) =>
    JSON.parse(
        wasm.renderDiagnostics(
            JSON.stringify(diagnostics),
            JSON.stringify(interface_),
            JSON.stringify(library),
            sourceCodeForFile
        )
    );

export const colorizeDiagnostics = (
    diagnostics: any[],
    sourceCodeForFile: (file: string) => string
) => JSON.parse(wasm.colorizeDiagnostics(JSON.stringify(diagnostics), sourceCodeForFile));
