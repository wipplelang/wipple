import * as wasm from "./pkg/wipple_compiler.js";
import type { linker, main } from "./generated/wipple-compiler.d.ts";

export const compile = (
    sources: main.File[],
    dependencies: linker.UnlinkedLibrary[],
): main.Result => JSON.parse(wasm.compile(JSON.stringify(sources), JSON.stringify(dependencies)));

export const link = (libraries: linker.UnlinkedLibrary[]) =>
    JSON.parse(wasm.link(JSON.stringify(libraries)));

export type * from "./generated/wipple-compiler.d.ts";
