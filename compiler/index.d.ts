import type { linker, main } from "./generated/wipple-compiler.d.ts";

export const compile: (sources: main.File[], dependencies: main.Interface | null) => main.Result;

export const link: (libraries: main.UnlinkedLibrary[]) => linker.Executable;

export type * from "./generated/wipple-compiler.d.ts";
