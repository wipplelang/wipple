import type { linker, main } from "./generated/wipple-compiler.d.ts";

export const compile: (
    sources: main.File[],
    dependencies: linker.UnlinkedLibrary | null,
) => main.Result;

export const link: (libraries: linker.UnlinkedLibrary[]) => linker.Executable;

export type * from "./generated/wipple-compiler.d.ts";
