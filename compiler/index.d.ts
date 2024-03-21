import type { File, Interface, Result, linker_Executable } from "./generated/wipple-compiler.d.ts";

export const compile: (sources: File[], dependencies: Interface | null) => Result;
export const link: (libraries: UnlinkedLibrary[]) => linker_Executable;
export const format: (code: string) => string;

export type * from "./generated/wipple-compiler.d.ts";
