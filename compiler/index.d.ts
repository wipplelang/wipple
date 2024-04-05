import type * as compiler from "./generated/wipple-compiler.d.ts";

export const compile: (
    sources: compiler.File[],
    dependencies: compiler.Interface | null,
) => compiler.Result;

export const link: (libraries: compiler.UnlinkedLibrary[]) => compiler.linker_Executable;

export const format: (code: string) => string;

export const listTypeParameters: (
    type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
) => compiler.lower_Path[];

export const typeDescription: (
    type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
    interface: compiler.Interface,
) => compiler.typecheck_MessageType;

export type * from "./generated/wipple-compiler.d.ts";
