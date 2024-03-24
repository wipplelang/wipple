import type * as compiler from "./generated/wipple-compiler.d.ts";

export const compile: (
    sources: compiler.File[],
    dependencies: compiler.Interface | null,
) => compiler.Result;

export const link: (libraries: compiler.UnlinkedLibrary[]) => compiler.linker_Executable;

export const format: (code: string) => string;

export const parseType: (
    code: string,
) => compiler.WithInfo<compiler.Info, compiler.syntax_Type> | null;

export const parsedTypeFromCompiled: (
    type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
) => compiler.WithInfo<compiler.Info, compiler.syntax_Type>;

export const parsedTypesAreEqual: (
    left: compiler.WithInfo<compiler.Info, compiler.syntax_Type>,
    right: compiler.WithInfo<compiler.Info, compiler.syntax_Type>,
) => boolean;

export type * from "./generated/wipple-compiler.d.ts";
