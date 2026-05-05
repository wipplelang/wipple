import compiler, { type CompileResult, type File } from "compiler";
import * as z from "zod";

export const InputMetadata = z.object({
    library: z.string().optional(),
});

export const LibraryMetadata = z.object({
    library: z.string().optional(),
    ide: z.any(),
});

export interface Library {
    metadata: z.infer<typeof LibraryMetadata>;
    files: File[];
}

import foundationLibrary from "../../library/dist/foundation.json";
import mathLibrary from "../../library/dist/math.json";
import musicLibrary from "../../library/dist/music.json";
import turtleLibrary from "../../library/dist/turtle.json";

export const libraries: Record<string, Library> = {
    foundation: foundationLibrary,
    math: mathLibrary,
    music: musicLibrary,
    turtle: turtleLibrary,
};

const loadedLibraries = new Map<string, CompileResult | undefined>();

export const loadLibrary = (name: string) => {
    if (!(name in libraries)) {
        return undefined;
    }

    if (loadedLibraries.has(name)) {
        return loadedLibraries.get(name)!;
    }

    console.log(`loading library '${name}'`);

    const library = libraries[name];

    if (library.metadata.library != null) {
        if (loadLibrary(library.metadata.library) == null) {
            loadedLibraries.set(name, undefined);
            return undefined;
        }
    }

    const result = compiler.compile(library.files, library.metadata.library) ?? undefined;

    loadedLibraries.set(name, result);

    if (result == null) {
        console.error(`failed to compile library '${name}'`);
        return undefined;
    }

    compiler.registerLibrary(name, result);

    return result;
};

// @ts-ignore
import runtime from "inline:../../library/src/runtime.js";

export { runtime };
