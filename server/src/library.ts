import * as compiler from "compiler";
import { readFileSync } from "node:fs";
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
    docs: Record<string, any>;
}

import foundationLibrary from "../../library/dist/foundation.json";
const foundationBin = readFileSync(new URL("../../library/dist/foundation.bin", import.meta.url));

import mathLibrary from "../../library/dist/math.json";
const mathBin = readFileSync(new URL("../../library/dist/math.bin", import.meta.url));

import musicLibrary from "../../library/dist/music.json";
const musicBin = readFileSync(new URL("../../library/dist/music.bin", import.meta.url));

import turtleLibrary from "../../library/dist/turtle.json";
const turtleBin = readFileSync(new URL("../../library/dist/turtle.bin", import.meta.url));

export const libraries: Record<string, [Library, Uint8Array]> = {
    foundation: [foundationLibrary, foundationBin],
    math: [mathLibrary, mathBin],
    music: [musicLibrary, musicBin],
    turtle: [turtleLibrary, turtleBin],
};

const loadedLibraries = new Set<string>();

export const loadLibrary = (name: string) => {
    if (!(name in libraries)) {
        return undefined;
    }

    const [library, bin] = libraries[name];

    if (library.metadata.library != null) {
        loadLibrary(library.metadata.library);
    }

    if (!loadedLibraries.has(name)) {
        compiler.register_library(name, bin);
        loadedLibraries.add(name);
    }
};

// @ts-ignore
import runtime from "inline:../../library/src/runtime.js";

export { runtime };
