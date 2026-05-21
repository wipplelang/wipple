import type { PlaygroundMetadata } from "$lib/models/Playground";
import * as compiler from "compiler";
import { Mutex } from "async-mutex";

interface LibraryMetadata {
    library?: string;
    ide: any;
}

export interface Library {
    metadata: LibraryMetadata;
    docs: Record<string, any>;
}

const loadedLibraries = new Map<string, [Library, Uint8Array]>();

const library = (name: string) =>
    Promise.all([
        import(`../../../../library/dist/${name}.json`).then(({ default: library }) => library),
        import(`../../../../library/dist/${name}.bin?url`)
            .then(({ default: url }) => fetch(url))
            .then((response) => response.arrayBuffer())
            .then((bin) => new Uint8Array(bin)),
    ]);

const libraries: Record<string, () => Promise<[Library, Uint8Array]>> = {
    foundation: () => library("foundation"),
    math: () => library("math"),
    music: () => library("music"),
    turtle: () => library("turtle"),
};

const loadLibrary = async (options: { name: string }) => {
    if (!(options.name in libraries)) {
        return undefined;
    }

    if (loadedLibraries.has(options.name)) {
        return;
    }

    const [library, bin] = await libraries[options.name]();

    if (library.metadata.library != null) {
        await loadLibrary({ name: library.metadata.library });
    }

    if (!loadedLibraries.has(options.name)) {
        compiler.register_library(options.name, bin);
        loadedLibraries.set(options.name, [library, bin]);
    }
};

const ideInfo = (options: PlaygroundMetadata) => {
    const info: any[] = [];

    let libraryName: string | undefined = options.library;
    while (libraryName != null) {
        const library: Library | undefined = loadedLibraries.get(libraryName)?.[0];
        if (library == null) {
            throw new Error("unknown library");
        }

        info.push(library.metadata.ide);

        libraryName = library.metadata.library;
    }

    return { info };
};

const compile = (
    options: PlaygroundMetadata & { code: string; groups?: boolean; graph?: boolean },
) => {
    using result = compiler.compile([new compiler.File("input", options.code)], options.library);
    if (result == null) {
        throw new Error("compilation failed");
    }

    const graph = options.graph ? result.graph() : undefined;

    const diagnostics = result.diagnostics();
    if (diagnostics != null) {
        return { graph, diagnostics };
    }

    const groups = options.groups ? result.groups() : undefined;

    const executableBase64 = result.executable();
    if (executableBase64 == null) {
        throw new Error("missing executable");
    }

    return { groups, graph, executable: executableBase64 };
};

const documentation = (options: PlaygroundMetadata) => {
    const library = loadedLibraries.get(options.library)?.[0];
    if (library == null) {
        throw new Error("unknown library");
    }

    return { items: library.docs };
};

const format = (options: { code: string }) => {
    const formatted = compiler.format(options.code);
    return { code: formatted };
};

const methods = { loadLibrary, ideInfo, compile, documentation, format };

export type CompilerWorkerMethods = {
    [K in keyof typeof methods]: (
        ...options: Parameters<(typeof methods)[K]>
    ) => Promise<ReturnType<(typeof methods)[K]>>;
};

export type CompilerWorkerResponse<K extends keyof CompilerWorkerMethods> = Awaited<
    ReturnType<CompilerWorkerMethods[K]>
>;

export const init = async (worker: Worker) => {
    await new Promise<void>((resolve) => {
        worker.onmessage = (e) => {
            if (e.data === "ready") {
                resolve();
            }
        };
    });

    const mutex = new Mutex();

    return Object.fromEntries(
        Object.keys(methods).map((name) => [
            name,
            (options: any) =>
                mutex.runExclusive(() => {
                    return new Promise<any>((resolve) => {
                        worker.onmessage = (e) => {
                            resolve(e.data);
                        };

                        worker.postMessage({ [name]: options });
                    });
                }),
        ]),
    ) as CompilerWorkerMethods;
};

if (typeof WorkerGlobalScope !== "undefined" && self instanceof WorkerGlobalScope) {
    onmessage = async (e) => {
        const method = Object.keys(e.data)[0] as keyof typeof methods;

        if (import.meta.env.DEV) {
            console.log(`compiler(${method}) request:`, e.data[method]);
        }

        let result = await methods[method](e.data[method]);
        if (result != null) {
            // Internally calls `toJSON`, which exposes the Wasm object properties
            result = JSON.parse(JSON.stringify(result));
        }

        if (import.meta.env.DEV) {
            console.log(`compiler(${method}) response:`, result);
        }

        postMessage(result);
    };

    postMessage("ready");
}
