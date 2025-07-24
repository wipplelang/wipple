import runtimes from "$lib/runtimes";
import type { RuntimeId } from "./Runtime";

export interface Playground {
    runtime: RuntimeId;
    code: string;
}

export interface PlaygroundMetadata {
    library: string;
}

export const playgroundMetadata = (playground: Playground): PlaygroundMetadata => ({
    library: runtimes[playground.runtime].library,
});
