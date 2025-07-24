import type { RuntimeId } from "$lib/runtimes";

export interface Preset {
    name: string;
    icon: string;
    backgroundClassName: string;
    runtime: RuntimeId;
}

export const preset = (preset: Preset) => preset;
