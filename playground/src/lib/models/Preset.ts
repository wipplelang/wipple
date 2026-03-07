import type { RuntimeId } from "$lib/runtimes";

export interface Preset {
    icon: string;
    name: string;
    description: string;
    runtime: RuntimeId;
}

export const preset = (preset: Preset) => preset;
