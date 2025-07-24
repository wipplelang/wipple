import type runtimes from "$lib/runtimes";
import type { Component } from "svelte";
import type { Command } from "./Command";

export interface Runtime {
    library: string;
    commands: Record<string, Record<string, Command>>;
    units?: Record<string, UnitInfo>;
    printEnabled?: boolean;
    Output?: Component;
}

export interface UnitInfo {
    presets: number[];
}

export const runtime = (runtime: Runtime) => runtime;

export type RuntimeId = keyof typeof runtimes;
