import type { Playground } from "./models/Playground";

export const context = $state({
    playground: undefined as Playground | undefined,
    ideInfo: [] as Record<string, any>[],
    diagnostic: undefined as any,
    runningLine: undefined as number | undefined,
});
