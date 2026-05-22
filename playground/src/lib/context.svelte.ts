import type { DocumentationItem } from "./models/Documentation";
import type { Groups } from "./models/Groups";
import type { Playground } from "./models/Playground";
import type * as wipple from "wipple";
import { init as initCompiler } from "$lib/workers/compiler.worker";
import CompilerWorker from "$lib/workers/compiler.worker?worker";

export const context = $state({
    playground: undefined as Playground | undefined,
    documentation: {} as Record<string, DocumentationItem>,
    ideInfo: [] as Record<string, any>[],
    groups: [] as Groups,
    diagnostic: undefined as wipple.Diagnostic | undefined,
    runningLine: undefined as number | undefined,
});

export const compilerWorker = await initCompiler(new CompilerWorker());
