import type { DocumentationItem } from "./models/Documentation";
import type { Groups } from "./models/Groups";
import type { Playground } from "./models/Playground";
import type * as wipple from "wipple";
import { init as initCompiler } from "@/workers/compiler.worker";
import CompilerWorker from "@/workers/compiler.worker?worker";
import { touchSupported } from "./util";

export const context = $state({
    playground: undefined as Playground | undefined,
    documentation: {} as Record<string, DocumentationItem>,
    ideInfo: [] as Record<string, any>[],
    groups: {} as Groups,
    highlightedGroup: undefined as string | undefined,
    diagnostic: undefined as wipple.Diagnostic | undefined,
    graph: undefined as wipple.Graph | undefined,
    runningLine: undefined as number | undefined,
    touchModeEnabled: touchSupported(),
});

export const compilerWorker = await initCompiler(new CompilerWorker());
