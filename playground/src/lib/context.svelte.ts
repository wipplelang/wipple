import type { DocumentationItem } from "./models/Documentation";
import type { Groups } from "./models/Groups";
import type { Playground } from "./models/Playground";
import type * as compiler from "compiler";

export const context = $state({
    playground: undefined as Playground | undefined,
    documentation: {} as Record<string, DocumentationItem>,
    ideInfo: [] as Record<string, any>[],
    groups: [] as Groups,
    diagnostic: undefined as compiler.Diagnostic | undefined,
    runningLine: undefined as number | undefined,
});
