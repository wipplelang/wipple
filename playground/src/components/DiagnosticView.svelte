<script lang="ts">
    import type { Diagnostic, DiagnosticLocation } from "wipple";
    import Markdown from "./Markdown.svelte";
    import { LinesAndColumns } from "lines-and-columns";
    import { context } from "@/context.svelte";
    import "./HoverLink.svelte";
    import Icon from "./Icon.svelte";
    import CodeEditor from "./CodeEditor.svelte";
    import Box from "./Box.svelte";

    interface Props {
        diagnostic: Diagnostic;
    }

    let { diagnostic }: Props = $props();

    const [primaryMessage, ...secondaryMessages] = $derived(diagnostic.message.split("\n\n"));
    const secondaryMessage = $derived(secondaryMessages.join("\n\n"));

    const cards = $derived.by(() => {
        const { code } = context.playground!;

        const index = new LinesAndColumns(code);

        const getLineSpan = (location: DiagnosticLocation) => {
            const start = index.locationForIndex(location.start);
            const end = index.locationForIndex(location.end);

            if (start == null || end == null || start.line !== end.line) {
                return undefined;
            }

            const lineStart = index.indexForLocation({ line: start.line, column: 0 })!;
            const lineEnd =
                index.indexForLocation({ line: start.line + 1, column: 0 }) ?? code.length;

            return {
                startIndex: location.start,
                endIndex: location.end,
                line: start.line,
                startColumn: start.column,
                endColumn: end.column,
                code: code.slice(lineStart, lineEnd).trimEnd(),
            };
        };

        const primaryCard = {
            primary: true,
            span: getLineSpan(diagnostic.locations[0])!,
            message: primaryMessage,
            consequences: [secondaryMessage],
        };

        if (primaryCard.span == null) {
            return [];
        }

        const secondaryCards = diagnostic.traces
            .map((trace) => ({
                primary: false,
                span: getLineSpan(trace.location)!,
                message: trace.message,
                consequences: trace.consequences,
            }))
            .filter((card) => card.span != null);

        secondaryCards.sort((a, b) => {
            if (a.span.line === b.span.line) {
                if (a.primary && !b.primary) {
                    return -1;
                } else if (!a.primary && b.primary) {
                    return 1;
                } else {
                    return a.span.startColumn - b.span.startColumn;
                }
            } else {
                return a.span.line - b.span.line;
            }
        });

        return [...secondaryCards, primaryCard];
    });
</script>

<div class="flex flex-col">
    {#each cards as card, index (index)}
        <div class="flex flex-row gap-[10px]">
            <div class="flex flex-col items-center">
                <div
                    class="bg-background-secondary flex size-[32px] items-center justify-center rounded-full"
                >
                    <p>{index + 1}</p>
                </div>

                {#if index < cards.length - 1}
                    <div class="bg-background-secondary w-[3px] flex-1"></div>
                {/if}
            </div>

            <Box
                scroll={false}
                class="mb-[10px] flex-1 flex-col p-[10px] data-primary:border-blue-500 data-primary:bg-blue-500/5"
                data-primary={card.primary || undefined}
            >
                {#if card.primary}
                    <p
                        class="mb-[4px] flex flex-row items-center gap-[5px] font-semibold text-blue-500"
                    >
                        <Icon>error</Icon>

                        Error
                    </p>
                {/if}

                <div
                    class="bg-background border-standard mb-[10px] rounded-[10px] px-[10px] py-[8px]"
                >
                    <CodeEditor
                        readOnly
                        code={card.span.code}
                        groups={{
                            diagnostic: {
                                locations: [
                                    {
                                        start: card.span.startColumn,
                                        end: card.span.endColumn,
                                        primary: true,
                                    },
                                ],
                            },
                        }}
                    />
                </div>

                <div class="font-semibold">
                    <Markdown content={card.message} />
                </div>

                {#each card.consequences as consequence, index (index)}
                    <Markdown content={consequence} />
                {/each}
            </Box>
        </div>
    {/each}
</div>
