<script lang="ts">
    import type { Diagnostic } from "wipple";
    import Markdown from "./Markdown.svelte";
    import { slide } from "svelte/transition";
    import { LinesAndColumns } from "lines-and-columns";
    import { context } from "@/context.svelte";
    import { defaultFontSize, lineHeightRatio, lineSpacingRatio } from "./CodeEditor.svelte";
    import "./HoverLink.svelte";
    import Icon from "./Icon.svelte";
    import ToolbarButton from "./ToolbarButton.svelte";

    interface Props {
        diagnostic: Diagnostic;
    }

    let { diagnostic }: Props = $props();

    const width = 300;
    const fontSize = 14;

    const [primaryMessage, ...secondaryMessages] = $derived(diagnostic.message.split("\n\n"));
    const secondaryMessage = $derived(secondaryMessages.join("\n\n"));

    const cards = $derived.by(() => {
        const index = new LinesAndColumns(context.playground!.code);

        const cards = [
            {
                primary: true,
                position: diagnostic.locations[0].start,
                line: index.locationForIndex(diagnostic.locations[0].start)?.line ?? -1,
                message: primaryMessage,
                consequences: [secondaryMessage],
            },
            ...diagnostic.traces.map((trace) => ({
                primary: false,
                position: trace.location.start,
                line: index.locationForIndex(trace.location.start)?.line ?? -1,
                message: trace.message,
                consequences: trace.consequences,
            })),
        ].filter((card) => card.line !== -1);

        cards.sort((a, b) => {
            if (a.line === b.line) {
                if (a.primary && !b.primary) {
                    return -1;
                } else if (!a.primary && b.primary) {
                    return 1;
                } else {
                    return a.position - b.position;
                }
            } else {
                return a.line - b.line;
            }
        });

        return cards;
    });

    let background: HTMLDivElement;

    const layout = (node: HTMLDivElement) => {
        const lineHeight = defaultFontSize * lineHeightRatio;
        const lineSpacing = defaultFontSize * lineSpacingRatio;
        const padding = 10;

        let i = 0;
        let minTop = 0;
        for (const child of node.children) {
            if (!("card" in (child as HTMLElement).dataset)) continue;

            const card = cards[i];

            const lineOffset = card.line * (lineHeight + lineSpacing);
            const top = Math.max(lineOffset, minTop + padding);
            minTop = top + child.getBoundingClientRect().height;
            (child as HTMLElement).style.top = `${top}px`;

            i++;
        }

        background.style.minHeight = `${minTop}px`;
    };

    const primaryColor =
        "color-mix(in srgb, var(--color-blue-500) 5%, var(--color-background-secondary-alt) 95%)";

    let showAll = $state(false);
</script>

<div
    class="printing:hidden relative shrink-0"
    style:width="{width}px"
    style:min-width="{width}px"
    transition:slide={{ axis: "x" }}
>
    <div
        use:layout
        class="absolute inset-0 h-full max-w-full py-[10px]"
        style:width="{width}px"
        style:min-width="{width}px"
    >
        <div bind:this={background} class="bg-background-secondary/50 absolute inset-0"></div>

        {#each cards as card, index (index)}
            <div
                data-card
                data-hidden={!card.primary && !showAll ? true : undefined}
                data-primary={card.primary || undefined}
                class="border-standard bg-background-secondary-alt absolute inset-x-[10px] flex flex-col gap-[4px] overflow-scroll rounded-[10px] p-[10px] transition duration-250 data-hidden:opacity-0 data-primary:border-blue-500/50 data-primary:bg-(--primary-color)"
                style:--primary-color={primaryColor}
                style:font-size="{fontSize}px"
            >
                <div class="font-semibold">
                    {#if card.primary}
                        <p
                            class="mb-[4px] flex flex-row items-center gap-[5px] font-semibold text-blue-500"
                        >
                            <Icon>error</Icon>

                            Error
                        </p>
                    {/if}

                    <Markdown content={card.message} {fontSize} highlightGroups />
                </div>

                {#each card.consequences as consequence, index (index)}
                    <Markdown content={consequence} {fontSize} highlightGroups />
                {/each}

                {#if card.primary && cards.length > 1}
                    <ToolbarButton
                        data-active={showAll || undefined}
                        class="text-background-button -mx-[4px] -my-[2px] self-start bg-transparent px-[4px] data-[active]:font-bold"
                        onclick={() => (showAll = !showAll)}
                    >
                        Details
                    </ToolbarButton>
                {/if}
            </div>
        {/each}
    </div>
</div>
