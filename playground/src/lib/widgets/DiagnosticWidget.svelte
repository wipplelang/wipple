<svelte:options
    customElement={{
        tag: "wipple-diagnostic-widget",
        shadow: "none",
    }}
/>

<script lang="ts">
    import Box from "$lib/components/Box.svelte";
    import CodeEditor from "$lib/components/CodeEditor.svelte";
    import Icon from "$lib/components/Icon.svelte";
    import Markdown from "$lib/components/Markdown.svelte";
    import ToolbarButton from "$lib/components/ToolbarButton.svelte";
    import { scale } from "svelte/transition";
    import { context } from "$lib/context.svelte";
    import { PUBLIC_VISUALIZER_URL } from "$env/static/public";

    interface Props {
        animate: boolean;
        onclose?: () => void;
    }

    let { animate, onclose }: Props = $props();

    const [title, ...extra] = context.diagnostic.message.split("\n\n");

    let showExtra = $state<"diagnostic" | "visualizer">();

    const transition = animate ? scale : () => ({});

    const visualizationUrl = $derived.by(() => {
        if (!context.diagnostic?.graph) {
            return undefined;
        }

        const base = PUBLIC_VISUALIZER_URL;
        if (!base) {
            return undefined;
        }

        const url = new URL(base);
        url.searchParams.set("embed", "1");
        return url.toString();
    });

    let visualizationIFrame = $state<HTMLIFrameElement>();

    $effect(() => {
        if (visualizationIFrame == null) {
            return;
        }

        visualizationIFrame.addEventListener("load", (event) => {
            const visualizationWindow = (event.target as HTMLIFrameElement).contentWindow!;

            window.addEventListener("message", (event) => {
                if (event.source !== visualizationWindow) {
                    return;
                }

                if (event.data === "requestEmbed") {
                    visualizationWindow.postMessage(
                        { embed: $state.snapshot(context.diagnostic.graph) },
                        "*",
                    );
                }
            });
        });
    });
</script>

{#snippet lines(lines: any[])}
    <div class="mt-[15px] mb-[5px] flex flex-col gap-[10px]">
        {#each lines as line}
            <div class="rounded-xl border-[1.5px] border-black/5 dark:bg-gray-800">
                <CodeEditor
                    readOnly
                    code={line.source}
                    diagnostic={{
                        value: { locations: line.locations },
                        hideWidget: true,
                    }}
                />
            </div>
        {/each}
    </div>
{/snippet}

<div
    class="relative flex h-full w-full items-stretch justify-stretch pb-[10px]"
    in:transition={{ start: 0.95, opacity: 0.5 }}
>
    <Box class="flex flex-1 flex-col px-[10px] py-[8px] font-sans shadow-blue-500/10 transition">
        <div class="scroll flex flex-col gap-[2px]">
            <div class="font-semibold">
                <Markdown content={title} highlightGroups />
            </div>

            {#if extra.length === 1}
                <Markdown content={extra[0]} highlightGroups={false} />
            {/if}

            <div class="flex flex-row items-center gap-[10px]">
                {#snippet button(title: string, kind: NonNullable<typeof showExtra>)}
                    <ToolbarButton
                        data-active={showExtra === kind || undefined}
                        class="text-background-button -mx-[4px] self-start bg-transparent px-[4px] data-[active]:font-bold"
                        onclick={() => {
                            if (showExtra === kind) {
                                showExtra = undefined;
                            } else {
                                showExtra = kind;
                            }
                        }}
                    >
                        {title}
                    </ToolbarButton>
                {/snippet}

                {#if extra.length > 1}
                    {@render button("Details", "diagnostic")}
                {/if}

                {#if extra.length > 1 && visualizationUrl != null}
                    <div class="h-[1.25em] rounded-full border-[1px] border-gray-500/40"></div>
                {/if}

                {#if visualizationUrl != null}
                    {@render button("Visualize", "visualizer")}
                {/if}
            </div>

            {#if showExtra === "diagnostic"}
                <div class="mt-[5px]">
                    <Markdown content={extra.join("\n\n")} highlightGroups />
                </div>

                {#if context.diagnostic.lines != null && context.diagnostic.lines.length > 0}
                    {@render lines(context.diagnostic.lines)}
                {/if}

                {#if context.diagnostic.groups > 1}
                    <p class="mx-[4px] my-[8px] text-sm opacity-75">
                        <strong>Tip:</strong>
                        Hover over an
                        <span class="diagnostic diagnostic-dimmed">outlined</span>
                        piece of code to highlight related code.
                    </p>
                {/if}
            {/if}

            {#if showExtra === "visualizer" && visualizationUrl != null}
                <iframe
                    bind:this={visualizationIFrame}
                    title="Visualization"
                    class="aspect-[3/2] flex-1 rounded-xl border-[1.5px] border-black/5 dark:bg-gray-800"
                    src={visualizationUrl}
                ></iframe>
            {/if}
        </div>
    </Box>

    <div class="absolute top-[10px] right-[10px]">
        <ToolbarButton onclick={onclose} square>
            <Icon fill>close</Icon>
        </ToolbarButton>
    </div>
</div>
