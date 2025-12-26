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

    interface Props {
        stale: boolean;
        animate: boolean;
        ontoggleshowextra: (value: boolean) => void;
        onclose: () => void;
    }

    let { stale, animate, ontoggleshowextra, onclose }: Props = $props();

    const [title, ...extra] = context.diagnostic.message.split("\n\n");

    let showExtra = $state(false);

    const transition = animate ? scale : () => ({});
</script>

{#snippet lines(lines: any[])}
    <div class="mb-[5px] mt-[15px] flex flex-col gap-[10px]">
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
    <Box
        data-stale={stale || undefined}
        class="not-[[data-stale]]:border-blue-500 not-[[data-stale]]:shadow-md flex flex-1 flex-col px-[10px] py-[8px] font-sans shadow-blue-500/10 transition data-[stale]:opacity-60"
    >
        <div class="scroll flex flex-col gap-[2px]">
            <div class="font-semibold">
                <Markdown content={title} highlightGroups={showExtra} />
            </div>

            {#if extra.length === 1}
                <Markdown content={extra[0]} highlightGroups={false} />
            {/if}

            {#if extra.length > 1}
                <ToolbarButton
                    class="text-background-button -mx-[4px] self-start bg-transparent px-[4px]"
                    onclick={() => {
                        showExtra = !showExtra;
                        ontoggleshowextra(showExtra);
                    }}
                >
                    {#if showExtra}
                        Hide details&zwj;<Icon>expand_less</Icon>
                    {:else}
                        Show details&zwj;<Icon>expand_more</Icon>
                    {/if}
                </ToolbarButton>

                {#if showExtra}
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
            {/if}
        </div>
    </Box>

    <div class="absolute right-[10px] top-[10px]">
        <ToolbarButton onclick={onclose} square>
            <Icon fill>close</Icon>
        </ToolbarButton>
    </div>
</div>
