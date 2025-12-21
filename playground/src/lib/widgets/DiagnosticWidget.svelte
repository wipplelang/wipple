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

    interface Props {
        diagnostic: any;
        stale: boolean;
        onclose: () => void;
    }

    let { diagnostic, stale, onclose }: Props = $props();

    const [title, description, ...extra] = diagnostic.message.split("\n\n");

    let showExtra = $state(false);
</script>

<div class="relative flex h-full w-full items-stretch justify-stretch pb-[10px]">
    <Box
        data-stale={stale || undefined}
        class="not-[[data-stale]]:border-blue-500 not-[[data-stale]]:shadow-md flex flex-1 flex-col px-[10px] py-[8px] font-sans shadow-blue-500/10 transition data-[stale]:opacity-60"
    >
        <div class="scroll flex flex-col gap-[2px]">
            <div class="font-semibold">
                <Markdown content={title} />
            </div>

            {#if description}
                <Markdown content={description} />
            {/if}

            {#if diagnostic.lines != null && diagnostic.lines.length > 0}
                <div class="mb-[5px] mt-[15px] flex flex-col gap-[10px]">
                    {#each diagnostic.lines as line}
                        <div class="rounded-xl border-[1.5px] border-black/5">
                            <CodeEditor
                                readOnly
                                code={line.source}
                                diagnostic={{
                                    value: {
                                        locations: [
                                            {
                                                start: { index: line.start },
                                                end: { index: line.end },
                                            },
                                        ],
                                    },
                                    hideWidget: true,
                                }}
                            />
                        </div>
                    {/each}
                </div>
            {/if}

            {#if extra.length > 0}
                <ToolbarButton
                    class="text-background-button -mx-[4px] self-start bg-transparent px-[4px]"
                    onclick={() => (showExtra = !showExtra)}
                >
                    {#if showExtra}
                        Hide details&zwj;<Icon>expand_less</Icon>
                    {:else}
                        Show details&zwj;<Icon>expand_more</Icon>
                    {/if}
                </ToolbarButton>

                {#if showExtra}
                    <Markdown content={extra.join("\n\n")} />
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
