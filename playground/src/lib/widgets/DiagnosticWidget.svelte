<svelte:options
    customElement={{
        tag: "wipple-diagnostic-widget",
        shadow: "none",
    }}
/>

<script lang="ts">
    import Box from "$lib/components/Box.svelte";
    import Icon from "$lib/components/Icon.svelte";
    import Markdown from "$lib/components/Markdown.svelte";
    import ToolbarButton from "$lib/components/ToolbarButton.svelte";
    import { scale } from "svelte/transition";
    import DiagnosticVisualizer from "$lib/components/DiagnosticVisualizer.svelte";
    import type { Diagnostic } from "wipple";

    interface Props {
        code: string;
        diagnostic: Diagnostic;
        animate: boolean;
        onclose?: () => void;
    }

    const { code, diagnostic, animate, onclose }: Props = $props();

    let showAll = $state(false);
    let expanded = $state(false);

    const transition = animate ? scale : () => ({});

    let container = $state<HTMLDivElement>();
    $effect(() => {
        container?.scrollIntoView({ behavior: "smooth" });
    });

    const [title, ...extra] = diagnostic.message.split("\n\n");
</script>

<div
    bind:this={container}
    class="relative flex h-full w-full items-stretch justify-stretch pb-[10px]"
    in:transition={{ start: 0.95, opacity: 0.5 }}
>
    <Box class="flex flex-1 flex-col px-[10px] py-[8px] font-sans shadow-blue-500/10 transition">
        <div class="scroll flex flex-col gap-[2px]">
            <div class="mr-(--toolbar-height) font-semibold">
                <Markdown content={title} highlightGroups />
            </div>

            {#if extra.length === 1}
                <Markdown content={extra[0]} highlightGroups={false} />
            {/if}

            <div class="flex flex-row items-center gap-[10px]">
                {#snippet button(title: string, active: boolean, onclick: () => void)}
                    <ToolbarButton
                        data-active={active || undefined}
                        class="text-background-button -mx-[4px] self-start bg-transparent px-[4px] data-[active]:font-bold"
                        {onclick}
                    >
                        {title}
                    </ToolbarButton>
                {/snippet}

                {#if extra.length > 1}
                    {@render button("Details", showAll, () => (showAll = !showAll))}
                {/if}

                {#if diagnostic.graph != null}
                    {#if extra.length > 1}
                        <div class="h-[1.25em] rounded-full border-[1px] border-gray-500/40"></div>
                    {/if}

                    {@render button("Explore", expanded, () => (expanded = !expanded))}
                {/if}
            </div>

            {#if showAll}
                <div class="mt-[5px]">
                    <Markdown content={extra.join("\n\n")} highlightGroups />
                </div>

                {#if diagnostic.groups > 1}
                    <p class="mx-[4px] my-[8px] text-sm opacity-75">
                        <strong>Tip:</strong>
                        Hover over an
                        <span class="diagnostic diagnostic-dimmed">outlined</span>
                        piece of code to highlight related code.
                    </p>
                {/if}
            {/if}
        </div>
    </Box>

    <div class="absolute top-[10px] right-[10px]">
        <ToolbarButton onclick={onclose} square>
            <Icon fill>close</Icon>
        </ToolbarButton>
    </div>
</div>

{#if expanded}
    <DiagnosticVisualizer {code} {diagnostic} onclose={() => (expanded = false)} />
{/if}
