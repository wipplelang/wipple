<svelte:options
    customElement={{
        tag: "wipple-diagnostic-widget",
        shadow: "none",
    }}
/>

<script lang="ts">
    import Box from "@/components/Box.svelte";
    import DiagnosticView from "@/components/DiagnosticView.svelte";
    import Icon from "@/components/Icon.svelte";
    import Markdown from "@/components/Markdown.svelte";
    import ToolbarButton from "@/components/ToolbarButton.svelte";
    import type { Action } from "svelte/action";
    import { fade, fly, scale } from "svelte/transition";
    import type { Diagnostic } from "wipple";

    interface Props {
        diagnostic: Diagnostic;
        onclose?: () => void;
    }

    const { diagnostic, onclose }: Props = $props();

    const [primaryMessage, ...secondaryMessages] = $derived(diagnostic.message.split("\n\n"));

    let container = $state<HTMLDivElement>();
    $effect(() => {
        container?.scrollIntoView({ behavior: "smooth" });
    });

    let explore = $state(false);

    const portal: Action = (node) => {
        $effect(() => {
            document.body.appendChild(node);

            return () => {
                node.remove();
            };
        });
    };
</script>

<div
    bind:this={container}
    class="relative flex h-full w-full items-stretch justify-stretch pb-[10px]"
    in:scale={{ start: 0.95, opacity: 0.5 }}
>
    <Box class="flex flex-1 flex-col p-[10px] font-sans shadow-blue-500/10 transition">
        <p class="mb-[4px] flex flex-row items-center gap-[5px] font-semibold text-blue-500">
            <Icon>error</Icon>
            Error
        </p>

        <div class="font-semibold">
            <Markdown content={primaryMessage} />
        </div>

        {#each secondaryMessages as message, index (index)}
            <Markdown content={message} />
        {/each}

        {#if diagnostic.traces.length > 0}
            <ToolbarButton onclick={() => (explore = true)} class="mt-[10px] self-start">
                Explore
            </ToolbarButton>
        {/if}
    </Box>
</div>

{#if explore}
    <div
        use:portal
        class="fixed inset-0 flex h-screen w-screen items-center justify-center bg-black/10 p-[20px]"
        transition:fade
    >
        <div transition:fly={{ y: 50 }}>
            <Box class="flex max-h-screen w-[800px] flex-col gap-[14px] overflow-scroll p-[14px]">
                <div class="flex h-(--toolbar-height) flex-1 flex-row justify-between gap-[10px]">
                    <p class="text-xl font-semibold">Explore</p>

                    <ToolbarButton prominent onclick={() => (explore = false)}>Done</ToolbarButton>
                </div>

                <DiagnosticView {diagnostic} />
            </Box>
        </div>
    </div>
{/if}
