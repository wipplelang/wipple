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

    interface Props {
        diagnostic: any;
        stale: boolean;
        onclose: () => void;
    }

    let { diagnostic, stale, onclose }: Props = $props();
</script>

<div class="flex h-full w-full items-stretch justify-stretch pb-[10px]">
    <Box
        data-stale={stale || undefined}
        class="not-[[data-stale]]:border-blue-500 not-[[data-stale]]:shadow-md flex flex-1 flex-col px-[10px] py-[8px] font-sans shadow-blue-500/10 transition data-[stale]:opacity-60"
    >
        <div class="flex flex-row items-start justify-between">
            <div class="flex flex-col gap-[2px]">
                <div class="font-semibold">
                    <Markdown content={diagnostic.message} />
                </div>

                <Markdown content={diagnostic.description} />
            </div>

            <ToolbarButton onclick={onclose} square>
                <Icon fill>close</Icon>
            </ToolbarButton>
        </div>
    </Box>
</div>
