<script lang="ts">
    import colors from "$lib/assets/colors";
    import ColorPreview from "./ColorPreview.svelte";
    import ObjectWrapper from "./ObjectWrapper.svelte";

    interface Props {
        color: string;
    }

    let { color = $bindable() }: Props = $props();
</script>

<div class="grid grid-cols-6 gap-[4px] p-[4px]">
    {#each Object.entries(colors) as [name, { varName }]}
        <ObjectWrapper>
            <button class="flex flex-1" onclick={() => (color = name)}>
                <ColorPreview {name} {varName} />
            </button>
        </ObjectWrapper>
    {/each}
</div>
