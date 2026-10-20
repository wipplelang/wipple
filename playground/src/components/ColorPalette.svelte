<script lang="ts">
    import colors from "@/assets/colors";
    import ColorPreview from "./ColorPreview.svelte";
    import ObjectWrapper from "./ObjectWrapper.svelte";

    interface Props {
        color: string;
    }

    let { color = $bindable() }: Props = $props();
</script>

<div class="grid grid-cols-6 gap-[2px] p-[4px]">
    {#each Object.entries(colors) as [name, { varName }]}
        <ObjectWrapper>
            <button class="flex size-[20px] flex-1" onclick={() => (color = name)}>
                <ColorPreview {name} {varName} />
            </button>
        </ObjectWrapper>
    {/each}
</div>
