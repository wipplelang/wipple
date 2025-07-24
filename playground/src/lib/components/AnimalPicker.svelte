<script lang="ts">
    import animals from "$lib/assets/animals";
    import AnimalPreview from "./AnimalPreview.svelte";
    import ObjectWrapper from "./ObjectWrapper.svelte";

    interface Props {
        onclick: (animal: string) => void;
    }

    const { onclick }: Props = $props();
</script>

<div class="grid grid-cols-7 gap-[4px] p-[4px]">
    {#each animals as animal}
        <ObjectWrapper class="size-[30px]">
            <button class="flex flex-1" onclick={() => onclick(animal)}>
                <AnimalPreview name={animal} />
            </button>
        </ObjectWrapper>
    {/each}
</div>
