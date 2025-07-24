<script lang="ts">
    import { animalSvgUrl } from "$lib/assets/animals";
    import Tooltip from "$lib/components/Tooltip.svelte";

    interface Props {
        name: string;
    }

    const { name }: Props = $props();
</script>

<Tooltip class="aspect-square flex-1 bg-white ">
    <img src={animalSvgUrl(name)} alt={name} class="object-contain" />

    {#snippet content()}
        <span class="capitalize">{name}</span>
    {/snippet}
</Tooltip>
