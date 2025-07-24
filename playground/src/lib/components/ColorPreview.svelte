<script lang="ts">
    import Tooltip from "$lib/components/Tooltip.svelte";

    interface Props {
        name: string;
        varName: string;
    }

    const { name, varName }: Props = $props();
</script>

<Tooltip class="aspect-square flex-1" style="background-color: var({varName})">
    {#snippet content()}
        <span class="capitalize">{name}</span>
    {/snippet}
</Tooltip>
