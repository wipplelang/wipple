<script lang="ts">
    import { context } from "$lib/context.svelte";
    import Markdown from "./Markdown.svelte";

    interface Props {
        name: string;
    }

    const { name }: Props = $props();

    const item = $derived(context.documentation[name]);
</script>

{#if item}
    <div class="flex max-w-[300px] flex-col gap-[4px] p-[10px]">
        <Markdown content={item.docs} />
    </div>
{/if}
