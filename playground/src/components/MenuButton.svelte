<script lang="ts">
    import type { Snippet } from "svelte";

    interface Props {
        onclick: () => void;
        disabled?: boolean;
        destructive?: boolean;
        children: Snippet;
    }

    const { onclick, disabled = false, destructive = false, children }: Props = $props();
</script>

<button
    {onclick}
    {disabled}
    data-destructive={destructive || undefined}
    class="hover-highlight data-[destructive]:text-destructive flex flex-1 cursor-pointer flex-row items-center gap-2.5 rounded-md px-2.5 py-1 disabled:opacity-50"
>
    {@render children()}
</button>
