<script lang="ts">
    import type { Snippet } from "svelte";
    import type { ClassValue, HTMLAttributes } from "svelte/elements";

    interface Props extends HTMLAttributes<HTMLDivElement> {
        class?: ClassValue;
        scroll?: boolean;
        children: Snippet;
    }

    const { children, class: className = "", scroll = true, ...props }: Props = $props();
</script>

<div
    class={[
        "border-standard bg-background-secondary-alt rounded-[10px]",
        scroll ? "overflow-auto" : undefined,
        className,
    ]}
    {...props}
>
    {@render children()}
</div>
