<script lang="ts">
    import type { Snippet } from "svelte";
    import type { HTMLAttributes } from "svelte/elements";

    interface Props extends HTMLAttributes<HTMLButtonElement> {
        children: Snippet;
    }

    const { children, ...props }: Props = $props();
</script>

<button
    {...props}
    class={[
        "hover-highlight border-standard bg-background-secondary-alt flex h-full cursor-pointer flex-row items-center justify-start gap-[6px] rounded-[10px] px-2",
        props.class,
    ]}
>
    {@render children()}
</button>
