<script lang="ts">
    import type { Snippet } from "svelte";
    import type { HTMLAttributes } from "svelte/elements";

    interface Props extends HTMLAttributes<HTMLButtonElement> {
        children: Snippet;
        prominent?: boolean;
        square?: boolean;
    }

    const { children, ...props }: Props = $props();
</script>

<button
    {...props}
    data-prominent={props.prominent || undefined}
    data-square={props.square || undefined}
    class={[
        "hover-highlight-secondary bg-background-secondary data-[prominent]:bg-background-button data-[prominent]:hover-highlight-button h-(--toolbar-height) data-[square]:w-(--toolbar-height) cursor-pointer rounded-[8px] px-2 data-[prominent]:text-white",
        "flex flex-row items-center justify-center gap-[4px]",
        props.class,
    ]}
>
    {@render children()}
</button>
