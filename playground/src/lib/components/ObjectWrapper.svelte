<script lang="ts">
    import type { Snippet } from "svelte";
    import type { HTMLAttributes } from "svelte/elements";

    interface Props extends HTMLAttributes<HTMLElement> {
        children: Snippet;
        scaleOnHover?: boolean;
    }

    const { children, scaleOnHover = true, ...props }: Props = $props();
</script>

<div
    {...props}
    data-scale-on-hover={scaleOnHover || undefined}
    class={[
        "bg-placeholder border-standard data-[scale-on-hover]:hover:scale-120 size-(--code-editor-line-height) inline-flex overflow-clip rounded-[7px] transition-transform",
        props.class,
    ]}
>
    {@render children()}
</div>
