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
        "bg-placeholder border-standard inline-flex size-(--code-editor-line-height) overflow-clip rounded-[7px] transition-transform data-[scale-on-hover]:hover:scale-120",
        props.class,
    ]}
>
    {@render children()}
</div>
