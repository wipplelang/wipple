<script lang="ts">
    import type { Snippet } from "svelte";
    import type { HTMLAttributes } from "svelte/elements";

    interface Props extends HTMLAttributes<HTMLDivElement> {
        fill?: boolean;
        children: Snippet;
    }

    const { fill = false, children, ...props }: Props = $props();
</script>

<div {...props} data-fill={fill || undefined} class={["material-symbol text-icon", props.class]}>
    {@render children()}
</div>
