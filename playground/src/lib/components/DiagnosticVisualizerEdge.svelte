<script lang="ts">
    import { BaseEdge, type EdgeProps, getStraightPath } from "@xyflow/svelte";

    const { sourceX, sourceY, targetX, targetY, markerEnd }: EdgeProps = $props();

    let [edgePath] = $derived(
        getStraightPath({
            sourceX,
            sourceY,
            targetX,
            targetY,
        }),
    );
</script>

<BaseEdge
    path={edgePath}
    {markerEnd}
    style="stroke-width: 2px; stroke: color-mix(in srgb, var(--color-slate-500) 33%, white);"
/>
