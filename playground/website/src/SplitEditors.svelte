<script lang="ts" context="module">
    export const grayOutputColor = "#636f88";
    export const redOutputColor = "#f87171";
</script>

<script lang="ts">
    import { onMount } from "svelte";
    import Split from "split.js";
    import Editor from "./Editor.svelte";

    export let code: string;
    export let output: string;
    export let outputColor: string;
    export let change: () => void;

    let left: HTMLDivElement;
    let right: HTMLDivElement;

    onMount(() => {
        Split([left, right], {
            elementStyle: (dimension, size, gutterSize) => ({
                "flex-basis": `calc(${size}% - ${gutterSize}px)`,
                "overflow-x": "scroll",
            }),
            gutterStyle: (dimension, gutterSize) => ({
                "flex-basis": `${gutterSize}px`,
            }),
        });
    });
</script>

<div class="flex">
    <div bind:this={left}>
        <Editor bind:code {change} />
    </div>

    <div bind:this={right}>
        <Editor readOnly code={output} color={outputColor} />
    </div>
</div>
