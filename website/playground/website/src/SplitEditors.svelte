<script lang="ts" context="module">
    export const grayOutputColor = "#636f88";
    export const redOutputColor = "#f87171";
</script>

<script lang="ts">
    import { onMount } from "svelte";
    import Split from "split.js";
    import Editor from "./Editor.svelte";

    export let code: { text: string; color?: string }[];
    export let output: { text: string; color?: string }[];
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
        <Editor bind:code {change} focus />
    </div>

    <div bind:this={right}>
        <Editor readOnly code={output} />
    </div>
</div>
