<svelte:options
    customElement={{
        tag: "wipple-hover-link",
        shadow: "none",
    }}
/>

<script lang="ts">
    import CodeEditor from "./CodeEditor.svelte";
    import Icon from "./Icon.svelte";
    import Tooltip from "./Tooltip.svelte";

    const innerText = $host()
        .childNodes.values()
        .find((node) => node.nodeType === Node.TEXT_NODE);

    let source = $state("");
    if (innerText) {
        console.log("innerText:", innerText);
        source = innerText.textContent!;
        innerText?.remove();
    }
</script>

<Tooltip>
    {#snippet content()}
        <CodeEditor code={source} readOnly />
    {/snippet}

    <div
        class="mx-[0.5ch] bg-background-secondary rounded-full h-[1em] text-slate-500 flex items-center justify-center hover:bg-highlight-secondary"
    >
        <Icon>more_horiz</Icon>
    </div>
</Tooltip>

<style>
    :global(wipple-hover-link) {
        display: inline-block;
        vertical-align: text-top;
    }
</style>
