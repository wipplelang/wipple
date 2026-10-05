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
        source = innerText.textContent!;
        innerText?.remove();
    }
</script>

<Tooltip>
    {#snippet content()}
        <CodeEditor code={source} readOnly />
    {/snippet}

    <div
        class="bg-background-secondary hover:bg-highlight-secondary mx-[0.5ch] flex h-[1em] items-center justify-center rounded-full text-slate-500"
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
