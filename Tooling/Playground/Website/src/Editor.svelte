<script lang="ts">
    import Prism from "prismjs";
    import Misbehave from "misbehave";
    import { onMount } from "svelte";

    export let code: { text: string; color?: string }[];
    export let readOnly = false;
    export let change: (() => void) | undefined = undefined;
    export let focus = false;

    let editorPre: HTMLElement;
    let editorCode: HTMLElement;

    onMount(() => {
        if (readOnly) return;

        new Misbehave(editorCode, {
            oninput: () => {
                code = [{ text: editorCode.textContent }];
                change();
                Prism.highlightElement(editorCode);
            },
            softTabs: 4,
            pairs: [["(", ")"], ["[", "]"], ["{", "}"], ['"']],
        });

        editorPre.onclick = function () {
            editorCode.focus();
            return false;
        };

        if (focus) {
            editorCode.focus();
        }
    });
</script>

<pre
    bind:this={editorPre}
    class={readOnly
        ? "language-wipple"
        : "language-wipple line-numbers"}>
    <code
        bind:this={editorCode}
        contenteditable={!readOnly}
        autocorrect="off"
        autocapitalize="off"
        spellcheck="false"
    >
        {#each code as code}
            <div style={code.color && `color: ${code.color}`}>{code.text}</div>
        {/each}
    </code>
</pre>

<style>
    pre {
        min-height: 1.5em;
    }

    code {
        display: block;
        border-style: none;
        outline: none;
    }
</style>
