<script lang="ts">
    import Prism from "prismjs";
    import Misbehave from "misbehave";
    import { onMount } from "svelte";

    export let code: string;
    export let readOnly = false;
    export let color: string | undefined = undefined;
    export let change: (() => void) | undefined = undefined;

    let editorPre: HTMLElement;
    let editorCode: HTMLElement;

    onMount(() => {
        if (readOnly) return;

        new Misbehave(editorCode, {
            oninput: () => {
                code = editorCode.textContent;
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
    });
</script>

<pre
    bind:this={editorPre}
    class={readOnly
        ? "language-wipple"
        : "language-wipple line-numbers"}>
    <code
        bind:this={editorCode}
        style={color && `color: ${color}`}
        contenteditable={!readOnly}
        autocorrect="off"
        autocapitalize="off"
        spellcheck="false"
    >
        {code}
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
