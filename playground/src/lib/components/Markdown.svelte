<script lang="ts">
    import Markdown, { getAstNode, type Plugin } from "svelte-exmarkdown";
    import { gfmPlugin } from "svelte-exmarkdown/gfm";
    import CodeEditor from "./CodeEditor.svelte";

    interface Props {
        content: string;
    }

    const { content }: Props = $props();

    const plugins: Plugin[] = [gfmPlugin()];
</script>

<div class="markdown text-wrap">
    <Markdown md={content} {plugins}>
        {#snippet code()}
            {@const ast = getAstNode().current}
            {@const code = ast.children?.[0].value ?? ""}

            <span class="inline-flex size-fit">
                <CodeEditor readOnly {code} padding="0" />
            </span>
        {/snippet}
    </Markdown>
</div>
