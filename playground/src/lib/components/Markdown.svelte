<script lang="ts">
    import Markdown, { getAstNode, type Plugin } from "svelte-exmarkdown";
    import { gfmPlugin } from "svelte-exmarkdown/gfm";
    import rehypeRaw from "rehype-raw";
    import CodeEditor from "./CodeEditor.svelte";

    interface Props {
        content: string;
        highlightGroups?: boolean;
    }

    const { content, highlightGroups }: Props = $props();

    const plugins: Plugin[] = [gfmPlugin(), { rehypePlugin: [rehypeRaw] }];
</script>

<div class="markdown text-wrap">
    <Markdown md={content} {plugins}>
        {#snippet code()}
            {@const ast = getAstNode().current}
            {@const group = parseFloat((ast.properties?.dataGroup as string) ?? "-1")}
            {@const code = ast.children?.[0].value ?? ""}

            <span class="inline-flex size-fit">
                <CodeEditor
                    readOnly
                    {code}
                    highlightGroup={highlightGroups ? group : undefined}
                    padding="0 1px"
                />
            </span>
        {/snippet}
    </Markdown>
</div>
