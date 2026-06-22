<script lang="ts">
    import Markdown, { getAstNode, type Plugin } from "svelte-exmarkdown";
    import { gfmPlugin } from "svelte-exmarkdown/gfm";
    import rehypeRaw from "rehype-raw";
    import CodeEditor, { createGroups } from "./CodeEditor.svelte";
    import { context } from "$lib/context.svelte";

    interface Props {
        content: string;
        highlightGroups?: boolean;
        fontSize?: number;
    }

    const { content, highlightGroups, fontSize }: Props = $props();

    const plugins: Plugin[] = [gfmPlugin(), { rehypePlugin: [rehypeRaw] }];
</script>

<div class="markdown text-wrap" style:font-size={fontSize ? `${fontSize}px` : undefined}>
    <Markdown md={content} {plugins}>
        {#snippet code()}
            {@const ast = getAstNode().current}
            {@const group = parseFloat((ast.properties?.dataGroup as string) ?? "-1")}
            {@const code = ast.children?.[0]?.value ?? ""}
            {@const groups =
                highlightGroups && context.diagnostic != null
                    ? createGroups(context.diagnostic.groups, [
                          { start: 0, end: code.length, group },
                      ])
                    : []}

            <span class="inline-flex size-fit">
                <CodeEditor
                    readOnly
                    {code}
                    {groups}
                    highlightedGroup={highlightGroups && group !== -1 ? group : undefined}
                    padding="0 1px"
                    {fontSize}
                />
            </span>
        {/snippet}
    </Markdown>
</div>
