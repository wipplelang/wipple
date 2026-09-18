<script lang="ts">
    import Markdown, { getAstNode, type Plugin } from "svelte-exmarkdown";
    import { gfmPlugin } from "svelte-exmarkdown/gfm";
    import rehypeRaw from "rehype-raw";
    import CodeEditor, { createGroups } from "./CodeEditor.svelte";
    import { DiagnosticLocation } from "wipple";

    interface Props {
        content: string;
        fontSize?: number;
    }

    const { content, fontSize }: Props = $props();

    const plugins: Plugin[] = [gfmPlugin(), { rehypePlugin: [rehypeRaw] }];
</script>

<div class="markdown text-wrap" style:font-size={fontSize ? `${fontSize}px` : undefined}>
    <Markdown md={content} {plugins}>
        {#snippet code()}
            {@const ast = getAstNode().current}
            {@const group = parseFloat((ast.properties?.dataGroup as string) ?? "-1")}
            {@const code = ast.children?.[0]?.value ?? ""}
            {@const groups =
                group !== -1
                    ? createGroups([{ start: 0, end: code.length, group } as DiagnosticLocation], {
                          primary: false,
                      })
                    : {}}

            <span class="inline-flex">
                <CodeEditor readOnly {code} {groups} padding="0 1px" {fontSize} />
            </span>
        {/snippet}
    </Markdown>
</div>
