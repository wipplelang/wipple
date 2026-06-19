<script lang="ts">
    import { Handle, Position } from "@xyflow/svelte";
    import type { DiagnosticLocation } from "wipple";
    import Box from "./Box.svelte";
    import Markdown from "./Markdown.svelte";
    import CodeEditor from "./CodeEditor.svelte";

    interface Props {
        data: {
            id: string;
            code: string;
            location: DiagnosticLocation;
            message: string;
            consequences: string[];
            importance: number;
        };
    }

    const { data }: Props = $props();

    const isError = $derived(data.id === "error");

    const startOfLine = $derived(data.code.lastIndexOf("\n", data.location.start) + 1);
    const endOfLine = $derived(data.code.indexOf("\n", data.location.end));
    const code = $derived(data.code.slice(startOfLine, endOfLine === -1 ? undefined : endOfLine));

    const locations = $derived([
        {
            start: data.location.start - startOfLine,
            end: data.location.end - startOfLine,
        },
    ]);
</script>

<div
    style:width="350px"
    style:--bg-opacity="{(Math.max(data.importance, 0.1) / 10) * 100}%"
    style:--border-opacity="{(Math.max(data.importance, 0.1) / 2) * 100}%"
    class="bg-white"
>
    <Box
        class={[
            "flex flex-col gap-[10px] size-full p-[10px]",
            isError
                ? "bg-red-500/5 border-red-500/50"
                : "bg-blue-500/(--bg-opacity) border-blue-500/(--border-opacity)",
        ]}
    >
        <Box scroll class="p-[4px]">
            <CodeEditor
                {code}
                groups={[{ locations }]}
                highlightedGroup={0}
                highlightedGroupIsPrimary
                readOnly
            />
        </Box>

        <div class="flex flex-col gap-[10px]">
            <div class="font-semibold">
                <Markdown content={data.message} />
            </div>

            {#each data.consequences as consequence, index (index)}
                <Markdown content={consequence} />
            {/each}
        </div>
    </Box>
</div>

<Handle type="target" position={Position.Top} class="opacity-0" />
<Handle type="source" position={Position.Bottom} class="opacity-0" />
