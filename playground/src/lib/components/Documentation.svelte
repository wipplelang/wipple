<script lang="ts">
    import { context } from "$lib/context.svelte";
    import type { Command } from "$lib/models/Command";
    import CodeEditor from "./CodeEditor.svelte";
    import Markdown from "./Markdown.svelte";

    interface Props {
        name: string;
        command: Command;
    }

    const { name, command }: Props = $props();

    const item = $derived(context.documentation[name]);
</script>

{#if item}
    <div class="flex max-w-[300px] flex-col gap-[4px] p-[10px]">
        <CodeEditor readOnly code={command.code} />
        <Markdown content={item.docs} />
    </div>
{/if}
