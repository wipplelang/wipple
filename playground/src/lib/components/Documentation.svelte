<script lang="ts">
    import * as api from "$lib/api";
    import { context } from "$lib/context.svelte";
    import type { Command } from "$lib/models/Command";
    import { playgroundMetadata } from "$lib/models/Playground";
    import CodeEditor from "./CodeEditor.svelte";
    import Markdown from "./Markdown.svelte";

    interface Props {
        name: string;
        command: Command;
    }

    const { name, command }: Props = $props();

    const playground = $derived(context.playground);

    let documentation = $state<Record<string, any>>();
    $effect(() => {
        if (!playground || command.documentationName === null) {
            return;
        }

        (async () => {
            const response = await api.documentation({
                ...playgroundMetadata(playground),
                name: command.documentationName ?? name,
            });

            documentation = response.documentation ?? undefined;
        })();
    });
</script>

{#if documentation}
    <div class="flex max-w-[300px] flex-col gap-[4px] p-[10px]">
        <CodeEditor readOnly code={command.code} />
        <Markdown content={documentation.docs} />
    </div>
{/if}
