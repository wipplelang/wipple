<script lang="ts">
    import runtimes from "$lib/runtimes";
    import Command from "./Command.svelte";
    import { type Command as CommandType } from "$lib/models/Command";
    import { context } from "$lib/context.svelte";

    interface Props {
        dragInfo?: { commandId: string; x: number; y: number; command: CommandType } | undefined;
        ondrop: () => void;
    }

    let { dragInfo = $bindable(), ondrop }: Props = $props();

    const playground = $derived(context.playground);
    const runtime = $derived(playground && runtimes[playground.runtime]);
</script>

{#if runtime}
    <div class="flex flex-col gap-[14px]">
        {#each Object.entries(runtime.commands) as [sectionName, commands]}
            <div class="flex flex-col">
                <p class="mb-[4px] text-[95%]">
                    {sectionName}
                </p>

                {#each Object.entries(commands) as [commandName, command], tabindex}
                    <Command
                        {tabindex}
                        commandId={`${sectionName}:${commandName}`}
                        {commandName}
                        {command}
                        bind:dragInfo
                        {ondrop}
                    />
                {/each}
            </div>
        {/each}
    </div>
{/if}
