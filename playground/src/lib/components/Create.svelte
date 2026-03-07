<script lang="ts">
    import presets from "$lib/presets";
    import type { RuntimeId } from "$lib/runtimes";
    import Box from "./Box.svelte";
    import Logo from "./Logo.svelte";

    interface Props {
        onCreate: (runtime: RuntimeId) => void;
    }

    const { onCreate }: Props = $props();
</script>

<div class="absolute top-0 left-0 flex justify-center p-2.5">
    <Logo />
</div>

<div
    class="mx-auto flex w-full max-w-[600px] flex-1 flex-col justify-center-safe gap-2.5 overflow-auto px-[20px] py-[60px]"
>
    <h1 class="mb-3.5 flex flex-col items-center gap-2.5 text-center text-2xl font-semibold">
        What do you want to create?
    </h1>

    <div class="flex flex-col gap-2.5">
        {#each presets as preset (preset.runtime)}
            <button class="flex flex-1 cursor-pointer" onclick={() => onCreate(preset.runtime)}>
                <Box
                    class="flex flex-1 flex-row items-center gap-4 p-4 hover:bg-gray-50 dark:hover:bg-gray-800"
                >
                    <img src={preset.icon} alt={preset.name} class="size-[32px]" />

                    <div class="flex flex-1 flex-col text-left">
                        <span class="text-[large] font-semibold">{preset.name}</span>
                        <span class="text-sm opacity-50">{preset.description}</span>
                    </div>
                </Box>
            </button>
        {/each}
    </div>
</div>
