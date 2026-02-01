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

<div class="flex flex-1 flex-col items-center">
    <div class="mb-4 flex justify-center self-start p-2.5">
        <Logo />
    </div>

    <div
        class="mx-auto flex w-full max-w-[600px] flex-1 flex-col gap-2.5 overflow-auto px-4 pt-[80px]"
    >
        <h1 class="mb-3.5 flex flex-col items-center gap-2.5 text-center text-2xl font-semibold">
            What do you want to create?
        </h1>

        <div class="flex flex-row gap-2.5">
            {#each presets as preset}
                <button class="flex flex-1 cursor-pointer" onclick={() => onCreate(preset.runtime)}>
                    <Box class="flex-1">
                        <div class="relative flex-1 p-2.5 hover:bg-gray-50 dark:hover:bg-gray-800">
                            <div
                                class={[
                                    "absolute inset-0 z-0 mask-t-from-50% mask-t-to-black/75 opacity-50 transition-opacity hover:opacity-100",
                                    preset.backgroundClassName,
                                ]}
                            ></div>

                            <h2
                                class="pointer-events-none flex h-full flex-col justify-between gap-[10px] text-left font-semibold *:z-[1]"
                            >
                                <img src={preset.icon} alt={preset.name} class="size-[32px]" />
                                <span class="text-[large]">{preset.name}</span>
                            </h2>
                        </div>
                    </Box>
                </button>
            {/each}
        </div>
    </div>
</div>
