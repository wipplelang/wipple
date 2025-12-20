<script lang="ts">
    import Box from "./Box.svelte";
    import * as api from "$lib/api";
    import { context } from "$lib/context.svelte";
    import runtimes from "$lib/runtimes";
    import * as Comlink from "comlink";
    import type { RunnerEnv, RunnerWorkerType } from "$lib/runner.worker";
    import RunnerWorker from "$lib/runner.worker?worker";
    import type { OutputItem } from "$lib/models/OutputItem";
    import Markdown from "./Markdown.svelte";
    import Prompt from "./Prompt.svelte";

    const throttleMs = 200;

    export type RunState = "compiling" | "error" | "running";

    interface Props {
        runState: RunState | undefined;
        ondiagnostics: (diagnostics: any[]) => void;
    }

    let { runState = $bindable(), ondiagnostics }: Props = $props();

    let runnerWorker: InstanceType<typeof RunnerWorker> | undefined = undefined;
    let runnerWorkerLink: Comlink.Remote<RunnerWorkerType> | undefined = undefined;

    const createRunnerWorker = () => {
        runnerWorker?.terminate();

        runnerWorker = new RunnerWorker();
        runnerWorkerLink = Comlink.wrap<RunnerWorkerType>(runnerWorker);

        return runnerWorkerLink;
    };

    const playground = $derived(context.playground);
    const runtime = $derived(playground && runtimes[playground.runtime]);

    const throttle = () =>
        new Promise<void>((resolve) => {
            setTimeout(() => resolve(), throttleMs);
        });

    let output = $state<OutputItem[]>([]);
    let runtimeOutput = $state<any>();

    let abortController: AbortController | undefined;

    export const run = async () => {
        if (playground == null || runState != null) {
            await stopRunning(true);
            runState = undefined;
            return;
        }

        runState = "compiling";

        // Needed for runtimes that perform setup within a user event
        await runtimeOutput?._initializeOnClick?.();

        abortController = new AbortController();

        let response: api.CompileResponse;
        try {
            response = await api.compile(
                {
                    code: playground.code,
                    library: runtimes[playground.runtime].library,
                },
                {
                    signal: abortController.signal,
                },
            );
        } catch (error) {
            console.error(error);
            abortController = undefined;
            return;
        } finally {
            await throttle();
        }

        if ("diagnostics" in response) {
            ondiagnostics(response.diagnostics);
            runState = "error";
            return;
        }

        response satisfies api.CompileResponseSuccess;

        runState = "running";
        ondiagnostics([]);
        output = [];

        try {
            await runtimeOutput?._initialize?.();

            const env: RunnerEnv = {
                display: async (message: string) => {
                    output.push({
                        type: "display",
                        value: message,
                    });
                },
                prompt: async (message: string, submit: (value: string) => Promise<boolean>) => {
                    await new Promise<void>((resolve) => {
                        output.push({
                            type: "prompt",
                            prompt: message,
                            submit: async (value) => {
                                const valid = await submit(value);
                                if (valid) {
                                    resolve();
                                }

                                return valid;
                            },
                        });
                    });
                },
            };

            // Wrap custom functions (which are tied to Svelte components) so
            // they aren't proxied by Comlink
            for (const key in runtimeOutput) {
                env[key] = (...args) => runtimeOutput[key](...args);
            }

            const worker = createRunnerWorker();
            await worker.run(response.executable, Comlink.proxy(env));
        } finally {
            await stopRunning(false);
            runState = undefined;
        }
    };

    const stopRunning = async (force: boolean) => {
        abortController?.abort();

        runnerWorker?.terminate();
        runnerWorker = undefined;

        await runtimeOutput?._cleanup?.(force);
    };
</script>

{#if runtime?.Output == null && output.length === 0}
    <div class="text-current/40 flex flex-1 flex-col items-center justify-center p-[14px]">
        <p>No output</p>
        <p>
            <small>
                You can use <code class="border-standard rounded-[8px] px-[6px]">show</code> to display
                a message.
            </small>
        </p>
    </div>
{:else}
    <div class="flex flex-col gap-[10px] overflow-auto">
        {#if runtime?.Output}
            <runtime.Output bind:this={runtimeOutput} />
        {/if}

        {#each output as item}
            {#if item.type === "display"}
                <Box class="p-[14px]" scroll={false}>
                    <Markdown content={item.value} />
                </Box>
            {:else if item.type === "prompt"}
                <Box class="p-[14px]" scroll={false}>
                    <Prompt prompt={item.prompt} onsubmit={item.submit} />
                </Box>
            {/if}
        {/each}
    </div>
{/if}
