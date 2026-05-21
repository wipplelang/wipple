<script lang="ts">
    import Box from "./Box.svelte";
    import type { CompilerWorkerResponse } from "$lib/workers/compiler.worker";
    import { compilerWorker, context } from "$lib/context.svelte";
    import runtimes from "$lib/runtimes";
    import * as runner from "$lib/workers/runner.worker";
    import RunnerWorker from "$lib/workers/runner.worker?worker";
    import type { OutputItem, PromptOutputItem } from "$lib/models/OutputItem";
    import type { Groups } from "$lib/models/Groups";
    import Markdown from "./Markdown.svelte";
    import Prompt from "./Prompt.svelte";
    import Visualizer from "./Visualizer.svelte";

    export type RunState = "compiling" | "running";

    interface Props {
        runState: RunState | undefined;
        ondiagnostics: (diagnostics: any[]) => void;
        ongroups: (groups: Groups) => void;
        onchangeline: (line: number | undefined) => void;
    }

    let { runState = $bindable(), ondiagnostics, ongroups, onchangeline }: Props = $props();

    let runnerWorker: Worker | undefined = undefined;

    const createRunnerWorker = (env: runner.Env) => {
        runnerWorker?.terminate();
        runnerWorker = new RunnerWorker();

        return runner.init(runnerWorker, env);
    };

    const playground = $derived(context.playground);
    const runtime = $derived(playground && runtimes[playground.runtime]);

    let graph = $state<any>();
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
        onchangeline(undefined);

        // Needed for runtimes that perform setup within a user event
        await runtimeOutput?._initializeOnClick?.();

        abortController = new AbortController();

        const { library, visualizerEnabled } = runtimes[playground.runtime];

        let response: CompilerWorkerResponse<"compile">;
        try {
            response = await compilerWorker.compile({
                code: playground.code,
                library,
                groups: visualizerEnabled,
                graph: visualizerEnabled,
            });
        } catch (error) {
            console.error(error);
            abortController = undefined;
            return;
        } finally {
            runState = undefined;
        }

        graph = response.graph;

        if ("diagnostics" in response && response.diagnostics != null) {
            ondiagnostics(response.diagnostics);
            return;
        }

        if ("groups" in response && response.groups != null) {
            ongroups(response.groups);
        }

        runState = "running";
        ondiagnostics([]);
        output = [];

        try {
            await runtimeOutput?._initialize?.();

            let currentPrompt: {
                index: number;
                firstSubmission: boolean;
                input: Promise<string>;
            };

            const env: runner.Env = {
                trace: async (trace: any) => {
                    onchangeline(trace.line);
                },
                display: async (message: string) => {
                    output.push({
                        type: "display",
                        value: message,
                    });
                },
                beginPrompt: async (message: string) => {
                    currentPrompt = {
                        index: output.length,
                        firstSubmission: true,
                        input: new Promise<string>((resolve) => {
                            output.push({
                                type: "prompt",
                                prompt: message,
                                onsubmit: resolve,
                                valid: true,
                            });
                        }),
                    };
                },
                getPrompt: async () => {
                    const item = output[currentPrompt.index] as PromptOutputItem;

                    if (!currentPrompt.firstSubmission) {
                        item.valid = false;
                    } else {
                        currentPrompt.firstSubmission = false;
                    }

                    const input = await currentPrompt.input;

                    currentPrompt.input = new Promise<string>((resolve) => {
                        item.onsubmit = resolve;
                    });

                    return input;
                },
                endPrompt: async () => {
                    const item = output[currentPrompt.index] as PromptOutputItem;
                    item.onsubmit = undefined;
                    item.valid = true;
                },
            };

            // Wrap runtime functions (which are Svelte proxy objects)
            for (const key in runtimeOutput) {
                env[key] = (...args) => runtimeOutput[key](...args);
            }

            const { run } = createRunnerWorker(env);

            await run(response.executable);
        } finally {
            await stopRunning(false);
        }
    };

    const stopRunning = async (force: boolean) => {
        runState = undefined;
        abortController?.abort();

        runnerWorker?.terminate();
        runnerWorker = undefined;

        await runtimeOutput?._cleanup?.(force);

        onchangeline(undefined);
    };
</script>

<div class="flex h-full flex-col gap-[10px] overflow-auto">
    {#if runtime?.Output}
        <runtime.Output bind:this={runtimeOutput} />
    {/if}

    {#if runtime?.visualizerEnabled && graph != null}
        <div class="aspect-[3/2] max-h-[400px]">
            <Visualizer {graph} />
        </div>
    {/if}

    {#if runtime?.Output == null && output.length === 0}
        <div class="flex flex-1 flex-col items-center justify-center p-[14px] text-current/40">
            <p>No output</p>
            <p>
                <small>
                    You can use <code class="border-standard rounded-[8px] px-[6px]">show</code>
                    to display a message.
                </small>
            </p>
        </div>
    {:else}
        {#each output as item, index (index)}
            {#if item.type === "display"}
                <Box class="p-[14px]" scroll={false}>
                    <Markdown content={item.value} />
                </Box>
            {:else if item.type === "prompt"}
                <Box class="p-[14px]" scroll={false}>
                    <Prompt prompt={item.prompt} onsubmit={item.onsubmit} bind:valid={item.valid} />
                </Box>
            {/if}
        {/each}
    {/if}
</div>
