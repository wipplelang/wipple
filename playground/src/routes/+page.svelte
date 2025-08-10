<script lang="ts">
    import { playgroundMetadata, type Playground } from "$lib/models/Playground";
    import Box from "$lib/components/Box.svelte";
    import Logo from "$lib/components/Logo.svelte";
    import runtimes, { type RuntimeId } from "$lib/runtimes";
    import Create from "$lib/components/Create.svelte";
    import PrintingHeader from "$lib/components/PrintingHeader.svelte";
    import ToolbarButton from "$lib/components/ToolbarButton.svelte";
    import Icon from "$lib/components/Icon.svelte";
    import CodeEditor from "$lib/components/CodeEditor.svelte";
    import * as commands from "@codemirror/commands";
    import Tooltip from "$lib/components/Tooltip.svelte";
    import Commands from "$lib/components/Commands.svelte";
    import * as api from "$lib/api";
    import type { Command } from "$lib/models/Command";
    import BoxButton from "$lib/components/BoxButton.svelte";
    import { context } from "$lib/context.svelte";
    import Output, { type RunState } from "$lib/components/Output.svelte";
    import PrintButton from "$lib/components/PrintButton.svelte";
    import ShareButton from "$lib/components/ShareButton.svelte";
    import * as shareApi from "$lib/share";
    import { onMount } from "svelte";

    const loadPlayground = (): Playground | undefined => {
        const json = window.localStorage.getItem("playground");
        if (!json) {
            return undefined;
        }

        try {
            return JSON.parse(json);
        } catch (e) {
            console.error(e);
            return undefined;
        }
    };

    const savePlayground = (playground: Playground | undefined) => {
        if (playground) {
            localStorage.setItem("playground", JSON.stringify(playground));
        } else {
            localStorage.removeItem("playground");
        }
    };

    let playground = $state(loadPlayground());
    $effect(() => {
        context.playground = playground;
    });

    const createPlayground = (runtime: RuntimeId) => {
        playground = { runtime, code: "" };
    };

    const newPlayground = () => {
        const confirmed = confirm("Creating a new playground will clear this one. Are you sure?");
        if (confirmed) {
            playground = undefined;
        }
    };

    const tryLoadSharedPlayground = async () => {
        const params = new URLSearchParams(window.location.search);

        const shareId = params.get("share");
        if (!shareId) return;

        if (playground != null && playground.code.length > 0) {
            const confirmed = confirm(
                "Opening this shared playground will clear your current playground. Are you sure?",
            );

            if (!confirmed) {
                return;
            }
        }

        try {
            const response = await shareApi.get({ id: shareId });
            playground = {
                runtime: response.playground.runtime,
                code: response.playground.code,
            };
        } catch (e) {
            console.error(e);
            alert("Couldn't open this shared playground. Please verify the link and try again.");
        } finally {
            window.location.search = "";
        }
    };

    onMount(() => {
        tryLoadSharedPlayground();
    });

    const runtime = $derived(playground && runtimes[playground.runtime]);

    let editor: CodeEditor | undefined = $state();
    let output: Output | undefined = $state();

    let ideInfo = $state<Record<string, any>[]>([]);
    $effect(() => {
        context.ideInfo = ideInfo;
    });

    let ideInfoLibrary = $state<string>();
    $effect(() => {
        if (!playground) {
            return;
        }

        const metadata = playgroundMetadata(playground);

        if (ideInfoLibrary === metadata.library) {
            return;
        }

        ideInfoLibrary = metadata.library;

        (async () => {
            const response = await api.ideInfo({ ...metadata });
            ideInfo = response.info;
        })();
    });

    let dragInfo = $state<{ commandId: string; x: number; y: number; command: Command }>();

    const dropParams = $derived.by(() => {
        if (!editor || !dragInfo) {
            return undefined;
        }

        const dropParams = editor.getDropParams(dragInfo.command, {
            x: dragInfo.x,
            y: dragInfo.y,
        });

        return dropParams;
    });

    const ondrop = () => {
        if (editor && dropParams) {
            editor.drop(dropParams);
        }
    };

    let runState = $state<RunState>();
    let diagnostic = $state<any>();
    let diagnosticIsStale = $state(false);

    $effect(() => {
        playground?.code;

        if (runState === "error") {
            runState = undefined;
        }
    });

    $effect(() => {
        playground?.code;
        diagnosticIsStale = true;
    });

    $effect(() => {
        if (runState === "error" && diagnostic == null) {
            runState = undefined;
        }
    });
</script>

<svelte:window onbeforeunload={() => savePlayground(playground)} />

{#if playground}
    <div
        class="flex h-full flex-1 flex-col gap-[10px]"
        style:user-select={dragInfo ? "none" : "auto"}
        style:pointer-events={dragInfo ? "none" : "auto"}
    >
        <PrintingHeader />

        <div class="flex h-full flex-1 flex-row gap-[10px] p-[10px]">
            <div class="printing:hidden flex w-[250px] flex-col gap-[10px]">
                <Logo />

                <Box class="overflow-auto p-[14px]">
                    <Commands bind:dragInfo {ondrop} />
                </Box>

                <div class="flex flex-col gap-[10px] *:justify-start *:py-[6px]">
                    <BoxButton onclick={newPlayground}>
                        <Icon>add</Icon>
                        New Playground
                    </BoxButton>

                    <BoxButton
                        onclick={() => {
                            window.open("/docs", "_blank");
                        }}
                    >
                        <Icon>sticky_note_2</Icon>
                        Wipple Guide
                    </BoxButton>

                    <BoxButton
                        onclick={() => {
                            window.open(
                                "https://github.com/wipplelang/wipple/blob/main/CHANGELOG.md",
                                "_blank",
                            );
                        }}
                    >
                        <Icon>star</Icon>
                        What’s New
                    </BoxButton>
                </div>
            </div>

            <div class="flex min-w-[350px] flex-1 shrink-0 flex-col gap-[10px]">
                <div
                    class="printing:hidden h-(--toolbar-height) flex shrink-0 flex-row justify-between gap-[10px]"
                >
                    <div class="flex flex-row gap-[10px]">
                        <Tooltip content="Move Up">
                            <ToolbarButton
                                square
                                onclick={() => editor?.runCommand(commands.moveLineUp)}
                            >
                                <Icon>arrow_upward</Icon>
                            </ToolbarButton>
                        </Tooltip>

                        <Tooltip content="Move Down">
                            <ToolbarButton
                                square
                                onclick={() => editor?.runCommand(commands.moveLineDown)}
                            >
                                <Icon>arrow_downward</Icon>
                            </ToolbarButton>
                        </Tooltip>

                        <Tooltip content="Remove">
                            <ToolbarButton
                                square
                                onclick={() => editor?.runCommand(commands.deleteLine)}
                            >
                                <Icon>remove</Icon>
                            </ToolbarButton>
                        </Tooltip>

                        <Tooltip content="Undo">
                            <ToolbarButton square onclick={() => editor?.runCommand(commands.undo)}>
                                <Icon>undo</Icon>
                            </ToolbarButton>
                        </Tooltip>

                        <Tooltip content="Redo">
                            <ToolbarButton square onclick={() => editor?.runCommand(commands.redo)}>
                                <Icon>redo</Icon>
                            </ToolbarButton>
                        </Tooltip>
                    </div>
                </div>

                <Box class="flex-1">
                    <div
                        class={[
                            "relative h-full w-full transition-colors duration-75",
                            dragInfo && "bg-background-button/10",
                        ]}
                    >
                        <CodeEditor
                            bind:this={editor}
                            bind:code={playground.code}
                            diagnostic={dragInfo == null
                                ? {
                                      value: diagnostic,
                                      stale: diagnosticIsStale,
                                      onclose: () => (diagnostic = undefined),
                                  }
                                : undefined}
                            padding="14px"
                        />

                        {#if dropParams}
                            <div
                                class={[
                                    "border-background-button fixed border-collapse rounded-[6px]",
                                    dropParams.startLineNumber === dropParams.endLineNumber
                                        ? "border-[1px]"
                                        : "border-[2px]",
                                ]}
                                style:top={dropParams.top}
                                style:left={dropParams.left}
                                style:width={dropParams.width}
                                style:height={dropParams.height}
                            ></div>
                        {/if}
                    </div>
                </Box>
            </div>

            <div class="flex max-w-[450px] flex-1 flex-col gap-[10px] overflow-auto">
                <div
                    class="printing:hidden h-(--toolbar-height) flex shrink-0 flex-row justify-center gap-[10px]"
                >
                    <ToolbarButton
                        prominent={runState !== "error"}
                        onclick={() => output?.run()}
                        data-state={runState}
                        class="data-[state='error']:border-standard data-[state='error']:bg-background w-[200px] transition data-[prominent]:data-[state='compiling']:bg-sky-500 data-[prominent]:data-[state='running']:bg-sky-500 data-[state='error']:opacity-75"
                    >
                        {#if runState === undefined}
                            <Icon fill>play_arrow</Icon>
                            Run
                        {:else if runState === "compiling"}
                            <Icon fill class="animate-spin">progress_activity</Icon>
                        {:else if runState === "error"}
                            <Icon>error</Icon>
                            Errors found
                        {:else if runState === "running"}
                            <Icon fill>stop</Icon>
                            Stop
                        {/if}
                    </ToolbarButton>

                    {#if runtime?.printEnabled}
                        <PrintButton />
                    {/if}

                    <ShareButton />
                </div>

                <Output
                    bind:this={output}
                    bind:runState
                    ondiagnostics={(diagnostics) => {
                        diagnostic = diagnostics[0];
                        diagnosticIsStale = false;
                    }}
                />
            </div>
        </div>
    </div>
{:else}
    <Create onCreate={createPlayground} />
{/if}
