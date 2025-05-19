import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { CodeMirror, CodeMirrorRef, HighlightedCode, insertSnippet, Snippet } from "./codemirror";
import { RunOptions, Runner, RunnerRef } from "./runner";
import { MaterialSymbol, MaterialSymbolProps } from "react-material-symbols";
import { defaultThemeConfig, ThemeConfig } from "./codemirror/theme";
import { PaletteCategory, PaletteItem } from "../../models";
import { Animated, Logo, Markdown, Tooltip, useAlert } from "../../components";
import { defaultPaletteCategories, runtimes } from "../../runtimes";
import { ColorPicker } from "./assets/color-picker";
import { AssetClickHandler } from "./codemirror/assets";
import {
    animalAsset,
    colorAsset,
    dropdownAsset,
    melodyAsset,
    rhythmAsset,
    sliderAsset,
} from "./assets";
import { AnimalPicker } from "./assets/animal-picker";
import { MelodyPicker, RhythmPicker } from "./assets/melody-picker";
import * as commands from "@codemirror/commands";
import { nanoid } from "nanoid";
import { DndContext, DragOverlay, useDraggable, useDroppable } from "@dnd-kit/core";
import { CSS } from "@dnd-kit/utilities";
import { Command } from "@codemirror/view";
import {
    DiagnosticHelp,
    DiagnosticTemplate,
    DiagnosticTemplateData,
    resolveDiagnosticTemplate,
} from "../../templates/diagnostics";
import html2pdf from "html-to-pdf-js";
import { format as formatDate } from "date-fns";
import { useStore } from "../../store";
import { produce } from "immer";
import { Box } from "../../components/box";
import { ToolbarButton } from "../../components/toolbar-button";

export const CodeEditor = (props: {
    children: string;
    wipple: typeof import("wipple-wasm");
    onNewPlayground: () => void;
    onChange: (value: string) => void;
    readOnly?: boolean;
    runtime?: string;
}) => {
    const [store, setStore] = useStore();

    const theme = useMemo(() => defaultThemeConfig(), []);

    const [lastCompiledCode, setLastCompiledCode] = useState(props.children);

    const numberOfLines = useMemo(() => props.children.split("\n").length, [props.children]);

    const runtime = useMemo(
        () =>
            props.runtime != null && props.runtime in runtimes
                ? runtimes[props.runtime as keyof typeof runtimes]
                : undefined,
        [props.runtime],
    );

    const [editorHasFocus, setEditorHasFocus] = useState(false);
    const [runnerHasFocus, setRunnerHasFocus] = useState(false);

    const [diagnostics, setDiagnostics] = useState<any[]>([]);
    const [selectedDiagnostic, setSelectedDiagnostic] = useState<any>();

    useEffect(() => {
        setSelectedDiagnostic(diagnostics[0]);
    }, [diagnostics]);

    const codeForDiagnostic = useCallback(
        (diagnostic: any) => {
            const start = diagnostic.location.start.index;
            const end = diagnostic.location.end.index;

            return props.children.slice(start, end);
        },
        [lastCompiledCode],
    );

    const [highlightItems, setHighlightItems] = useState<Record<string, any>>({});

    const [runOptions, setRunOptions] = useState<RunOptions>({
        bundlePath: props.runtime ?? "base",
    });

    const contentRef = useRef<HTMLDivElement>(null);
    const codeMirrorRef = useRef<CodeMirrorRef>(null);
    const runnerRef = useRef<RunnerRef>(null);

    const [selection, setSelection] = useState({ start: 0, end: 0 });

    const isSelectingRange = useMemo(() => selection.start !== selection.end, [selection]);

    const selectionRect = useMemo(() => {
        if (!isSelectingRange || !codeMirrorRef.current) {
            return undefined;
        }

        const start = codeMirrorRef.current.editorView.coordsAtPos(selection.start, -1);
        const end = codeMirrorRef.current.editorView.coordsAtPos(selection.end, 1);

        if (!start || !end) {
            return undefined;
        }

        let maxLineLength = 0;
        let pos = selection.start;

        while (pos < selection.end) {
            const line = codeMirrorRef.current!.editorView.state.doc.lineAt(pos);

            const start = codeMirrorRef.current!.editorView.coordsAtPos(line.from, -1);
            const end = codeMirrorRef.current!.editorView.coordsAtPos(line.to, 1);

            pos = line.to + 1;

            if (!start || !end) {
                continue;
            }

            maxLineLength = Math.max(maxLineLength, end.right - start.left);
        }

        return {
            top: Math.min(start.top, end.top),
            left: Math.min(start.left, end.left),
            width: maxLineLength,
            height: Math.abs(start.top - end.bottom),
        };
    }, [selection]);

    const { displayAlert } = useAlert();

    const onClickAsset: AssetClickHandler = useCallback(({ start, end, asset }) => {
        switch (asset.type) {
            case "color": {
                displayAlert(({ dismiss }) => (
                    <ColorPicker
                        selection={asset.color}
                        onDismiss={(color) => {
                            codeMirrorRef.current?.editorView.dispatch({
                                changes: { from: start, to: end, insert: colorAsset(color) },
                            });

                            dismiss();
                        }}
                    />
                ));

                break;
            }
            case "animal": {
                displayAlert(({ dismiss }) => (
                    <AnimalPicker
                        selection={asset.animal}
                        onDismiss={(animal) => {
                            codeMirrorRef.current?.editorView.dispatch({
                                changes: { from: start, to: end, insert: animalAsset(animal) },
                            });

                            dismiss();
                        }}
                    />
                ));

                break;
            }
            case "dropdown": {
                codeMirrorRef.current?.editorView.dispatch({
                    changes: {
                        from: start,
                        to: end,
                        insert: dropdownAsset(asset.selection, asset.options),
                    },
                });

                break;
            }
            case "melody": {
                displayAlert(({ dismiss }) => (
                    <MelodyPicker
                        selection={asset.melody}
                        onDismiss={(melody) => {
                            codeMirrorRef.current?.editorView.dispatch({
                                changes: { from: start, to: end, insert: melodyAsset(melody) },
                            });

                            dismiss();
                        }}
                    />
                ));

                break;
            }
            case "rhythm": {
                displayAlert(({ dismiss }) => (
                    <RhythmPicker
                        selection={asset.rhythm}
                        onDismiss={(rhythm) => {
                            codeMirrorRef.current?.editorView.dispatch({
                                changes: { from: start, to: end, insert: rhythmAsset(rhythm) },
                            });

                            dismiss();
                        }}
                    />
                ));

                break;
            }
            case "slider": {
                codeMirrorRef.current?.editorView.dispatch({
                    changes: {
                        from: start,
                        to: end,
                        insert: sliderAsset(asset.value, asset.min, asset.max),
                    },
                });

                break;
            }
            default:
                asset satisfies never;
                break;
        }
    }, []);

    const runCommand = (command: Command) => {
        if (codeMirrorRef.current) {
            command(codeMirrorRef.current.editorView);
            codeMirrorRef.current.editorView.focus();
        }
    };

    const format = useCallback(async () => {
        if (!runnerRef.current) {
            return;
        }

        const formatted = await runnerRef.current.format(
            codeMirrorRef.current!.editorView.state.sliceDoc(),
        );

        props.onChange(formatted);
    }, [props.onChange]);

    const [draggedCommand, setDraggedCommand] = useState<{
        id: string;
        item: PaletteItem;
    }>();

    const [showStatus, setShowStatus] = useState(false);
    const [hasEdited, setHasEdited] = useState(false);

    const [isCompiling, setCompiling] = useState(false);
    const [isRunning, setRunning] = useState(false);

    const handleChangeCompiling = useCallback((isCompiling: boolean) => {
        setCompiling(isCompiling);

        if (!isCompiling) {
            setHasEdited(false);
            setShowStatus(true);
        }
    }, []);

    const handleChangeRunning = useCallback((isRunning: boolean) => {
        setRunning(isRunning);
    }, []);

    const isCompilingOrRunning = isCompiling || isRunning;

    const handleRun = useCallback(async () => {
        if (!runnerRef.current) return;

        if (isCompilingOrRunning) {
            await runnerRef.current.stop?.();
        } else {
            setLastCompiledCode(props.children);
            await runnerRef.current.run?.();
        }
    }, [isCompilingOrRunning, props.children]);

    const highlightedCode: HighlightedCode | undefined = useMemo(() => {
        if (!selectedDiagnostic || hasEdited) {
            return undefined;
        }

        return {
            startIndex: selectedDiagnostic.location.start.index,
            endIndex: selectedDiagnostic.location.end.index,
            severity: selectedDiagnostic.severity,
        };
    }, [selectedDiagnostic, hasEdited]);

    const print = useCallback(async () => {
        if (contentRef.current) {
            setStore(
                produce((store) => {
                    store.isPrinting = true;
                }),
            );

            // Wait for the color scheme to update
            await new Promise((resolve) => setTimeout(resolve, 500));

            try {
                const filename = `wipple-${formatDate(new Date(), "yyyyMMddHHmmss")}.pdf`;

                const pdf = await html2pdf()
                    .set({
                        filename,
                        enableLinks: false,
                        html2canvas: { scale: 4 },
                        jsPDF: {
                            unit: "in",
                            format: "letter",
                            orientation: "landscape",
                        },
                    })
                    .from(contentRef.current)
                    .toPdf()
                    .get("pdf");

                const a = document.createElement("a");
                a.href = pdf.output("bloburl");
                a.download = filename;
                a.click();
            } finally {
                setStore(
                    produce((store) => {
                        store.isPrinting = false;
                    }),
                );
            }
        }
    }, []);

    const handleFocus = useCallback(() => setEditorHasFocus(true), []);

    const handleBlur = useCallback(() => setEditorHasFocus(false), []);

    const handleChange = useCallback(
        (value: string) => {
            setHasEdited(true);
            props.onChange(value);
        },
        [props.onChange],
    );

    return (
        <DndContext
            onDragEnd={({ over }) => {
                if (!draggedCommand || !codeMirrorRef.current) {
                    return;
                }

                setDraggedCommand(undefined);

                if (over) {
                    const line = over.id as number;

                    const snippet: Snippet = {
                        code: draggedCommand.item.code,
                        replace: draggedCommand.item.replace ?? false,
                    };

                    insertSnippet(
                        codeMirrorRef.current.editorView,
                        snippet,
                        line,
                        isSelectingRange ? selection : undefined,
                    );

                    format();
                }
            }}
        >
            <div
                ref={contentRef}
                data-printing={store.isPrinting || undefined}
                className={`flex flex-col w-full h-full gap-2.5 ${
                    store.isPrinting ? "p-[0.5in] overflow-hidden" : ""
                }`}
            >
                {store.isPrinting ? <PrintHeader /> : null}

                <div
                    className={`flex-1 flex flex-row justify-stretch p-2.5 gap-2.5 ${
                        store.isPrinting ? "overflow-hidden" : ""
                    }`}
                >
                    {!store.isPrinting ? (
                        <div className="h-full w-[240px] opacity-1">
                            <div className="flex flex-col w-[240px] gap-2.5">
                                <CommandPalette
                                    theme={theme}
                                    categories={
                                        runtime?.paletteCategories ?? defaultPaletteCategories
                                    }
                                    highlightItems={highlightItems}
                                    draggedCommand={draggedCommand}
                                    onBeginDraggingCommand={(id, item) =>
                                        setDraggedCommand({ id, item })
                                    }
                                />
                            </div>
                        </div>
                    ) : null}

                    <div className="h-full flex flex-col gap-2.5 flex-1 opacity-1 min-w-[350px] flex-shrink-0">
                        {!store.isPrinting ? (
                            <div className="flex flex-row justify-between shrink-0 h-8 gap-2.5">
                                <ToolbarButton icon="add" onClick={props.onNewPlayground}>
                                    New
                                </ToolbarButton>

                                <div className="flex flex-row h-full gap-2.5">
                                    <Tooltip description="Move Up">
                                        <ToolbarButton
                                            square
                                            icon="arrow_upward"
                                            onClick={() => runCommand(commands.moveLineUp)}
                                        />
                                    </Tooltip>

                                    <Tooltip description="Move Down">
                                        <ToolbarButton
                                            square
                                            icon="arrow_downward"
                                            onClick={() => runCommand(commands.moveLineDown)}
                                        />
                                    </Tooltip>

                                    <Tooltip description="Remove">
                                        <ToolbarButton
                                            square
                                            icon="remove"
                                            onClick={() => runCommand(commands.deleteLine)}
                                        />
                                    </Tooltip>

                                    <Tooltip description="Undo">
                                        <ToolbarButton
                                            square
                                            icon="undo"
                                            onClick={() => runCommand(commands.undo)}
                                        />
                                    </Tooltip>

                                    <Tooltip description="Redo">
                                        <ToolbarButton
                                            square
                                            icon="redo"
                                            onClick={() => runCommand(commands.redo)}
                                        />
                                    </Tooltip>
                                </div>
                            </div>
                        ) : null}

                        <Box fill>
                            <div
                                className={`flex flex-col inset-0 w-full h-full gap-2.5 ${
                                    store.isPrinting ? "py-2.5" : ""
                                }`}
                            >
                                <CodeMirror
                                    ref={codeMirrorRef}
                                    autoFocus
                                    onFocus={handleFocus}
                                    onBlur={handleBlur}
                                    onChange={handleChange}
                                    onChangeSelection={setSelection}
                                    readOnly={props.readOnly ?? false}
                                    onClickAsset={onClickAsset}
                                    theme={theme}
                                    highlightedCode={highlightedCode}
                                    highlightItems={highlightItems}
                                >
                                    {props.children}
                                </CodeMirror>

                                {draggedCommand ? (
                                    <div className="absolute inset-0 bg-blue-500 bg-opacity-10">
                                        {draggedCommand.item.replace ? (
                                            <DropTargetArea rect={selectionRect} />
                                        ) : (
                                            <DropTargetLines
                                                numberOfLines={numberOfLines}
                                                theme={theme}
                                            />
                                        )}
                                    </div>
                                ) : null}
                            </div>
                        </Box>
                    </div>

                    <div className={store.isPrinting ? "h-full" : "basis-[550px] h-full"}>
                        <div className="flex flex-col h-full">
                            {!store.isPrinting ? (
                                <>
                                    <div className="flex flex-row justify-center gap-2.5 h-8 mb-2.5">
                                        <RunButton
                                            isRunning={isCompilingOrRunning}
                                            onClick={handleRun}
                                        />

                                        <OptionsButton
                                            icon="print"
                                            description="Print"
                                            onClick={print}
                                        />
                                    </div>

                                    <Animated direction="vertical" open={showStatus}>
                                        {diagnostics.length > 0 ? (
                                            <ErrorBrowser
                                                diagnostics={diagnostics}
                                                active={!hasEdited}
                                                onSelectDiagnostic={setSelectedDiagnostic}
                                                codeForDiagnostic={codeForDiagnostic}
                                            />
                                        ) : null}
                                    </Animated>
                                </>
                            ) : null}

                            <Runner
                                ref={runnerRef}
                                wipple={props.wipple}
                                options={runOptions}
                                runtime={runtime}
                                hasFocus={runnerHasFocus}
                                onFocus={() => setRunnerHasFocus(true)}
                                onBlur={() => setRunnerHasFocus(false)}
                                onChangeDiagnostics={setDiagnostics}
                                onChangeHighlightItems={setHighlightItems}
                                onChangeCompiling={handleChangeCompiling}
                                onChangeRunning={handleChangeRunning}
                            >
                                {props.children}
                            </Runner>
                        </div>
                    </div>
                </div>

                <DragOverlay className="w-full">
                    {draggedCommand ? (
                        <div className="w-screen h-screen">
                            <div
                                className="flex flex-row items-center bg-white dark:bg-gray-800 w-fit h-fit -mx-1 px-1 rounded-md shadow-lg"
                                style={{
                                    fontFamily: theme.fontFamily,
                                    fontSize: theme.fontSize,
                                    lineHeight: theme.lineHeight,
                                }}
                            >
                                <CommandPreviewContent
                                    code={draggedCommand.item.code}
                                    theme={theme}
                                    highlightItems={highlightItems}
                                />
                            </div>
                        </div>
                    ) : null}
                </DragOverlay>
            </div>
        </DndContext>
    );
};

const RunButton = (props: { isRunning: boolean; onClick: (() => void) | undefined }) => (
    <button
        onClick={props.onClick}
        disabled={props.onClick == null}
        className={`flex-1 flex flex-row items-center justify-center gap-0.5 disabled:bg-gray-200 dark:disabled:bg-gray-700 transition-colors text-white disabled:text-gray-400 dark:disabled:text-gray-600 rounded-lg max-w-[200px] ${
            props.isRunning
                ? "enabled:hover:bg-red-600 bg-red-500 dark:enabled:hover:bg-red-400"
                : "enabled:hover:bg-blue-600 bg-blue-500 dark:enabled:hover:bg-blue-400"
        }`}
    >
        {props.onClick ? (
            <>
                <MaterialSymbol
                    icon={props.isRunning ? "stop" : "play_arrow"}
                    fill
                    className="text-2xl"
                />

                {props.isRunning ? "Stop" : "Run"}
            </>
        ) : (
            <>
                <div className="text-2xl h-[1lh]"></div>
                Loading
            </>
        )}
    </button>
);

const OptionsButton = (props: {
    icon: MaterialSymbolProps["icon"];
    description: string;
    onClick: () => void;
}) => (
    <div className="aspect-square">
        <Tooltip
            className="flex items-center justify-center bg-blue-100 dark:bg-blue-950 hover:bg-blue-500 text-blue-500 hover:text-white transition-colors rounded-lg w-full h-full"
            onClick={props.onClick}
            description={props.description}
        >
            <MaterialSymbol icon={props.icon} className="text-2xl" />
        </Tooltip>
    </div>
);

const ErrorBrowser = (props: {
    diagnostics: any[];
    onSelectDiagnostic: (diagnostic: any) => void;
    codeForDiagnostic: (diagnostic: any) => string;
    active: boolean;
}) => {
    const [activeIndex, setActiveIndex] = useState(0);

    const activeDiagnostic = props.diagnostics[activeIndex];

    useEffect(() => {
        props.onSelectDiagnostic(activeDiagnostic);
    }, [activeDiagnostic]);

    const [resolved, setResolved] = useState<{
        template: DiagnosticTemplate;
        data: DiagnosticTemplateData;
    }>();

    useEffect(() => {
        // Reset the history
        setResolved(undefined);

        requestAnimationFrame(() => {
            const resolved = resolveDiagnosticTemplate(
                activeDiagnostic,
                props.codeForDiagnostic(activeDiagnostic),
            );

            setResolved(resolved);
        });
    }, [activeDiagnostic, props.codeForDiagnostic]);

    return (
        <div className="pb-3">
            <div
                className={`flex flex-col rounded-lg border-[1px] transition-colors ${
                    props.active
                        ? "bg-red-50 dark:bg-red-950 border-red-300 dark:border-red-700"
                        : "bg-gray-100 dark:bg-gray-800 border-gray-200 dark:border-gray-700"
                }`}
            >
                <div className="flex flex-row items-center justify-center pt-0.5 px-1">
                    <MaterialSymbol
                        icon="keyboard_arrow_left"
                        as="button"
                        disabled={activeIndex === 0}
                        className="text-xl font-medium text-blue-500 disabled:text-gray-400"
                        onClick={() => setActiveIndex(activeIndex - 1)}
                    />

                    <p className="flex-1 text-center text-sm font-semibold">
                        {props.diagnostics.length > 1
                            ? `${activeIndex + 1} of ${props.diagnostics.length} errors`
                            : "1 error"}
                    </p>

                    <MaterialSymbol
                        icon="keyboard_arrow_right"
                        as="button"
                        disabled={activeIndex === props.diagnostics.length - 1}
                        className="text-xl font-medium text-blue-500 disabled:text-gray-400"
                        onClick={() => setActiveIndex(activeIndex + 1)}
                    />
                </div>

                <div className="text-sm pt-1 px-2 pb-2">
                    {resolved ? (
                        <ErrorMarkdown template={resolved.template} data={resolved.data} />
                    ) : (
                        <Markdown>{`Unknown error: \`${activeDiagnostic.template.id}\``}</Markdown>
                    )}
                </div>
            </div>
        </div>
    );
};

const ErrorMarkdown = (props: { template: DiagnosticTemplate; data: DiagnosticTemplateData }) => {
    const diagnostic = useMemo(() => props.template(props.data), [props.template]);

    const [history, setHistory] = useState<DiagnosticHelp[]>([]);

    useEffect(() => {
        setHistory([]);
    }, [props.template]);

    const handleNext = useCallback(
        (help: DiagnosticHelp) => {
            if (help) {
                setHistory((history) => [...history, help]);
            } else {
                setHistory([]);
            }
        },
        [props.data],
    );

    return (
        <div className="flex flex-col items-start gap-2">
            <div className="flex flex-col">
                <Markdown className="font-semibold">{diagnostic.title}</Markdown>
                <Markdown className="opacity-75">{diagnostic.description}</Markdown>
            </div>

            {history.length === 0 && diagnostic.help != null ? (
                <div className="flex flex-row gap-2">
                    <ErrorTemplateButton onClick={() => setHistory([diagnostic.help])}>
                        Help
                    </ErrorTemplateButton>
                </div>
            ) : (
                history.map((help, index) => (
                    <ErrorHelpView
                        key={index}
                        help={help}
                        onNext={index === history.length - 1 ? handleNext : undefined}
                    />
                ))
            )}
        </div>
    );
};

const ErrorTemplateButton = (props: {
    onClick?: () => void;
    destructive?: boolean;
    children: string;
}) => (
    <button
        disabled={props.onClick == null}
        className={`flex-1 flex flex-row items-center justify-center px-1.5 py-1 gap-0.5 text-white rounded-lg disabled:bg-gray-400 disabled:dark:bg-gray-600 ${
            props.destructive
                ? "enabled:hover:bg-red-600 enabled:bg-red-500 enabled:dark:hover:bg-red-400"
                : "enabled:hover:bg-blue-600 enabled:bg-blue-500 enabled:dark:hover:bg-blue-400"
        }`}
        onClick={props.onClick}
    >
        {props.children}
    </button>
);

const ErrorHelpView = (props: {
    help: DiagnosticHelp;
    onNext?: (next: DiagnosticHelp) => void;
}) => {
    const containerRef = useRef<HTMLDivElement>(null);

    if (!props.help) {
        return null;
    }

    const makeContainer = (body: JSX.Element) => (
        <div ref={containerRef} className="flex flex-col items-start gap-2">
            {body}
        </div>
    );

    switch (props.help.type) {
        case "message":
            return makeContainer(
                <>
                    <Markdown>{props.help.message}</Markdown>

                    {props.onNext ? (
                        <ErrorTemplateButton onClick={() => props.onNext!(undefined)}>
                            Reset
                        </ErrorTemplateButton>
                    ) : null}
                </>,
            );
        case "choice":
            return makeContainer(
                <>
                    <Markdown>{props.help.question}</Markdown>

                    <div className="flex flex-row gap-1">
                        {props.help.choices.map(({ choice, response }, index) => (
                            <ErrorTemplateButton
                                key={index}
                                onClick={props.onNext ? () => props.onNext!(response) : undefined}
                            >
                                {choice}
                            </ErrorTemplateButton>
                        ))}
                    </div>
                </>,
            );
        case "prompt":
            const help = props.help;

            return makeContainer(
                <>
                    <Markdown>{props.help.prompt}</Markdown>

                    <ErrorPrompt
                        onSubmit={
                            props.onNext
                                ? (answer) => props.onNext!(help.response({ answer }))
                                : undefined
                        }
                    />
                </>,
            );
    }
};

const ErrorPrompt = (props: { onSubmit?: (answer: string) => void }) => {
    const [value, setValue] = useState("");

    return (
        <form
            onSubmit={(e) => {
                e.preventDefault();
                props.onSubmit?.(value);
            }}
            className="relative mt-1"
        >
            <input
                disabled={props.onSubmit == null}
                placeholder="Type here..."
                onChange={(e) => setValue(e.target.value)}
                className={`w-full rounded-md px-2 py-1.5 outline outline-gray-300 dark:outline-gray-700 focus:outline-blue-500 ${
                    value ? "bg-white dark:bg-gray-950" : "bg-gray-50 dark:bg-gray-900"
                }`}
            />

            <button
                type="submit"
                disabled={props.onSubmit == null || !value}
                className="absolute top-0 bottom-0 right-1.5 my-auto flex items-center justify-center w-6 h-6 bg-blue-500 disabled:bg-gray-300 disabled:dark:bg-gray-800 rounded-md"
            >
                <MaterialSymbol icon="arrow_forward" className="text-white text-xl" />
            </button>
        </form>
    );
};

const NoErrors = (props: { active: boolean }) => (
    <div className="pb-3">
        <div
            className={`flex flex-row items-center justify-center gap-1.5 p-1.5 rounded-lg text-sm font-semibold border-[1px] transition-colors ${
                props.active
                    ? "bg-green-50 dark:bg-green-950 border-green-300 dark:border-green-700 text-green-600 dark:text-green-400"
                    : "bg-gray-100 dark:bg-gray-800 border-gray-200 dark:border-gray-700 text-gray-600 dark:text-gray-400"
            }`}
        >
            No errors
        </div>
    </div>
);

const CommandPalette = (props: {
    header?: JSX.Element;
    categories: PaletteCategory[];
    theme: ThemeConfig;
    highlightItems: Record<string, any>;
    draggedCommand: { id: string; item: PaletteItem } | undefined;
    onBeginDraggingCommand: (id: string, item: PaletteItem) => void;
}) => (
    <div className="flex-[1.5] flex flex-col h-full bg-white dark:bg-gray-900 gap-2.5 z-10">
        <Logo />

        <Box>
            {props.categories.map((category) => (
                <div key={category.title} className="flex flex-col mb-2.5">
                    <p className="text-gray-600 dark:text-gray-400 text-sm mb-1">
                        {category.title}
                    </p>

                    {category.items.map((item) => (
                        <div key={item.title}>
                            {props.draggedCommand == null || props.draggedCommand.item !== item ? (
                                <CommandPreview
                                    key={item.title}
                                    item={item}
                                    theme={props.theme}
                                    highlightItems={props.highlightItems}
                                    onDragStart={(id) => {
                                        props.onBeginDraggingCommand(id, item);
                                    }}
                                />
                            ) : (
                                <div className="h-[1lh] mb-1 " />
                            )}
                        </div>
                    ))}
                </div>
            ))}
        </Box>
    </div>
);

const CommandPreview = (props: {
    item: PaletteItem;
    theme: ThemeConfig;
    highlightItems: Record<string, any>;
    onDragStart: (id: string) => void;
}) => {
    const id = useMemo(() => nanoid(), []);

    const { attributes, listeners, setNodeRef, transform, isDragging } = useDraggable({ id });

    useEffect(() => {
        if (isDragging) {
            props.onDragStart(id);
        }
    }, [isDragging, id]);

    const style = {
        transform: CSS.Transform.toString(
            transform && { x: transform.x, y: transform.y, scaleX: 1, scaleY: 1 },
        ),
    };

    return (
        <div ref={setNodeRef} style={style} {...listeners} {...attributes}>
            <CommandPreviewContent
                code={props.item.title}
                theme={props.theme}
                highlightItems={props.highlightItems}
            />
        </div>
    );
};

const CommandPreviewContent = (props: {
    code: string;
    theme: ThemeConfig;
    highlightItems: Record<string, any>;
}) => (
    <div className="hover:bg-gray-100 dark:bg-gray-800 transition -mx-1 px-1 rounded-lg">
        <div className="w-fit pointer-events-none">
            <CodeMirror
                autoFocus={false}
                readOnly={true}
                theme={props.theme}
                highlightItems={props.highlightItems}
            >
                {props.code}
            </CodeMirror>
        </div>
    </div>
);

const DropTargetLines = (props: { numberOfLines: number; theme: ThemeConfig }) => (
    <div className="absolute inset-0 flex flex-col">
        <div className="pt-1.5 pb-1">
            <DropTargetLine index={-1} theme={props.theme} />
        </div>

        {new Array(props.numberOfLines).fill(undefined).map((_, index) => (
            <DropTargetLine
                key={index}
                index={index}
                useLineHeight
                expand={index === props.numberOfLines - 1}
                theme={props.theme}
            />
        ))}
    </div>
);

const DropTargetLine = (props: {
    index: number;
    useLineHeight?: boolean;
    expand?: boolean;
    theme: ThemeConfig;
}) => {
    const { setNodeRef, isOver } = useDroppable({ id: props.index });

    return (
        <div
            ref={setNodeRef}
            className={`flex flex-col ${!props.useLineHeight ? "h-1" : ""} ${
                props.expand ? "flex-1" : ""
            }`}
        >
            <div
                className="relative"
                style={{
                    height: props.useLineHeight
                        ? props.theme.fontSize * props.theme.lineHeight
                        : undefined,
                }}
            >
                <div className="absolute left-4 right-4 bottom-0">
                    <div
                        className={`w-full h-1 bg-blue-500 rounded-full transition-opacity ${
                            isOver ? "opacity-100" : "opacity-0"
                        }`}
                    />
                </div>
            </div>

            {props.expand ? <div className="flex-1"></div> : null}
        </div>
    );
};

const dropTargetAreaPaddingForSelection = 6;

const DropTargetArea = (props: {
    rect: { top: number; left: number; width: number; height: number } | undefined;
}) => {
    const { setNodeRef, isOver } = useDroppable({ id: 0 });

    return (
        <div ref={setNodeRef} className="absolute inset-0">
            <div
                className={props.rect ? "fixed" : "p-2 w-full h-full"}
                style={
                    props.rect && {
                        top: props.rect.top - dropTargetAreaPaddingForSelection,
                        left: props.rect.left - dropTargetAreaPaddingForSelection,
                        width: props.rect.width + dropTargetAreaPaddingForSelection * 2,
                        height: props.rect.height + dropTargetAreaPaddingForSelection * 2,
                    }
                }
            >
                <div
                    className={`w-full h-full border-[3px] rounded-md border-blue-500 transition ${
                        isOver ? "border-opacity-100 scale-100" : "border-opacity-0 scale-[102.5%]"
                    }`}
                />
            </div>
        </div>
    );
};

const PrintHeader = () => {
    const [store, _setStore] = useStore();

    return (
        <div>
            <img src="/playground/images/logo.svg" className="w-12 h-12 mb-2.5" />

            <p className="text-lg font-semibold">
                {store.user?.displayName
                    ? `Created by ${store.user.displayName} with Wipple`
                    : "Created with Wipple"}
            </p>

            <p className="text-blue-500 font-semibold">wipple.org</p>
        </div>
    );
};
