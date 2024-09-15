import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import {
    CodeMirror,
    CodeMirrorRef,
    getTokenAtPos,
    HighlightedCode,
    insertSnippet,
    Snippet,
} from "./codemirror";
import { RunOptions, Runner, RunnerRef } from "./runner";
import { MaterialSymbol } from "react-material-symbols";
import { ThemeConfig } from "./codemirror/theme";
import { Help, PaletteCategory, PaletteItem } from "../models";
import {
    Animated,
    ContextMenuButton,
    defaultAnimationDuration,
    Markdown,
    Tooltip,
    TooltipContent,
    useAlert,
} from "../components";
import { defaultPaletteCategories, runtimes } from "../runtimes";
import { ColorPicker } from "./color-picker";
import { AssetClickHandler } from "./codemirror/assets";
import {
    animalAsset,
    colorAsset,
    dropdownAsset,
    instrumentAsset,
    noteAsset,
    objectAsset,
    sliderAsset,
} from "./assets";
import { AnimalPicker } from "./animal-picker";
import { NotePicker } from "./note-picker";
import { InstrumentPicker } from "./instrument-picker";
import { ObjectPicker } from "./object-picker";
import { StateCommand } from "@codemirror/state";
import * as commands from "@codemirror/commands";
import {
    FloatingPortal,
    shift,
    useClientPoint,
    useFloating,
    useInteractions,
} from "@floating-ui/react";
import { nanoid } from "nanoid";
import { DndContext, DragOverlay, useDraggable, useDroppable } from "@dnd-kit/core";
import { CSS } from "@dnd-kit/utilities";
import { Rect } from "@codemirror/view";
import { HelpAlert } from "./help-alert";
import {
    DiagnosticHelp,
    DiagnosticTemplate,
    resolveDiagnosticTemplate,
} from "../templates/diagnostics";
import Mustache from "mustache";
import html2pdf from "html-to-pdf-js";
import { format as formatDate } from "date-fns";
import { useStore } from "../store";
import { produce } from "immer";

export function CodeEditor<Settings>(props: {
    children: string;
    wipple: typeof import("wipple-wasm");
    onChange: (value: string) => void;
    theme: ThemeConfig;
    locked?: boolean;
    readOnly?: boolean;
    runtime?: {
        name: string;
        settings: Settings | undefined;
        onChangeSettings: (settings: Settings) => void;
    };
    autofocus?: boolean;
    onFocus?: () => void;
    onBlur?: () => void;
    onMoveUp?: () => void;
    onMoveDown?: () => void;
    onDelete?: () => void;
    onReset?: () => void;
    menu?: JSX.Element;
}) {
    const [store, setStore] = useStore();

    const [lastCompiledCode, setLastCompiledCode] = useState(props.children);

    const numberOfLines = useMemo(() => props.children.split("\n").length, [props.children]);

    const runtime = useMemo(
        () =>
            props.runtime != null && props.runtime.name in runtimes
                ? {
                      ...runtimes[props.runtime.name as keyof typeof runtimes],
                      settings: props.runtime.settings,
                      onChangeSettings: props.runtime.onChangeSettings,
                  }
                : undefined,
        [props.runtime],
    );

    const [isFocused, setFocused] = useState(props.autofocus ?? false);
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
        bundlePath: props.runtime?.name ?? "base",
    });

    const contentRef = useRef<HTMLDivElement>(null);
    const codeMirrorRef = useRef<CodeMirrorRef>(null);
    const runnerRef = useRef<RunnerRef>(null);

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
            case "note": {
                displayAlert(({ dismiss }) => (
                    <NotePicker
                        selection={asset.note}
                        onDismiss={(note) => {
                            codeMirrorRef.current?.editorView.dispatch({
                                changes: { from: start, to: end, insert: noteAsset(note) },
                            });

                            dismiss();
                        }}
                    />
                ));

                break;
            }
            case "instrument": {
                displayAlert(({ dismiss }) => (
                    <InstrumentPicker
                        selection={asset.instrument}
                        onDismiss={(instrument) => {
                            codeMirrorRef.current?.editorView.dispatch({
                                changes: {
                                    from: start,
                                    to: end,
                                    insert: instrumentAsset(instrument),
                                },
                            });

                            dismiss();
                        }}
                    />
                ));

                break;
            }
            case "object": {
                displayAlert(({ dismiss }) => (
                    <ObjectPicker
                        selection={asset.object}
                        onDismiss={(object) => {
                            codeMirrorRef.current?.editorView.dispatch({
                                changes: { from: start, to: end, insert: objectAsset(object) },
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

    const onAddLine = (position: "start" | "end") => {
        if (!codeMirrorRef.current) {
            return;
        }

        const editorView = codeMirrorRef.current.editorView;

        props.onChange(position === "start" ? "\n" + props.children : props.children + "\n");

        editorView.focus();
        editorView.dispatch({
            selection: {
                anchor: position === "start" ? 0 : editorView.state.doc.length,
            },
        });
    };

    const runCommand = (command: StateCommand) => {
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

    const [help, setHelp] = useState<{ rect: Rect; help: Help }>();
    const [helpVisible, setHelpVisible] = useState(false);

    const {
        refs: helpFloatingRefs,
        floatingStyles: helpFloatingStyles,
        context: helpFloatingContext,
    } = useFloating({
        open: help != null,
        placement: "bottom",
        middleware: [shift({ padding: 8 })],
    });

    const { getFloatingProps: getHelpFloatingProps } = useInteractions([
        useClientPoint(helpFloatingContext, {
            enabled: help != null,
            x: ((help?.rect.left ?? 0) + (help?.rect.right ?? 0)) / 2,
            y: help?.rect.bottom ?? 0,
        }),
    ]);

    const handleLongPress = useCallback(async (pos: number, rect: Rect) => {
        setHelp(undefined);

        if (!codeMirrorRef.current || !runnerRef.current) {
            return;
        }

        const code = getTokenAtPos(codeMirrorRef.current.editorView.state, pos);

        const help = await runnerRef.current.help(pos, code);
        if (!help) {
            return;
        }

        setHelp({ rect, help });
        setHelpVisible(true);
    }, []);

    const dismissHelp = useCallback(() => {
        setHelpVisible(false);

        setTimeout(() => {
            setHelp(undefined);
        }, defaultAnimationDuration);
    }, []);

    useEffect(() => {
        const handleMousedown = (event: MouseEvent) => {
            if (
                helpFloatingRefs.floating.current != null &&
                !helpFloatingRefs.floating.current.contains(event.target as HTMLElement)
            ) {
                dismissHelp();
            }
        };

        const handleKeydown = (event: KeyboardEvent) => {
            if (event.key === "Escape") {
                dismissHelp();
            }
        };

        window.addEventListener("mousedown", handleMousedown);
        window.addEventListener("keydown", handleKeydown);

        return () => {
            window.removeEventListener("mousedown", handleMousedown);
            window.removeEventListener("keydown", handleKeydown);
        };
    }, [helpFloatingRefs, dismissHelp]);

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
        setShowStatus(!isCompiling);

        if (!isCompiling) {
            setHasEdited(false);
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
                const pdf = await html2pdf()
                    .set({
                        filename: `wipple-${formatDate(new Date(), "yyyyMMddHHmmss")}.pdf`,
                        margin: 8,
                        enableLinks: false,
                        html2canvas: { scale: 4 },
                    })
                    .from(contentRef.current)
                    .toPdf()
                    .get("pdf");

                window.open(pdf.output("bloburl"), "_blank");
            } finally {
                setStore(
                    produce((store) => {
                        store.isPrinting = false;
                    }),
                );
            }
        }
    }, []);

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

                    insertSnippet(codeMirrorRef.current.editorView, snippet, line);

                    format();
                }
            }}
        >
            <div
                className="flex flex-row justify-stretch w-full border-2 border-gray-100 dark:border-gray-800 rounded-md overflow-clip"
                tabIndex={0}
                autoFocus={isFocused}
                onFocus={() => setFocused(true)}
                onBlur={() => setFocused(false)}
            >
                <Sidebar
                    theme={props.theme}
                    paletteCategories={runtime?.paletteCategories ?? defaultPaletteCategories}
                    highlightItems={highlightItems}
                    draggedCommand={draggedCommand}
                    onBeginDraggingCommand={(id, item) => setDraggedCommand({ id, item })}
                    isRunning={isCompilingOrRunning}
                    onClickRun={handleRun}
                    showStatus={showStatus}
                    diagnostics={diagnostics}
                    onSelectDiagnostic={setSelectedDiagnostic}
                    codeForDiagnostic={codeForDiagnostic}
                    hasEdited={hasEdited}
                    optionsHandlers={{
                        onPrint: print,
                        onFormat: format,
                        onSelectAll: () => runCommand(commands.selectAll),
                        onUndo: () => runCommand(commands.undo),
                        onRedo: () => runCommand(commands.redo),
                        onMoveUp: props.onMoveUp,
                        onMoveDown: props.onMoveDown,
                        onReset: props.onReset,
                        onDelete: props.onDelete,
                    }}
                />

                <div ref={contentRef} className="flex-1 flex flex-col overflow-visible">
                    {store.isPrinting ? <PrintHeader /> : null}

                    <div className="relative px-4">
                        <AddLineButton
                            direction="start"
                            disabled={draggedCommand != null || (props.readOnly ?? false)}
                            onClick={() => onAddLine("start")}
                        />

                        <CodeMirror
                            ref={codeMirrorRef}
                            autoFocus
                            onChange={(value) => {
                                setHasEdited(true);
                                props.onChange(value);
                            }}
                            onLongPress={handleLongPress}
                            readOnly={props.readOnly ?? false}
                            onClickAsset={onClickAsset}
                            theme={props.theme}
                            highlightedCode={highlightedCode}
                            highlightItems={highlightItems}
                        >
                            {props.children}
                        </CodeMirror>

                        {draggedCommand ? (
                            <div className="absolute inset-0 bg-blue-500 bg-opacity-10 mb-2">
                                {draggedCommand.item.replace ? (
                                    <DropTargetArea />
                                ) : (
                                    <DropTargetLines
                                        numberOfLines={numberOfLines}
                                        theme={props.theme}
                                    />
                                )}
                            </div>
                        ) : null}

                        <AddLineButton
                            direction="end"
                            disabled={draggedCommand != null || (props.readOnly ?? false)}
                            onClick={() => onAddLine("end")}
                        />
                    </div>

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

            {help ? (
                <FloatingPortal>
                    <div
                        ref={helpFloatingRefs.setFloating}
                        style={helpFloatingStyles}
                        {...getHelpFloatingProps()}
                        className="z-20"
                    >
                        <div style={{ marginTop: 4 }}>
                            <TooltipContent open={helpVisible}>
                                {help.help.summary}

                                <button
                                    className="inline-flex items-center justify-center ml-2 w-5 h-5 align-bottom rounded-lg bg-blue-100 dark:bg-blue-900 text-blue-500 text-lg"
                                    onClick={() => {
                                        dismissHelp();

                                        displayAlert(({ dismiss }) => (
                                            <HelpAlert help={help.help} dismiss={dismiss} />
                                        ));
                                    }}
                                >
                                    <MaterialSymbol icon="more_horiz" />
                                </button>
                            </TooltipContent>
                        </div>
                    </div>
                </FloatingPortal>
            ) : null}

            <DragOverlay className="w-full">
                {draggedCommand ? (
                    <div className="w-screen h-screen">
                        <div
                            className="flex flex-row items-center bg-white dark:bg-gray-800 w-fit h-fit -mx-1 px-1 rounded-md shadow-lg"
                            style={{
                                fontFamily: props.theme.fontFamily,
                                fontSize: props.theme.fontSize,
                                lineHeight: props.theme.lineHeight,
                            }}
                        >
                            <CommandPreviewContent
                                code={draggedCommand.item.code}
                                theme={props.theme}
                                highlightItems={highlightItems}
                            />
                        </div>
                    </div>
                ) : null}
            </DragOverlay>
        </DndContext>
    );
}

const Sidebar = (props: {
    theme: ThemeConfig;
    paletteCategories: PaletteCategory[];
    highlightItems: Record<string, any>;
    draggedCommand: { id: string; item: PaletteItem } | undefined;
    onBeginDraggingCommand: (id: string, item: PaletteItem) => void;
    isRunning: boolean;
    onClickRun: (() => void) | undefined;
    showStatus: boolean;
    diagnostics: any[];
    onSelectDiagnostic: (diagnostic: any) => void;
    codeForDiagnostic: (diagnostic: any) => string;
    hasEdited: boolean;
    optionsHandlers: {
        onPrint: () => void;
        onFormat: () => void;
        onSelectAll: () => void;
        onUndo: () => void;
        onRedo: () => void;
        onSuggestFixes?: () => void;
        onMoveUp?: () => void;
        onMoveDown?: () => void;
        onReset?: () => void;
        onDelete?: () => void;
    };
}) => {
    const scrollContainerRef = useRef<HTMLDivElement>(null);

    return (
        <div className="flex flex-col items-stretch w-52 min-h-80">
            <div className="flex-1 flex flex-col items-stretch bg-gray-50 dark:bg-gray-900 pt-4 px-4">
                <div className="flex flex-row gap-2.5">
                    <RunButton isRunning={props.isRunning} onClick={props.onClickRun} />

                    <OptionsButton
                        onPrint={props.optionsHandlers.onPrint}
                        onFormat={props.optionsHandlers.onFormat}
                        onSelectAll={props.optionsHandlers.onSelectAll}
                        onUndo={props.optionsHandlers.onUndo}
                        onRedo={props.optionsHandlers.onRedo}
                        onSuggestFixes={props.optionsHandlers.onSuggestFixes}
                        onMoveUp={props.optionsHandlers.onMoveUp}
                        onMoveDown={props.optionsHandlers.onMoveDown}
                        onReset={props.optionsHandlers.onReset}
                        onDelete={props.optionsHandlers.onDelete}
                    />
                </div>

                <div className="flex-1 relative w-full">
                    <div
                        ref={scrollContainerRef}
                        className="absolute inset-0 overflow-y-scroll no-scrollbar flex flex-col gap-2.5 pt-2.5 pb-4"
                    >
                        <Animated direction="vertical" open={props.showStatus}>
                            {props.diagnostics.length > 0 ? (
                                <ErrorBrowser
                                    diagnostics={props.diagnostics}
                                    active={!props.hasEdited}
                                    onSelectDiagnostic={props.onSelectDiagnostic}
                                    codeForDiagnostic={props.codeForDiagnostic}
                                    scrollContainerRef={scrollContainerRef}
                                />
                            ) : (
                                <NoErrors active={!props.hasEdited} />
                            )}
                        </Animated>

                        <CommandPalette
                            categories={props.paletteCategories}
                            theme={props.theme}
                            highlightItems={props.highlightItems}
                            draggedCommand={props.draggedCommand}
                            onBeginDraggingCommand={props.onBeginDraggingCommand}
                        />
                    </div>

                    <div className="absolute left-0 right-0 top-0 h-4 bg-gradient-to-b from-gray-50 dark:from-gray-900 to-transparent pointer-events-none" />

                    <div className="absolute left-0 right-0 bottom-0 h-4 bg-gradient-to-t from-gray-50 dark:from-gray-900 to-transparent pointer-events-none" />
                </div>
            </div>
        </div>
    );
};

const RunButton = (props: { isRunning: boolean; onClick: (() => void) | undefined }) => (
    <button
        onClick={props.onClick}
        disabled={props.onClick == null}
        className={`flex-1 flex flex-row items-center justify-center gap-0.5 disabled:bg-gray-200 dark:disabled:bg-gray-700 transition-colors text-white disabled:text-gray-400 dark:disabled:text-gray-600 rounded-lg ${
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
    onPrint: () => void;
    onFormat: () => void;
    onSelectAll: () => void;
    onUndo: () => void;
    onRedo: () => void;
    onSuggestFixes?: () => void;
    onMoveUp?: () => void;
    onMoveDown?: () => void;
    onReset?: () => void;
    onDelete?: () => void;
}) => (
    <div className="aspect-square">
        <ContextMenuButton
            items={[
                {
                    title: "Print",
                    icon: "print",
                    onClick: props.onPrint,
                },
                {
                    title: "Format",
                    icon: "format_align_left",
                    onClick: props.onFormat,
                },
                {
                    title: "Select All",
                    shortcut: {
                        mac: "⌘ A",
                        win: "Ctrl A",
                    },
                    icon: "select_all",
                    tutorialItemId: "selectAll",
                    onClick: props.onSelectAll,
                },
                {
                    title: "Undo",
                    shortcut: {
                        mac: "⌘ Z",
                        win: "Ctrl Z",
                    },
                    icon: "undo",
                    onClick: props.onUndo,
                },
                {
                    title: "Redo",
                    shortcut: {
                        mac: "⇧ ⌘ Z",
                        win: "Ctrl Y",
                    },
                    icon: "redo",
                    onClick: props.onRedo,
                },
                {
                    title: "Move Up",
                    icon: "arrow_upward",
                    disabled: props.onMoveUp == null,
                    onClick: () => props.onMoveUp!(),
                },
                {
                    title: "Move Down",
                    icon: "arrow_downward",
                    disabled: props.onMoveDown == null,
                    onClick: () => props.onMoveDown!(),
                },
                {
                    title: "Reset",
                    icon: "restart_alt",
                    role: "destructive",
                    onClick: props.onReset,
                    disabled: props.onReset == null,
                },
                {
                    title: "Delete",
                    icon: "delete",
                    role: "destructive",
                    onClick: props.onDelete,
                },
            ]}
        >
            <button className="flex items-center justify-center bg-blue-100 dark:bg-blue-950 hover:bg-blue-500 text-blue-500 hover:text-white transition-colors rounded-lg w-full h-full">
                <MaterialSymbol icon="more_horiz" fill className="text-2xl" />
            </button>
        </ContextMenuButton>
    </div>
);

const ErrorBrowser = (props: {
    diagnostics: any[];
    onSelectDiagnostic: (diagnostic: any) => void;
    codeForDiagnostic: (diagnostic: any) => string;
    active: boolean;
    scrollContainerRef: React.RefObject<HTMLDivElement>;
}) => {
    const [activeIndex, setActiveIndex] = useState(0);

    const activeDiagnostic = props.diagnostics[activeIndex];

    useEffect(() => {
        props.onSelectDiagnostic(activeDiagnostic);
    }, [activeDiagnostic]);

    const [resolved, setResolved] = useState<{
        template: DiagnosticTemplate;
        data: Record<string, string>;
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
        <div
            className={`flex flex-col rounded-lg border-2 transition-colors ${
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
                    <ErrorTemplateView
                        template={resolved.template}
                        data={resolved.data}
                        scrollContainerRef={props.scrollContainerRef}
                    />
                ) : (
                    <Markdown>{`Unknown error: \`${activeDiagnostic.template.id}\``}</Markdown>
                )}
            </div>
        </div>
    );
};

const ErrorTemplateView = (props: {
    template: DiagnosticTemplate;
    data: Record<string, string>;
    scrollContainerRef: React.RefObject<HTMLDivElement>;
}) => {
    // TODO: Select a variant at random for experiments
    const variant = useMemo(() => props.template.variants[0], [props.template]);

    const [history, setHistory] = useState<
        {
            help: DiagnosticHelp;
            data: Record<string, string>;
        }[]
    >([]);

    useEffect(() => {
        setHistory([]);
    }, [variant]);

    const handleNext = useCallback(
        (help: DiagnosticHelp, data: Record<string, string>) => {
            if (help) {
                setHistory((history) => [
                    ...history,
                    {
                        help,
                        data: { ...props.data, ...data },
                    },
                ]);
            } else {
                setHistory([]);

                // Scroll back to the top
                props.scrollContainerRef.current?.scrollTo({
                    top: 0,
                    behavior: "smooth",
                });
            }
        },
        [props.data, props.scrollContainerRef],
    );

    return (
        <div className="flex flex-col items-start gap-2">
            <div className="flex flex-col">
                <TemplateView className="font-semibold" data={props.data}>
                    {variant.title}
                </TemplateView>

                <TemplateView className="opacity-75" data={props.data}>
                    {variant.description}
                </TemplateView>
            </div>

            {history.length === 0 && variant.help != null ? (
                <div className="flex flex-row gap-2">
                    <ErrorTemplateButton
                        onClick={() => setHistory([{ help: variant.help, data: props.data }])}
                    >
                        Help
                    </ErrorTemplateButton>
                </div>
            ) : (
                history.map((item, index) => (
                    <ErrorHelpView
                        key={index}
                        help={item.help}
                        data={item.data}
                        onNext={index === history.length - 1 ? handleNext : undefined}
                        scrollContainerRef={props.scrollContainerRef}
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
    data: Record<string, string>;
    onNext?: (next: DiagnosticHelp, data: Record<string, string>) => void;
    scrollContainerRef: React.RefObject<HTMLDivElement>;
}) => {
    const containerRef = useRef<HTMLDivElement>(null);

    // Scroll into view
    useEffect(() => {
        setTimeout(() => {
            if (containerRef.current && props.scrollContainerRef.current) {
                const offset =
                    containerRef.current.getBoundingClientRect().top -
                    props.scrollContainerRef.current.getBoundingClientRect().top +
                    props.scrollContainerRef.current.scrollTop -
                    24;

                props.scrollContainerRef.current.scrollTo({
                    top: offset,
                    behavior: "smooth",
                });
            }
        }, defaultAnimationDuration * 2);
    }, []);

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
                    <TemplateView data={props.data}>{props.help.message}</TemplateView>

                    {props.onNext ? (
                        <ErrorTemplateButton onClick={() => props.onNext!(undefined, {})}>
                            Reset
                        </ErrorTemplateButton>
                    ) : null}
                </>,
            );
        case "choice":
            return makeContainer(
                <>
                    <TemplateView data={props.data}>{props.help.question}</TemplateView>

                    <div className="flex flex-row gap-1">
                        {props.help.choices.map((choice, index) => (
                            <ErrorTemplateButton
                                key={index}
                                onClick={
                                    props.onNext ? () => props.onNext!(choice.then, {}) : undefined
                                }
                            >
                                {choice.name}
                            </ErrorTemplateButton>
                        ))}
                    </div>
                </>,
            );
        case "prompt":
            const help = props.help;

            return makeContainer(
                <>
                    <TemplateView data={props.data}>{props.help.question}</TemplateView>

                    <ErrorPrompt
                        onSubmit={
                            props.onNext
                                ? (answer) => {
                                      props.onNext!(help.then, { ...props.data, answer });
                                  }
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

const TemplateView = (props: {
    className?: string;
    data: Record<string, string>;
    children: string;
}) => {
    const rendered = useMemo(
        () => Mustache.render(props.children, props.data),
        [props.data, props.children],
    );

    return <Markdown className={props.className}>{rendered}</Markdown>;
};

const NoErrors = (props: { active: boolean }) => (
    <div
        className={`flex flex-row items-center justify-center gap-1.5 p-1.5 rounded-lg text-sm font-semibold border-2 transition-colors ${
            props.active
                ? "bg-green-50 dark:bg-green-950 border-green-300 dark:border-green-700 text-green-600 dark:text-green-400"
                : "bg-gray-100 dark:bg-gray-800 border-gray-200 dark:border-gray-700 text-gray-600 dark:text-gray-400"
        }`}
    >
        No errors
    </div>
);

const CommandPalette = (props: {
    categories: PaletteCategory[];
    theme: ThemeConfig;
    highlightItems: Record<string, any>;
    draggedCommand: { id: string; item: PaletteItem } | undefined;
    onBeginDraggingCommand: (id: string, item: PaletteItem) => void;
}) => {
    const [selection, setSelection] = useState(props.categories[0]);

    const allCommandsCategory: PaletteCategory = useMemo(
        () => ({
            title: "All Commands",
            items: props.categories.flatMap((category) => category.items),
        }),
        [props.categories],
    );

    return (
        <div className="flex flex-col bg-white dark:bg-gray-800 border-2 border-gray-100 dark:border-gray-800 rounded-lg px-3 py-1">
            <ContextMenuButton
                items={[...props.categories, allCommandsCategory].map((category) => ({
                    title: category.title,
                    onClick: () => setSelection(category),
                }))}
                className="z-50 cursor-pointer"
            >
                <div className="-ml-2 pl-2 w-fit flex flex-row items-center gap-0.5 rounded-lg hover:bg-gray-50 dark:hover:bg-gray-700 transition-colors">
                    <span className="text-sm">{selection.title}</span>

                    <MaterialSymbol
                        icon="keyboard_arrow_down"
                        fill
                        className="text-xl text-blue-500"
                    />
                </div>
            </ContextMenuButton>

            {selection.items.map((item) => (
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
    );
};

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
    <div className="w-fit hover:drop-shadow-sm transition">
        <div className="w-fit pointer-events-none">
            <CodeMirror
                autoFocus={false}
                onChange={() => {}}
                onLongPress={() => {}}
                readOnly={true}
                onClickAsset={() => {}}
                theme={props.theme}
                highlightItems={props.highlightItems}
            >
                {props.code}
            </CodeMirror>
        </div>
    </div>
);

const AddLineButton = (props: {
    direction: "start" | "end";
    disabled: boolean;
    onClick: () => void;
}) => (
    <Tooltip
        description="Add Line"
        onClick={props.onClick}
        disabled={props.disabled}
        className={props.disabled ? "pointer-events-none" : ""}
    >
        <div
            className={`group flex w-full cursor-vertical-text ${
                props.direction === "start" ? "pt-1.5 pb-1" : "pt-1 pb-2.5"
            }`}
        >
            <div className="w-full h-1 bg-blue-500 rounded-full opacity-0 group-hover:opacity-100 transition-opacity" />
        </div>
    </Tooltip>
);

const DropTargetLines = (props: { numberOfLines: number; theme: ThemeConfig }) => (
    <div className="absolute top-0 left-0 right-0 flex flex-col">
        <div className="pt-1.5 pb-1">
            <DropTargetLine index={-1} theme={props.theme} />
        </div>

        {new Array(props.numberOfLines).fill(undefined).map((_, index) => (
            <DropTargetLine key={index} index={index} useLineHeight theme={props.theme} />
        ))}
    </div>
);

const DropTargetLine = (props: { index: number; useLineHeight?: boolean; theme: ThemeConfig }) => {
    const { setNodeRef, isOver } = useDroppable({ id: props.index });

    return (
        <div
            ref={setNodeRef}
            className={`relative ${!props.useLineHeight ? "h-1" : ""}`}
            style={{
                height: props.useLineHeight
                    ? props.theme.fontSize * props.theme.lineHeight
                    : undefined,
            }}
        >
            <div className="absolute left-0 right-0 bottom-0">
                <div
                    className={`mx-4 w-full h-1 bg-blue-500 rounded-full transition-opacity ${
                        isOver ? "opacity-100" : "opacity-0"
                    }`}
                />
            </div>
        </div>
    );
};

const DropTargetArea = () => {
    const { setNodeRef, isOver } = useDroppable({ id: 0 });

    return (
        <div ref={setNodeRef} className="absolute inset-0 p-2">
            <div
                className={`w-full h-full border-[3px] rounded-md border-blue-500 transition-opacity ${
                    isOver ? "border-opacity-100" : "border-opacity-0"
                }`}
            />
        </div>
    );
};

const PrintHeader = () => {
    const [store, _setStore] = useStore();

    return (
        <div className="flex flex-col p-4 bg-gray-50 rounded-lg">
            <img src="/playground/images/logo.svg" className="w-16 h-16 mb-4" />

            <p className="text-xl font-semibold">
                {store.user?.displayName
                    ? `Created by ${store.user.displayName} with Wipple`
                    : "Created with Wipple"}
            </p>

            <p className="text-lg text-gray-500 font-semibold">
                Create your own at <span className="text-blue-500">wipple.org</span>
            </p>
        </div>
    );
};
