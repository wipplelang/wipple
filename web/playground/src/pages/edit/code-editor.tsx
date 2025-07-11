import { MouseEventHandler, useCallback, useEffect, useMemo, useRef, useState } from "react";
import { CodeMirror, CodeMirrorRef, HighlightedCode } from "./codemirror";
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
import { Command, EditorView } from "@codemirror/view";
import html2pdf from "html-to-pdf-js";
import { format as formatDate } from "date-fns";
import { useStore } from "../../store";
import { produce } from "immer";
import { Box } from "../../components/box";
import { ToolbarButton } from "../../components/toolbar-button";
import { FloatingPortal } from "@floating-ui/react";

interface DragInfo {
    id: string;
    paletteItem: PaletteItem;
    x: number;
    y: number;
}

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

    const [dragInfo, setDragInfo] = useState<DragInfo>();

    const dropParams = useMemo(() => {
        if (!codeMirrorRef.current || !dragInfo) {
            return undefined;
        }

        const dropParams = getDropParams(codeMirrorRef.current.editorView, dragInfo.paletteItem, {
            x: dragInfo.x,
            y: dragInfo.y,
        });

        return dropParams;
    }, [dragInfo]);

    const dropParamsRef = useRef(dropParams);
    useEffect(() => {
        dropParamsRef.current = dropParams;
    }, [dropParams]);

    const onDrop = useCallback(() => {
        if (codeMirrorRef.current && dropParamsRef.current) {
            drop(codeMirrorRef.current.editorView, dropParamsRef.current);
            format();
        }
    }, []);

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
        <div
            ref={contentRef}
            data-printing={store.isPrinting || undefined}
            className={`flex flex-col w-full h-full gap-2.5 ${
                store.isPrinting ? "p-[0.5in] overflow-hidden" : ""
            }`}
            style={{
                userSelect: dragInfo ? "none" : "auto",
                pointerEvents: dragInfo ? "none" : "auto",
            }}
        >
            {store.isPrinting ? <PrintHeader /> : null}

            <div
                className={`flex-1 flex flex-row justify-stretch p-2.5 gap-2.5 h-full ${
                    store.isPrinting ? "overflow-hidden" : ""
                }`}
            >
                {!store.isPrinting ? (
                    <div className="h-full w-[240px] opacity-1">
                        <div className="flex flex-col w-[240px] gap-2.5">
                            <CommandPalette
                                theme={theme}
                                categories={runtime?.paletteCategories ?? defaultPaletteCategories}
                                highlightItems={highlightItems}
                                dragInfo={dragInfo}
                                onChangeDragInfo={setDragInfo}
                                onDrop={onDrop}
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

                    <Box fill padding={false}>
                        <div
                            className={`flex flex-col *:h-full inset-0 w-full h-full gap-2.5 p-4 ${
                                dragInfo ? "bg-blue-500/10" : ""
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

                            {dropParams ? (
                                <div
                                    className={`border-blue-500 fixed border-collapse rounded-[6px] ${
                                        dropParams.startLineNumber === dropParams.endLineNumber
                                            ? "border-[1px]"
                                            : "border-[2px]"
                                    }`}
                                    style={{
                                        top: dropParams.top,
                                        left: dropParams.left,
                                        width: dropParams.width,
                                        height: dropParams.height,
                                    }}
                                ></div>
                            ) : null}
                        </div>
                    </Box>
                </div>

                <div className={store.isPrinting ? "h-full" : "basis-[450px] h-full"}>
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
        </div>
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
                    <ErrorMarkdown diagnostic={activeDiagnostic} />
                </div>
            </div>
        </div>
    );
};

const ErrorMarkdown = (props: { diagnostic: any }) => (
    <div className="flex flex-col items-start">
        <Markdown className="font-semibold">{props.diagnostic.message}</Markdown>
        <Markdown className="opacity-75">{props.diagnostic.description}</Markdown>
    </div>
);

const CommandPalette = (props: {
    header?: JSX.Element;
    categories: PaletteCategory[];
    theme: ThemeConfig;
    highlightItems: Record<string, any>;
    dragInfo?: DragInfo;
    onChangeDragInfo: (dragInfo: DragInfo | undefined) => void;
    onDrop: () => void;
}) => (
    <div className="flex-[1.5] flex flex-col h-full gap-2.5 z-10">
        <Logo />

        <Box>
            {props.categories.map((category) => (
                <div key={category.title} className="flex flex-col mb-2.5">
                    <p className="text-gray-600 dark:text-gray-400 text-sm mb-1">
                        {category.title}
                    </p>

                    {category.items.map((item) => (
                        <div key={item.title}>
                            <CommandPreview
                                key={item.title}
                                item={item}
                                theme={props.theme}
                                highlightItems={props.highlightItems}
                                dragInfo={props.dragInfo}
                                onChangeDragInfo={props.onChangeDragInfo}
                                onDrop={props.onDrop}
                            />
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
    dragInfo?: DragInfo;
    onChangeDragInfo: (dragInfo: DragInfo | undefined) => void;
    onDrop: () => void;
}) => {
    const stickyThresholdTime = 400; // ms
    const stickyThresholdDistance = 10; // px

    const id = useMemo(() => nanoid(), []);

    const isDragging = useMemo(() => props.dragInfo?.id === id, [props.dragInfo, id]);

    const onMouseDown: MouseEventHandler = (e) => {
        const startTimestamp = e.timeStamp;
        const startX = e.clientX;

        const update = (e: MouseEvent) => {
            props.onChangeDragInfo({ id, x: e.clientX, y: e.clientY, paletteItem: props.item });
        };

        const onmousemove = (e: MouseEvent) => {
            update(e);
        };

        const end = (drop: boolean) => {
            if (drop) {
                props.onDrop();
            }

            window.removeEventListener("mousemove", onmousemove);
            window.removeEventListener("mouseup", onmouseup);
            window.removeEventListener("keydown", onkeydown);
            props.onChangeDragInfo(undefined);
        };

        const onmouseup = (e: MouseEvent) => {
            if (
                e.timeStamp - startTimestamp > stickyThresholdTime ||
                Math.abs(e.clientX - startX) > stickyThresholdDistance
            ) {
                end(true);
            } else {
                // If the mouse is clicked instead of dragged, treat it as "sticky"
                setTimeout(() => {
                    window.addEventListener("click", () => end(true), { once: true });
                }, stickyThresholdTime);
            }
        };

        const onkeydown = (e: KeyboardEvent) => {
            if (e.key === "Escape") {
                end(false);
            }
        };

        window.addEventListener("mousemove", onmousemove);
        window.addEventListener("mouseup", onmouseup, { once: true });
        window.addEventListener("keydown", onkeydown);

        requestAnimationFrame(() => {
            update(e.nativeEvent);
        });
    };

    return (
        <div
            className="hover-highlight -mx-[4px] cursor-pointer rounded-[8px] px-[2px]"
            style={{ visibility: isDragging ? "hidden" : "visible" }}
            onMouseDown={onMouseDown}
        >
            {isDragging ? (
                <FloatingPortal root={document.body}>
                    <div
                        className="border-[1.5px] border-gray-100 dark:border-gray-900 bg-white dark:bg-gray-950 shadow-lg shadow-black/2.5 pointer-events-none fixed size-max rounded-[8px] px-1 z-50"
                        style={{ top: props.dragInfo!.y + "px", left: props.dragInfo!.x + "px" }}
                    >
                        <CommandPreviewContent
                            code={props.item.code}
                            theme={props.theme}
                            highlightItems={props.highlightItems}
                        />
                    </div>
                </FloatingPortal>
            ) : null}

            <div className="pointer-events-none size-full">
                <CommandPreviewContent
                    code={props.item.title}
                    theme={props.theme}
                    highlightItems={props.highlightItems}
                />
            </div>
        </div>
    );
};

const CommandPreviewContent = (props: {
    code: string;
    theme: ThemeConfig;
    highlightItems: Record<string, any>;
}) => (
    <div className="hover:bg-gray-100 dark:hover:bg-gray-800 transition -mx-1 p-1 rounded-lg">
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

export const getDropParams = (
    editorView: EditorView,
    item: PaletteItem,
    { x, y }: { x: number; y: number },
) => {
    const { top, bottom, left, right } = editorView.contentDOM.getBoundingClientRect();
    const width = editorView.contentDOM.clientWidth;

    if (x < left || x > right || y < top || y > bottom) {
        // Out of bounds
        return undefined;
    }

    // Insert at beginning if document is empty
    if (editorView.state.doc.length === 0) {
        return {
            item,
            startLineNumber: 1,
            endLineNumber: 1,
            top: top + "px",
            left: left + "px",
            width: width + "px",
            height: "0",
        };
    }

    const lineHeight = parseFloat(
        window
            .getComputedStyle(editorView.contentDOM)
            .getPropertyValue("--code-editor-line-height")
            .replace(/px$/, ""),
    );

    const lineSpacing = parseFloat(
        window
            .getComputedStyle(editorView.contentDOM)
            .getPropertyValue("--code-editor-line-spacing")
            .replace(/px$/, ""),
    );

    // Choose the closest line, and allow going one past the end
    const startLineNumber =
        Math.min(Math.floor((y - top) / (lineHeight + lineSpacing)), editorView.state.doc.lines) +
        1;

    let endLineNumber = startLineNumber;
    if (item.replace && endLineNumber <= editorView.state.doc.lines) {
        // Select until the next closing brace (or the end of the document)

        const iter = editorView.state.doc.iterRange(
            editorView.state.doc.line(startLineNumber).from,
        );

        let openingBraceCount = 0;
        for (const line of iter) {
            if (iter.lineBreak) {
                endLineNumber++;
                continue;
            }

            openingBraceCount += line.match("{")?.length ?? 0;
            openingBraceCount -= line.match("}")?.length ?? 0;

            if (openingBraceCount < 0) {
                break;
            }
        }

        endLineNumber += openingBraceCount + 1;

        endLineNumber = Math.min(endLineNumber, editorView.state.doc.lines + 1);
    }

    const offset = (lineNumber: number) =>
        `${top + (lineHeight + lineSpacing) * (lineNumber - 1)}px`;

    return {
        item,
        startLineNumber,
        endLineNumber,
        top: offset(startLineNumber),
        left: left + "px",
        width: width + "px",
        height: `calc(${offset(endLineNumber)} - ${offset(startLineNumber)})`,
    };
};

export const drop = (
    editorView: EditorView,
    params: NonNullable<ReturnType<typeof getDropParams>>,
) => {
    const { item: command, startLineNumber, endLineNumber } = params;

    if (command.replace) {
        const from =
            startLineNumber <= editorView.state.doc.lines
                ? editorView.state.doc.line(startLineNumber).from
                : editorView.state.doc.length;

        const to =
            endLineNumber <= editorView.state.doc.lines
                ? editorView.state.doc.line(endLineNumber).to
                : editorView.state.doc.length;

        const inner = editorView.state.sliceDoc(from, to);

        const before = startLineNumber <= editorView.state.doc.lines ? "" : "\n";

        const code = command.code.replace(/\b_\b/, inner);

        editorView.dispatch({
            changes: { from, to, insert: before + code },
        });
    } else if (startLineNumber <= 1) {
        editorView.dispatch({
            changes: { from: 0, to: 0, insert: command.code + "\n" },
        });
    } else {
        const pos =
            startLineNumber <= editorView.state.doc.lines
                ? editorView.state.doc.line(startLineNumber - 1).to
                : editorView.state.doc.length;

        editorView.dispatch({
            changes: { from: pos, to: pos, insert: "\n" + command.code },
        });
    }
};
