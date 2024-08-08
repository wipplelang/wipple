import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import {
    Animated,
    Ansi,
    Button,
    ContextMenuButton,
    ContextMenuItem,
    Markdown,
    Tooltip,
    Transition,
    TutorialItem,
    defaultAnimationDuration,
    useAlert,
} from "../components";
import { CodeMirror, CodeMirrorRef } from "./codemirror";
import * as commands from "@codemirror/commands";
import { RunOptions, Runner, RunnerRef } from "./runner";
import { MaterialSymbol } from "react-material-symbols";
import { ThemeConfig } from "./codemirror/theme";
import { Help, IntelligentFix, PaletteItem } from "../models";
import { useWindowSize } from "usehooks-ts";
import { HelpAlert } from "./help-alert";
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
import { defaultPaletteItems, runtimes } from "../runtimes";
import { SetupIcon } from "./setup-icon";
import { StateCommand } from "@codemirror/state";
import actionsIcon from "./assets/actions.svg";
import lookupIcon from "./assets/lookup.svg";
import { NotePicker } from "./note-picker";
import { AnimalPicker } from "./animal-picker";
import { InstrumentPicker } from "./instrument-picker";
import { ObjectPicker } from "./object-picker";
import { ErrorDoc, docForError } from "../docs/errors";
import { FloatingPortal } from "@floating-ui/react";
import { produce } from "immer";

export function CodeEditor<Settings>(props: {
    children: string;
    wipple: typeof import("wipple-wasm");
    onChange: (value: string) => void;
    theme: ThemeConfig;
    locked?: boolean;
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
    onDelete: () => void;
    onReset?: () => void;
}) {
    const [isFocused, setFocused] = useState(props.autofocus ?? false);

    useEffect(() => {
        if (isFocused) {
            props.onFocus?.();
        } else {
            props.onBlur?.();
        }
    }, [isFocused]);

    const [runOptions, setRunOptions] = useState<RunOptions>({
        dependenciesPath: props.runtime?.name ?? "base",
    });

    const [runnerHasFocus, setRunnerHasFocus] = useState(false);

    const [diagnostics, setDiagnostics] = useState<any[]>([]);
    const [driverDiagnostics, setDriverDiagnostics] = useState<any[]>([]);
    const [highlightItems, setHighlightItems] = useState<Record<string, any>>({});

    const [lookUpEnabled, setLookUpEnabled] = useState(false);

    const codeMirrorRef = useRef<CodeMirrorRef>(null);
    const runnerRef = useRef<RunnerRef>(null);

    const windowSize = useWindowSize();

    const lineDiagnostics = useMemo(() => {
        if (driverDiagnostics.length !== diagnostics.length) {
            return [];
        }

        const editorView = codeMirrorRef.current?.editorView;
        if (!editorView) {
            return [];
        }

        const editorRect = editorView.dom.getBoundingClientRect();

        const coveredLines: number[] = [];

        return diagnostics.flatMap((diagnostic, index) => {
            const driverDiagnostic = driverDiagnostics[index];

            if (diagnostic.location.start.index > editorView.state.doc.length) {
                return [];
            }

            const line = editorView.state.doc.lineAt(diagnostic.location.start.index);

            if (coveredLines.includes(line.number)) {
                return [];
            }

            coveredLines.push(line.number);

            const top = editorView.coordsAtPos(diagnostic.location.start.index)?.top;
            if (!top) {
                return [];
            }

            const lineStartRect = editorView.coordsAtPos(line.from)!;
            const lineEndRect = editorView.coordsAtPos(line.to)!;

            const width = lineEndRect.right - lineStartRect.left;
            const height = lineEndRect.bottom - lineStartRect.top;

            return [
                {
                    top: top + window.scrollY,
                    right: windowSize.width - editorRect.right,
                    width: Math.max(editorRect.width - width - 48, 32),
                    height,
                    diagnostic,
                    driverDiagnostic,
                },
            ];
        });
    }, [diagnostics, driverDiagnostics, windowSize]);

    const applyFix = (fix: any, start: number, end: number) => {
        const editorView = codeMirrorRef.current?.editorView;
        if (!editorView) {
            return;
        }

        let replacement = editorView.state.sliceDoc(start, end);
        if (fix.replacement) {
            replacement = fix.replacement.replace(/\b_\b/, replacement);
        }

        replacement = (fix.before ?? "") + replacement + (fix.after ?? "");

        editorView.dispatch({
            changes: { from: start, to: end, insert: replacement },
        });
    };

    const [lineIntelligentFixes, setLineIntelligentFixes] = useState<
        {
            top: number;
            right: number;
            width: number;
            height: number;
            intelligentFix: IntelligentFix;
        }[]
    >();

    const requestIntelligentFixes = useCallback(async () => {
        if (!runnerRef.current) {
            setLineIntelligentFixes(undefined);
            return;
        }

        setLineIntelligentFixes([]);

        let hasIntelligentFixes = false;
        for (const { top, right, width, height, driverDiagnostic } of lineDiagnostics) {
            const intelligentFix = await runnerRef.current.getIntelligentFix(driverDiagnostic);
            if (intelligentFix != null) {
                const lineIntelligentFix = {
                    top,
                    right,
                    width,
                    height,
                    intelligentFix,
                };

                setLineIntelligentFixes((intelligentFixes) =>
                    intelligentFixes
                        ? [...intelligentFixes, lineIntelligentFix]
                        : [lineIntelligentFix],
                );

                hasIntelligentFixes = true;
            }
        }

        if (!hasIntelligentFixes) {
            alert("No fixes available.");
            setLineIntelligentFixes(undefined);
        }
    }, [lineDiagnostics]);

    const getHelpForCode = useCallback(
        (position: number, code: string) => runnerRef.current!.help(position, code),
        [],
    );

    const format = useCallback(async () => {
        if (!runnerRef.current) {
            return;
        }

        const formatted = await runnerRef.current.format(
            codeMirrorRef.current!.editorView.state.sliceDoc(),
        );

        props.onChange(formatted);
    }, [props.onChange]);

    const [animationsSettled, setAnimationsSettled] = useState(false);

    useEffect(() => {
        setTimeout(() => {
            setAnimationsSettled(true);
        }, defaultAnimationDuration);
    }, []);

    const { displayAlert } = useAlert();

    const onClickLookUp = useCallback((help: Help) => {
        displayAlert(({ dismiss }) => <HelpAlert help={help} dismiss={dismiss} />);
    }, []);

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

    const renderBubble = (
        index: number,
        top: number,
        right: number,
        height: number,
        content: JSX.Element,
    ) => (
        <FloatingPortal key={index}>
            <div
                className="absolute w-fit pr-4"
                style={{
                    top: top + props.theme.fontSize / 4 - 1,
                    right,
                    height,
                }}
            >
                <Transition
                    in={!lookUpEnabled}
                    animateOnMount
                    inStyle={{ opacity: 1, x: 0 }}
                    outStyle={{ opacity: 0.5, x: "1rem" }}
                >
                    {content}
                </Transition>
            </div>
        </FloatingPortal>
    );

    return (
        <div
            autoFocus={props.autofocus}
            className="flex flex-col w-full"
            tabIndex={0}
            onFocus={() => setFocused(true)}
            onBlur={() => setFocused(false)}
        >
            <div className="flex flex-col border-2 border-gray-100 dark:border-gray-800 rounded-md overflow-clip">
                <div className="flex flex-row items-center justify-between w-full p-1 bg-gray-50 dark:bg-gray-900">
                    <div className="flex flex-row items-center">
                        {!lookUpEnabled ? (
                            <>
                                {!props.locked ? (
                                    <ContextMenuButton
                                        items={[
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
                                                title: "Delete",
                                                icon: "delete",
                                                role: "destructive",
                                                onClick: props.onDelete,
                                            },
                                        ]}
                                    >
                                        <MenuContainer>
                                            <button className="group transition-colors rounded-md px-1 h-7 hover:bg-gray-100 dark:hover:bg-gray-800">
                                                <MaterialSymbol
                                                    icon="more_vert"
                                                    className="text-lg"
                                                />
                                            </button>
                                        </MenuContainer>
                                    </ContextMenuButton>
                                ) : null}

                                <TutorialItem id="commandsButton">
                                    <PaletteButton
                                        setup={props.runtime?.name}
                                        assets={
                                            props.runtime
                                                ? runtimes[
                                                      props.runtime.name as keyof typeof runtimes
                                                  ].assetItems
                                                : []
                                        }
                                        items={
                                            props.runtime
                                                ? runtimes[
                                                      props.runtime.name as keyof typeof runtimes
                                                  ].paletteItems
                                                : defaultPaletteItems
                                        }
                                    />
                                </TutorialItem>
                            </>
                        ) : null}
                    </div>

                    <div className="flex-1 flex flex-row items-center justify-end gap-1">
                        {!lookUpEnabled ? (
                            <TutorialItem id="editButton">
                                <ActionsButton
                                    onSelectAll={() => runCommand(commands.selectAll)}
                                    onUndo={() => runCommand(commands.undo)}
                                    onRedo={() => runCommand(commands.redo)}
                                    onFormat={format}
                                    onSuggestFixes={
                                        lineDiagnostics.length > 0
                                            ? requestIntelligentFixes
                                            : undefined
                                    }
                                    onReset={props.onReset}
                                />
                            </TutorialItem>
                        ) : null}

                        <LookUpToggle enabled={lookUpEnabled} onChange={setLookUpEnabled} />
                    </div>
                </div>

                <AddLineButton
                    direction="start"
                    disabled={lookUpEnabled}
                    onClick={() => onAddLine("start")}
                />

                <div className="relative pb-[3px]">
                    <CodeMirror
                        ref={codeMirrorRef}
                        autoFocus
                        onChange={(value) => {
                            setDiagnostics([]);
                            props.onChange(value);
                        }}
                        onDrop={() => {
                            format();
                        }}
                        readOnly={lookUpEnabled}
                        lookUpEnabled={lookUpEnabled}
                        onClickLookUp={onClickLookUp}
                        help={getHelpForCode}
                        onClickAsset={onClickAsset}
                        theme={props.theme}
                        diagnostics={diagnostics}
                        highlightItems={highlightItems}
                    >
                        {props.children}
                    </CodeMirror>

                    {!animationsSettled
                        ? null
                        : lineIntelligentFixes != null
                        ? lineIntelligentFixes.map(
                              ({ top, right, width, height, intelligentFix }, index) =>
                                  renderBubble(
                                      index,
                                      top,
                                      right,
                                      height,
                                      <IntelligentFixBubble
                                          width={width}
                                          height={height}
                                          theme={props.theme}
                                          message={intelligentFix.message}
                                          onClick={() => {
                                              props.onChange(intelligentFix.fixedCode);
                                              setLineIntelligentFixes(undefined);
                                          }}
                                      />,
                                  ),
                          )
                        : lineDiagnostics.map(({ top, right, width, height, diagnostic }, index) =>
                              renderBubble(
                                  index,
                                  top,
                                  right,
                                  height,
                                  <DiagnosticBubble
                                      width={width}
                                      height={height}
                                      theme={props.theme}
                                      diagnostic={diagnostic}
                                      onApplyFix={applyFix}
                                  />,
                              ),
                          )}
                </div>

                <AddLineButton
                    direction="end"
                    disabled={lookUpEnabled}
                    onClick={() => onAddLine("end")}
                />

                {animationsSettled ? (
                    <Animated direction="vertical" clip>
                        <Runner
                            ref={runnerRef}
                            wipple={props.wipple}
                            options={runOptions}
                            runtime={
                                props.runtime != null && props.runtime.name in runtimes
                                    ? {
                                          Component:
                                              runtimes[props.runtime.name as keyof typeof runtimes]
                                                  .Component,
                                          settings: props.runtime.settings,
                                          onChangeSettings: props.runtime.onChangeSettings,
                                      }
                                    : undefined
                            }
                            hasFocus={runnerHasFocus}
                            onFocus={() => setRunnerHasFocus(true)}
                            onBlur={() => setRunnerHasFocus(false)}
                            onChangeDiagnostics={(diagnostics, driverDiagnostics) => {
                                setDiagnostics(diagnostics);
                                setDriverDiagnostics(driverDiagnostics);
                            }}
                            onChangeHighlightItems={setHighlightItems}
                        >
                            {props.children}
                        </Runner>
                    </Animated>
                ) : null}
            </div>
        </div>
    );
}

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
            <div className="mx-4 w-full h-1 bg-blue-500 rounded-full opacity-0 group-hover:opacity-100 transition-opacity" />
        </div>
    </Tooltip>
);

const LookUpToggle = (props: { enabled: boolean; onChange?: (enabled: boolean) => void }) => (
    <>
        {props.enabled ? (
            <p className="flex-1 px-1.5 text-sm text-gray-500 dark:text-gray-400">
                Select a piece of code for help.
            </p>
        ) : null}

        <TutorialItem id="lookUpButton">
            <MenuContainer>
                <button
                    className={`group flex flex-row items-center justify-center gap-1 transition-colors rounded-md ${
                        props.enabled
                            ? "mx-1 px-2 py-1 bg-blue-500 text-white text-sm"
                            : "px-2 h-7 hover:bg-gray-100 dark:hover:bg-gray-800"
                    }`}
                    onClick={() => props.onChange?.(!props.enabled)}
                >
                    {props.enabled ? (
                        <p className="whitespace-nowrap">Done</p>
                    ) : (
                        <>
                            <img src={lookupIcon} className="w-4 h-4" />
                            <p className="text-xs text-nowrap">Look Up</p>
                        </>
                    )}
                </button>
            </MenuContainer>
        </TutorialItem>
    </>
);

const ActionsButton = (props: {
    onSelectAll: () => void;
    onUndo: () => void;
    onRedo: () => void;
    onFormat: () => void;
    onSuggestFixes?: () => void;
    onReset?: () => void;
}) => (
    <ContextMenuButton
        items={[
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
                title: "Format",
                icon: "format_align_left",
                onClick: props.onFormat,
            },
            {
                title: "Suggest Fixes",
                icon: "build_circle",
                onClick: props.onSuggestFixes,
                disabled: props.onSuggestFixes == null,
            },
            {
                title: "Reset",
                icon: "restart_alt",
                role: "destructive",
                onClick: props.onReset,
                disabled: props.onReset == null,
            },
        ]}
    >
        <MenuContainer>
            <button className="group flex flex-row items-center justify-center transition-colors rounded-md px-2 gap-1 h-7 hover:bg-gray-100 dark:hover:bg-gray-800">
                <img
                    src={actionsIcon}
                    className="w-[20px] h-[20px] translate-x-[1px] translate-y-[-1px]"
                />

                <p className="text-xs">Actions</p>
            </button>
        </MenuContainer>
    </ContextMenuButton>
);

const PaletteButton = (props: { setup?: string; assets: PaletteItem[]; items: PaletteItem[] }) => {
    const items = props.items.map(
        (item, index): ContextMenuItem => ({
            title: ({ onDismiss }) => (
                <div
                    key={index}
                    draggable
                    className="flex flex-col items-start w-full pointer-events-[bounding-box]"
                    onDragStart={(event) => {
                        event.dataTransfer.setData(
                            "wipple/snippet",
                            JSON.stringify({
                                code: item.code,
                                insertLine: true,
                            }),
                        );

                        onDismiss();
                    }}
                >
                    <code className="whitespace-nowrap">{item.title}</code>
                </div>
            ),
        }),
    );

    return (
        <ContextMenuButton
            items={
                props.assets.length > 0
                    ? [
                          {
                              title: ({ onDismiss }) => (
                                  <div className="flex flex-row items-center gap-1 py-1">
                                      {props.assets.map((asset, index) => (
                                          <div
                                              key={index}
                                              draggable
                                              onDragStart={(event) => {
                                                  event.dataTransfer.setData(
                                                      "wipple/snippet",
                                                      JSON.stringify({
                                                          code: asset.code,
                                                          insertLine: false,
                                                      }),
                                                  );

                                                  onDismiss();
                                              }}
                                          >
                                              {asset.title}
                                          </div>
                                      ))}
                                  </div>
                              ),
                              divider: true,
                              highlight: false,
                          },
                          ...items,
                      ]
                    : items
            }
        >
            <MenuContainer>
                <button className="group flex flex-row items-center justify-center transition-colors rounded-md pl-2 pr-1 gap-1 h-7 hover:bg-gray-100 dark:hover:bg-gray-800">
                    {props.setup ? <SetupIcon setup={props.setup} size="sm" /> : null}
                    <p className="text-xs ml-1">Add</p>
                    <MaterialSymbol icon="expand_more" className="text-lg" />
                </button>
            </MenuContainer>
        </ContextMenuButton>
    );
};

const DiagnosticBubble = (props: {
    width: number;
    height: number;
    theme: ThemeConfig;
    diagnostic: any;
    onApplyFix: (fix: any, start: number, end: number) => void;
}) => {
    const [isExpanded, setExpanded] = useState(false);

    const [doc, setDoc] = useState<ErrorDoc>();

    useEffect(() => {
        if (isExpanded) {
            setDoc(docForError(props.diagnostic.message));
        }
    }, [isExpanded]);

    const { displayAlert } = useAlert();

    return (
        <div style={{ maxWidth: isExpanded ? undefined : props.width }}>
            <div className="flex flex-row items-start justify-end" style={{ height: props.height }}>
                <button
                    className={`flex flex-row items-center gap-1.5 px-2 rounded-lg overflow-x-scroll no-scrollbar whitespace-nowrap transition-colors ${
                        props.diagnostic.severity === "error"
                            ? " text-red-600 dark:text-red-500"
                            : " text-yellow-600 dark:text-yellow-400"
                    } ${
                        isExpanded
                            ? "z-50 max-w-none w-max bg-white dark:bg-gray-800 border-2 border-gray-100 dark:border-gray-800 shadow-lg"
                            : `h-full hover:text-white ${
                                  props.diagnostic.severity === "error"
                                      ? "bg-red-50 dark:bg-red-950 hover:bg-red-500"
                                      : "bg-yellow-50 dark:bg-yellow-950 hover:bg-yellow-500"
                              }`
                    }`}
                    onClick={isExpanded ? undefined : () => setExpanded(true)}
                >
                    <Animated
                        key={isExpanded ? "active" : "inactive"}
                        direction={["horizontal", "vertical"]}
                        unsized
                    >
                        <div className="flex flex-col items-start gap-1 py-1">
                            <div className="flex flex-row items-center justify-stretch gap-2 w-full">
                                <div className="flex flex-row items-center gap-1.5 flex-1">
                                    {isExpanded ? null : (
                                        <MaterialSymbol
                                            icon={
                                                props.diagnostic.severity === "error"
                                                    ? "error"
                                                    : "info"
                                            }
                                            className="text-lg"
                                        />
                                    )}

                                    <Markdown>{props.diagnostic.message}</Markdown>
                                </div>

                                {isExpanded ? (
                                    <button
                                        className="flex items-center justify-center w-5 h-5 -m-1 -mt-2 text-black dark:text-gray-50 rounded-full hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors"
                                        onClick={() => setExpanded(false)}
                                    >
                                        <MaterialSymbol icon="close" className="text-lg" />
                                    </button>
                                ) : null}
                            </div>

                            {isExpanded ? (
                                <div className="flex flex-col items-start gap-1">
                                    {props.diagnostic.explanations.map((explanation: any) => (
                                        <Markdown className="text-gray-600 dark:text-gray-400">
                                            {explanation.item.message}
                                        </Markdown>
                                    ))}
                                </div>
                            ) : null}

                            {isExpanded ? (
                                <div className="flex flex-row w-full gap-2.5">
                                    {props.diagnostic.fix ? (
                                        <div className="flex flex-row items-center justify-stretch gap-2 pb-1 text-black dark:text-gray-50">
                                            <div className="flex-1">
                                                <Markdown>{props.diagnostic.fix.message}</Markdown>
                                            </div>

                                            <button
                                                className="h-[1lh] px-2 rounded-lg text-white bg-blue-500 hover:bg-blue-600 transition-colors"
                                                onClick={() => {
                                                    setExpanded(false);
                                                    props.onApplyFix(
                                                        props.diagnostic.fix!,
                                                        props.diagnostic.location.start.index,
                                                        props.diagnostic.location.end.index,
                                                    );
                                                }}
                                            >
                                                Fix
                                            </button>
                                        </div>
                                    ) : (
                                        <p className="text-gray-400 dark:text-gray-600">
                                            No fixes available
                                        </p>
                                    )}

                                    <button
                                        className="h-[1lh] px-2 rounded-lg text-blue-500 bg-blue-500 hover:bg-blue-500 hover:text-white bg-opacity-25 transition-colors"
                                        onClick={() => {
                                            setExpanded(false);

                                            displayAlert(({ dismiss }) => (
                                                <RawDiagnosticAlert
                                                    raw={props.diagnostic.raw}
                                                    dismiss={dismiss}
                                                />
                                            ));
                                        }}
                                    >
                                        More
                                    </button>

                                    {doc ? (
                                        <button
                                            className="h-[1lh] px-2 rounded-lg text-blue-500 bg-blue-500 hover:bg-blue-500 hover:text-white bg-opacity-25 transition-colors"
                                            onClick={() => {
                                                setExpanded(false);

                                                displayAlert(({ dismiss }) => (
                                                    <ErrorDocAlert doc={doc} dismiss={dismiss} />
                                                ));
                                            }}
                                        >
                                            Help
                                        </button>
                                    ) : null}
                                </div>
                            ) : null}
                        </div>
                    </Animated>
                </button>
            </div>
        </div>
    );
};

const IntelligentFixBubble = (props: {
    width: number;
    height: number;
    theme: ThemeConfig;
    message: string;
    onClick: () => void;
}) => (
    <div style={{ maxWidth: props.width }}>
        <div className="flex flex-row items-start justify-end" style={{ height: props.height }}>
            <button
                className={`flex flex-row items-center gap-1.5 px-2 rounded-lg overflow-x-scroll no-scrollbar whitespace-nowrap transition-colors text-sky-600 dark:text-sky-500 h-full hover:text-white bg-sky-50 dark:bg-sky-950 hover:bg-sky-500`}
                onClick={props.onClick}
            >
                <div className="flex flex-row items-center gap-1.5 py-1">
                    <MaterialSymbol icon="build_circle" />
                    <Markdown>{props.message}</Markdown>
                </div>
            </button>
        </div>
    </div>
);

const MenuContainer = (props: React.PropsWithChildren<{}>) => (
    <div className="flex flex-row items-center text-gray-800 dark:text-gray-400 text-opacity-50 h-7">
        {props.children}
    </div>
);

const RawDiagnosticAlert = (props: { raw: string; dismiss: () => void }) => {
    return (
        <div className="flex flex-col w-[650px] gap-4">
            <div className="max-h-[75vh] overflow-y-scroll">
                <Ansi>{props.raw}</Ansi>
            </div>

            <Button role="primary" fill onClick={props.dismiss}>
                Done
            </Button>
        </div>
    );
};

const ErrorDocAlert = (props: { doc: ErrorDoc; dismiss: () => void }) => (
    <div className="flex flex-col w-[650px] gap-4">
        <div className="max-h-[75vh] overflow-y-scroll">
            <Markdown className="text-2xl font-semibold mb-4">{`What does "${props.doc.error}" mean?`}</Markdown>

            <div className="help">
                <Markdown className="prose">{props.doc.doc}</Markdown>
            </div>
        </div>

        <Button role="primary" fill onClick={props.dismiss}>
            Done
        </Button>
    </div>
);
