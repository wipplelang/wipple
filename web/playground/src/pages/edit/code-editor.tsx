import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import {
    Animated,
    ContextMenuButton,
    Markdown,
    Tooltip,
    Transition,
    defaultAnimationDuration,
    useAlert,
} from "../../components";
import { CodeMirror, CodeMirrorRef } from "./codemirror";
import * as commands from "@codemirror/commands";
import { RunOptions, Runner, RunnerRef } from "./runner";
import { MaterialSymbol } from "react-material-symbols";
import { ThemeConfig } from "./codemirror/theme";
import { Help } from "../../models";
import { useWindowSize } from "usehooks-ts";
import { HelpAlert } from "./help-alert";
import { Palette } from "./palette";
import { ColorPicker } from "./color-picker";
import { AssetClickHandler } from "./codemirror/assets";
import { colorAsset } from "./assets";
import { RenderedDiagnostic, RenderedFix } from "wipple-render";
import { runtimes } from "../../runtimes";
import { SetupIcon } from "./setup-icon";
import { StateCommand } from "@codemirror/state";

export const CodeEditor = (props: {
    children: string;
    onChange: (value: string) => void;
    theme: ThemeConfig;
    runtime?: string;
    autofocus?: boolean;
    onFocus?: () => void;
    onBlur?: () => void;
}) => {
    const [isFocused, setFocused] = useState(props.autofocus ?? false);

    useEffect(() => {
        if (isFocused) {
            props.onFocus?.();
        } else {
            props.onBlur?.();
        }
    }, [isFocused]);

    const [runOptions, setRunOptions] = useState<RunOptions>({
        dependenciesPath: props.runtime ?? "base",
    });

    const [runnerHasFocus, setRunnerHasFocus] = useState(false);

    const [diagnostics, setDiagnostics] = useState<RenderedDiagnostic[]>([]);

    const [lookUpEnabled, setLookUpEnabled] = useState(false);

    const codeMirrorRef = useRef<CodeMirrorRef>(null);
    const runnerRef = useRef<RunnerRef>(null);

    const windowSize = useWindowSize();

    const lineDiagnostics = useMemo(() => {
        const editorView = codeMirrorRef.current?.editorView;
        if (!editorView) {
            return [];
        }

        const editorRect = editorView.dom.getBoundingClientRect();

        const coveredLines: number[] = [];

        return diagnostics.flatMap((diagnostic) => {
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
                    top: top - editorRect.top,
                    width: editorRect.width - width,
                    height,
                    diagnostic,
                },
            ];
        });
    }, [diagnostics, windowSize]);

    const applyFix = (fix: RenderedFix, start: number, end: number) => {
        const editorView = codeMirrorRef.current?.editorView;
        if (!editorView) {
            return;
        }

        let replacement = editorView.state.sliceDoc(start, end);
        if (fix.replacement) {
            replacement = fix.replacement.replace("_", replacement);
        }

        replacement = (fix.before ?? "") + replacement + (fix.after ?? "");

        editorView.dispatch({
            changes: { from: start, to: end, insert: replacement },
        });
    };

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
        displayAlert(({ dismiss }) => (
            <HelpAlert
                help={help}
                dismiss={() => {
                    setLookUpEnabled(false);
                    dismiss();
                }}
            />
        ));
    }, []);

    const onClickAsset: AssetClickHandler = useCallback(({ start, end, type, value }) => {
        switch (type) {
            case "Color":
                displayAlert(({ dismiss }) => (
                    <ColorPicker
                        selection={value}
                        onDismiss={(color) => {
                            codeMirrorRef.current?.editorView.dispatch({
                                changes: { from: start, to: end, insert: colorAsset(color) },
                            });

                            dismiss();
                        }}
                    />
                ));

                break;
            default:
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
                    <div className="flex flex-1 flex-row items-center">
                        {props.runtime != null && !lookUpEnabled ? (
                            <>
                                <SetupButton setup={props.runtime} onClick={() => alert("TODO")} />
                            </>
                        ) : null}

                        <LookUpToggle enabled={lookUpEnabled} onChange={setLookUpEnabled} />

                        {lookUpEnabled ? (
                            <p className="px-1.5 text-sm text-gray-500 dark:text-gray-400">
                                Select a piece of code for help.
                            </p>
                        ) : (
                            <>
                                <FormatButton onClick={format} />

                                <EditButton
                                    onSelectAll={() => runCommand(commands.selectAll)}
                                    onUndo={() => runCommand(commands.undo)}
                                    onRedo={() => runCommand(commands.redo)}
                                />
                            </>
                        )}
                    </div>

                    <div className="flex flex-shrink flex-row gap-2">
                        <Transition
                            in={!(lookUpEnabled ?? false)}
                            inStyle={{ opacity: 1 }}
                            outStyle={{ opacity: 0 }}
                        >
                            <Palette
                                items={
                                    props.runtime
                                        ? runtimes[props.runtime as keyof typeof runtimes]
                                              .paletteItems
                                        : []
                                }
                            />
                        </Transition>
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
                    >
                        {props.children}
                    </CodeMirror>

                    {!animationsSettled
                        ? null
                        : lineDiagnostics.map(({ top, width, height, diagnostic }, index) => (
                              <div
                                  key={index}
                                  className="absolute right-4 w-fit"
                                  style={{
                                      top: top + props.theme.fontSize / 4 - 1,
                                      height,
                                      paddingLeft: "3rem",
                                  }}
                              >
                                  <Transition
                                      in={!lookUpEnabled}
                                      animateOnMount
                                      inStyle={{ opacity: 1, x: 0 }}
                                      outStyle={{ opacity: 0.5, x: "1rem" }}
                                  >
                                      <DiagnosticBubble
                                          top={top}
                                          width={width}
                                          height={height}
                                          theme={props.theme}
                                          diagnostic={diagnostic}
                                          onApplyFix={applyFix}
                                      />
                                  </Transition>
                              </div>
                          ))}
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
                            options={runOptions}
                            runtime={
                                props.runtime != null && props.runtime in runtimes
                                    ? runtimes[props.runtime as keyof typeof runtimes].Component
                                    : undefined
                            }
                            hasFocus={runnerHasFocus}
                            onFocus={() => setRunnerHasFocus(true)}
                            onBlur={() => setRunnerHasFocus(false)}
                            onChangeDiagnostics={setDiagnostics}
                        >
                            {props.children}
                        </Runner>
                    </Animated>
                ) : null}
            </div>
        </div>
    );
};

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

const SetupButton = (props: { setup: string; onClick: () => void }) => (
    <MenuContainer>
        <button className="group flex flex-row items-center justify-center gap-1 transition-colors rounded-md px-2 h-7 hover:bg-gray-100 dark:hover:bg-gray-800">
            <SetupIcon setup={props.setup} size="sm" />
            <p className="text-xs capitalize">{props.setup}</p>
        </button>
    </MenuContainer>
);

const LookUpToggle = (props: { enabled: boolean; onChange?: (enabled: boolean) => void }) => (
    <MenuContainer>
        <button
            className={`group flex flex-row items-center justify-center gap-1 transition-colors rounded-md ${
                props.enabled
                    ? "px-2 py-1 bg-blue-500 text-white text-sm"
                    : "px-2 h-7 hover:bg-gray-100 dark:hover:bg-gray-800"
            }`}
            onClick={() => props.onChange?.(!props.enabled)}
        >
            {props.enabled ? (
                <p className="whitespace-nowrap">Done</p>
            ) : (
                <>
                    <MaterialSymbol icon="search" className="text-lg" />
                    <p className="text-xs text-nowrap">Look Up</p>
                </>
            )}
        </button>
    </MenuContainer>
);

const FormatButton = (props: { onClick: () => void }) => (
    <MenuContainer>
        <button
            className="group flex flex-row items-center justify-center transition-colors rounded-md gap-1 px-2 h-7 hover:bg-gray-100 dark:hover:bg-gray-800"
            onClick={props.onClick}
        >
            <MaterialSymbol icon="notes" className="text-lg" />

            <p className="text-xs">Format</p>
        </button>
    </MenuContainer>
);

const EditButton = (props: { onSelectAll: () => void; onUndo: () => void; onRedo: () => void }) => (
    <ContextMenuButton
        items={[
            {
                title: "Select All",
                shortcut: {
                    mac: "⌘ A",
                    win: "Ctrl A",
                },
                icon: "select_all",
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
        ]}
    >
        <MenuContainer>
            <button className="group flex flex-row items-center justify-center transition-colors rounded-md px-2 gap-1 h-7 hover:bg-gray-100 dark:hover:bg-gray-800">
                <MaterialSymbol icon="text_fields_alt" className="text-lg" />
                <p className="text-xs">Edit</p>
            </button>
        </MenuContainer>
    </ContextMenuButton>
);

const ColorBlock = (props: { className: string }) => (
    <div
        className={`-ml-3 w-5 h-5 rounded-md border-2 border-gray-100 dark:border-gray-800 transition-transform ${props.className}`}
    />
);

const DiagnosticBubble = (props: {
    top: number;
    width: number;
    height: number;
    theme: ThemeConfig;
    diagnostic: RenderedDiagnostic;
    onApplyFix: (fix: RenderedFix, start: number, end: number) => void;
}) => {
    const [isExpanded, setExpanded] = useState(false);

    return (
        <div style={{ maxWidth: isExpanded ? undefined : props.width }}>
            <div className="w-10 pointer-events-none" />

            <div
                className="flex flex-row items-center justify-end"
                style={{ height: props.height }}
            >
                <button
                    className={`flex flex-row items-center gap-1.5 px-2 rounded-lg overflow-x-scroll whitespace-nowrap transition-colors ${
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
                    <Animated direction={["horizontal", "vertical"]} unsized>
                        <div className="flex flex-col gap-1 py-1">
                            <div className="flex flex-row items-center justify-stretch gap-2">
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
                                <div className="flex flex-row w-full">
                                    {props.diagnostic.fix ? (
                                        <div className="flex flex-row items-center justify-stretch gap-2 pb-1 text-black dark:text-gray-50">
                                            <div className="flex-1">
                                                <Markdown>{props.diagnostic.fix.message}</Markdown>
                                            </div>

                                            <button
                                                className="px-2 rounded-lg text-white bg-blue-500 hover:bg-blue-600 transition-colors"
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
                                </div>
                            ) : null}
                        </div>
                    </Animated>
                </button>
            </div>
        </div>
    );
};

const MenuContainer = (props: React.PropsWithChildren<{}>) => (
    <div className="flex flex-row items-center text-gray-800 dark:text-gray-400 text-opacity-50 h-7">
        {props.children}
    </div>
);
