import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import {
    Animated,
    Markdown,
    Menu,
    MenuContainer,
    Tooltip,
    Transition,
    defaultAnimationDuration,
    useAlert,
} from "../../components";
import { CodeMirror, CodeMirrorRef } from "./codemirror";
import { RunOptions, Runner, RunnerRef } from "./runner";
import { MaterialSymbol } from "react-material-symbols";
import { ThemeConfig } from "./codemirror/theme";
import { Help } from "../../models";
import { useHotkeys } from "react-hotkeys-hook";
import { useWindowSize } from "usehooks-ts";
import { HelpAlert } from "./help-alert";
import { AssetPalette, SnippetPalette } from "./palette";
import { flushSync } from "react-dom";
import { Turtle } from "../../runtimes";
import { ColorPicker } from "./color-picker";
import { AssetClickHandler } from "./codemirror/assets";
import { colorAsset } from "./assets";
import { RenderedDiagnostic, RenderedFix } from "wipple-render";

export const CodeEditor = (props: {
    children: string;
    onChange: (value: string) => void;
    theme: ThemeConfig;
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

    const [isHovering, setHovering] = useState(props.autofocus ?? false);

    const [runOptions, setRunOptions] = useState<RunOptions>({
        dependenciesPath: "turtle",
    });

    const [runnerHasFocus, setRunnerHasFocus] = useState(false);

    const [diagnostics, setDiagnostics] = useState<RenderedDiagnostic[]>([]);

    const [isListeningForQuickHelp, setListeningForQuickHelp] = useState(true);
    const [quickHelpEnabled, setQuickHelpEnabled] = useState(false);
    const [quickHelpLocked, setQuickHelpLocked] = useState(false);

    useHotkeys(
        "alt",
        (e) => {
            if (!quickHelpLocked) {
                if (isListeningForQuickHelp) {
                    setQuickHelpEnabled(e.type === "keydown");
                } else {
                    setListeningForQuickHelp(true);
                }
            }
        },
        {
            enabled: isFocused || quickHelpEnabled,
            enableOnContentEditable: true,
            keydown: true,
            keyup: true,
        },
        [isFocused, quickHelpEnabled, quickHelpLocked, isListeningForQuickHelp],
    );

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

        // TODO
        // const replacement =
        //     (fix.before ?? "") +
        //     (fix.replacement ?? editorView.state.sliceDoc(start, end)) +
        //     (fix.after ?? "");

        // editorView.dispatch({
        //     changes: { from: start, to: end, insert: replacement },
        // });
    };

    const getHelpForCode = useCallback(
        (position: number, code: string) => runnerRef.current!.help(position, code),
        [],
    );

    const [animationsSettled, setAnimationsSettled] = useState(false);

    useEffect(() => {
        setTimeout(() => {
            setAnimationsSettled(true);
        }, defaultAnimationDuration);
    }, []);

    const { displayAlert } = useAlert();

    const onClickQuickHelp = useCallback((help: Help) => {
        setQuickHelpEnabled(true);
        setListeningForQuickHelp(false);

        displayAlert(({ dismiss }) => (
            <HelpAlert
                help={help}
                dismiss={() => {
                    setQuickHelpEnabled(false);
                    setQuickHelpLocked(false);
                    dismiss();
                }}
            />
        ));
    }, []);

    const onClickAsset: AssetClickHandler = useCallback(({ start, end, type, value }) => {
        switch (type) {
            case "color":
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

    return (
        <div
            autoFocus={props.autofocus}
            className="flex flex-col w-full"
            tabIndex={0}
            onFocus={() => setFocused(true)}
            onBlur={() => setFocused(false)}
            onMouseEnter={() => setHovering(true)}
            onMouseLeave={() => setHovering(false)}
        >
            <div className="mx-[14px] h-[14px] z-10">
                <Transition
                    in={quickHelpEnabled || isFocused || isHovering}
                    exitAnimationDuration={defaultAnimationDuration}
                    inClassName="animate-in fade-in slide-in-from-bottom-[1px] ease-linear"
                    outClassName="animate-out fade-out slide-out-to-bottom-[1px] ease-linear"
                >
                    <div className="flex flex-row items-center justify-between">
                        <div className="flex flex-row gap-2">
                            <QuickHelpToggle
                                enabled={quickHelpEnabled}
                                locked={quickHelpLocked}
                                onChange={setQuickHelpLocked}
                            />

                            <Transition
                                in={!quickHelpLocked}
                                exitAnimationDuration={defaultAnimationDuration}
                                inClassName="animate-in fade-in slide-in-from-left-4"
                                outClassName="animate-out fade-out slide-out-to-left-4"
                            >
                                <Menu
                                    items={[
                                        {
                                            name: "Code",
                                            onClick: () => setFocused(true),
                                            children: [
                                                {
                                                    name: "TODO 1",
                                                    onClick: () => {
                                                        setFocused(true);
                                                    },
                                                },
                                                {
                                                    name: "TODO 2",
                                                    onClick: () => {
                                                        setFocused(true);
                                                    },
                                                },
                                            ],
                                        },
                                        {
                                            name: "Edit",
                                            onClick: () => setFocused(true),
                                            children: [
                                                {
                                                    name: "TODO 1",
                                                    onClick: () => {
                                                        setFocused(true);
                                                    },
                                                },
                                                {
                                                    name: "TODO 2",
                                                    onClick: () => {
                                                        setFocused(true);
                                                    },
                                                },
                                            ],
                                        },
                                        {
                                            name: "Select",
                                            onClick: () => setFocused(true),
                                            children: [
                                                {
                                                    name: "TODO 1",
                                                    onClick: () => {
                                                        setFocused(true);
                                                    },
                                                },
                                                {
                                                    name: "TODO 2",
                                                    onClick: () => {
                                                        setFocused(true);
                                                    },
                                                },
                                            ],
                                        },
                                        {
                                            name: "View",
                                            onClick: () => setFocused(true),
                                            children: [
                                                {
                                                    name: "TODO 1",
                                                    onClick: () => {
                                                        setFocused(true);
                                                    },
                                                },
                                                {
                                                    name: "TODO 2",
                                                    onClick: () => {
                                                        setFocused(true);
                                                    },
                                                },
                                            ],
                                        },
                                        {
                                            name: "Help",
                                            onClick: () => setFocused(true),
                                            children: [
                                                {
                                                    name: "TODO 1",
                                                    onClick: () => {
                                                        setFocused(true);
                                                    },
                                                },
                                                {
                                                    name: "TODO 2",
                                                    onClick: () => {
                                                        setFocused(true);
                                                    },
                                                },
                                            ],
                                        },
                                    ]}
                                />
                            </Transition>
                        </div>

                        <div className="flex flex-row gap-2">
                            <Transition
                                in={!(quickHelpLocked ?? false)}
                                exitAnimationDuration={defaultAnimationDuration}
                                inClassName="animate-in fade-in"
                                outClassName="animate-out fade-out"
                            >
                                <PaletteMenu />
                            </Transition>
                        </div>
                    </div>
                </Transition>
            </div>

            <div className="flex flex-col border-2 border-gray-100 dark:border-gray-800 rounded-md">
                <Animated direction="vertical">
                    {quickHelpLocked ?? false ? (
                        <p className="pt-5 px-4 text-sm text-gray-500 dark:text-gray-400">
                            Hover over a piece of code for help.
                        </p>
                    ) : null}
                </Animated>

                <div className="relative py-[3px]">
                    <CodeMirror
                        ref={codeMirrorRef}
                        autoFocus
                        onChange={(value) => {
                            flushSync(() => {
                                setDiagnostics([]);
                            });

                            props.onChange(value);
                        }}
                        readOnly={quickHelpLocked}
                        quickHelpEnabled={quickHelpEnabled || quickHelpLocked}
                        onClickQuickHelp={onClickQuickHelp}
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
                              <Transition
                                  key={index}
                                  in={!quickHelpEnabled && !quickHelpLocked}
                                  animateOnMount
                                  exitAnimationDuration={defaultAnimationDuration}
                                  inClassName="animate-in slide-in-from-right-4 fade-in-50"
                                  outClassName="animate-out slide-out-to-right-4 fade-out-50"
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
                          ))}
                </div>

                <Animated direction="vertical" clip>
                    <Runner
                        ref={runnerRef}
                        options={runOptions}
                        runtime={Turtle}
                        hasFocus={runnerHasFocus}
                        onFocus={() => setRunnerHasFocus(true)}
                        onBlur={() => setRunnerHasFocus(false)}
                        onChangeDiagnostics={setDiagnostics}
                    >
                        {props.children}
                    </Runner>
                </Animated>
            </div>
        </div>
    );
};

const PaletteMenu = () => {
    const [Selection, setSelection] = useState<React.FC<{ onClose: () => void }>>();

    return (
        <Tooltip description="Palette" disabled={Selection != null}>
            <MenuContainer>
                <Animated direction="horizontal">
                    {Selection ? (
                        <div className="px-1">
                            <Selection onClose={() => setSelection(undefined)} />
                        </div>
                    ) : (
                        <div className="flex flex-row items-center">
                            <button
                                className="group h-[28px] mt-[2px] transition-colors"
                                onClick={() => setSelection(() => AssetPalette)}
                            >
                                <div className="flex flex-row px-1 ml-3 mb-0.5">
                                    <ColorBlock className="bg-red-500 z-[3] group-hover:-rotate-[5deg] group-hover:-translate-x-0.5" />
                                    <ColorBlock className="bg-green-500 z-[2] group-hover:rotate-[5deg]" />
                                    <ColorBlock className="bg-blue-500 z-[1] group-hover:-rotate-[10deg] group-hover:translate-x-0.5" />
                                </div>
                            </button>

                            <div className="rounded-full border-l-2 border-gray-100 dark:border-gray-800 h-[18px]" />

                            <button
                                className="group hover:scale-110 h-[28px] mt-[2px] transition-all"
                                onClick={() => setSelection(() => SnippetPalette)}
                            >
                                <div className="flex items-center px-1 mb-0.5">
                                    <code className="text-xs">abc</code>
                                </div>
                            </button>
                        </div>
                    )}
                </Animated>
            </MenuContainer>
        </Tooltip>
    );
};

const QuickHelpToggle = (props: {
    enabled: boolean;
    locked: boolean;
    onChange?: (quickHelpEnabled: boolean) => void;
}) => {
    return (
        <Tooltip
            description={
                props.locked
                    ? undefined
                    : `Quick Help ${
                          /mac/.test(navigator.userAgent.toLowerCase()) ? "(⌥)" : "(Alt)"
                      }`
            }
        >
            <Animated direction="horizontal">
                <MenuContainer>
                    <button
                        className={`group flex flex-row items-center justify-center gap-1 h-[28px] transition-colors ${
                            props.locked
                                ? "px-2 bg-blue-500 text-white"
                                : `w-[24px] ${
                                      props.enabled
                                          ? "bg-gray-100 dark:bg-gray-800"
                                          : "hover:bg-gray-100 dark:hover:bg-gray-800"
                                  }`
                        }`}
                        disabled={props.enabled && !props.locked}
                        onClick={() => props.onChange?.(!props.locked)}
                    >
                        {props.locked ? (
                            <p className="whitespace-nowrap">Done</p>
                        ) : (
                            <MaterialSymbol icon="frame_inspect" className="text-lg" />
                        )}
                    </button>
                </MenuContainer>
            </Animated>
        </Tooltip>
    );
};

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
        <div
            className="absolute right-4"
            style={{
                top: props.top + props.theme.fontSize / 4 - 1,
                maxWidth: isExpanded ? undefined : props.width,
                height: props.height,
                paddingLeft: "3rem",
            }}
        >
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

                            {/* {isExpanded ? (
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
                                                        props.diagnostic.primaryLabel.span.start,
                                                        props.diagnostic.primaryLabel.span.end,
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
                            ) : null} */}
                        </div>
                    </Animated>
                </button>
            </div>
        </div>
    );
};
