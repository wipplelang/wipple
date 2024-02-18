import { useEffect, useState } from "react";
import {
    Animated,
    Menu,
    MenuContainer,
    Tooltip,
    Transition,
    defaultAnimationDuration,
} from "../../components";
import { CodeMirror } from "./codemirror";
import { RunOptions, Runner } from "./runner";
import { MaterialSymbol } from "react-material-symbols";
import { defaultThemeConfig } from "./codemirror/theme";
import { Diagnostic } from "../../models";
import { useHotkeys } from "react-hotkeys-hook";

export const CodeEditor = (props: {
    children: string;
    onChange?: (value: string) => void;
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

    const [isHovering, setHovering] = useState(false);

    const [runOptions, setRunOptions] = useState<RunOptions>({
        dependenciesPath: "turtle",
    });

    const [runnerHasFocus, setRunnerHasFocus] = useState(false);

    const [diagnostics, setDiagnostics] = useState<Diagnostic[]>([]);

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
                    value={quickHelpEnabled || isFocused || isHovering ? {} : undefined}
                    exitAnimationDuration={defaultAnimationDuration}
                    inClassName="animate-in fade-in slide-in-from-bottom-[1px] ease-linear"
                    outClassName="animate-out fade-out slide-out-to-bottom-[1px] ease-linear"
                >
                    {() => (
                        <div className="flex flex-row items-center justify-between">
                            <div className="flex flex-row gap-2">
                                <QuickHelpToggle
                                    enabled={quickHelpEnabled}
                                    locked={quickHelpLocked}
                                    onChange={setQuickHelpLocked}
                                />

                                <Transition
                                    value={quickHelpLocked ? undefined : {}}
                                    exitAnimationDuration={defaultAnimationDuration}
                                    inClassName="animate-in fade-in slide-in-from-left-4"
                                    outClassName="animate-out fade-out slide-out-to-left-4"
                                >
                                    {() => (
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
                                    )}
                                </Transition>
                            </div>

                            <div className="flex flex-row gap-2">
                                <Transition
                                    value={quickHelpLocked ?? false ? undefined : {}}
                                    exitAnimationDuration={defaultAnimationDuration}
                                    inClassName="animate-in fade-in"
                                    outClassName="animate-out fade-out"
                                >
                                    {() => <InsertMenu />}
                                </Transition>
                            </div>
                        </div>
                    )}
                </Transition>
            </div>

            <div className="flex flex-col border-2 border-gray-100 dark:border-gray-800 rounded-md overflow-clip">
                <Animated direction="vertical">
                    {quickHelpLocked ?? false ? (
                        <p className="pt-5 px-4 text-sm text-gray-500 dark:text-gray-400">
                            Hover over a piece of code for help.
                        </p>
                    ) : null}
                </Animated>

                <div className="py-[3px]">
                    <CodeMirror
                        onChange={(code) => {
                            props.onChange?.(code);
                            setDiagnostics([]);
                        }}
                        readOnly={quickHelpLocked}
                        quickHelpEnabled={quickHelpEnabled || quickHelpLocked}
                        onClickQuickHelp={(selected) => {
                            setQuickHelpEnabled(selected);
                            setListeningForQuickHelp(!selected);
                        }}
                        theme={defaultThemeConfig()}
                        diagnostics={diagnostics}
                    >
                        {props.children}
                    </CodeMirror>
                </div>

                <Animated direction="vertical" clip>
                    <Runner
                        options={runOptions}
                        runtime={undefined}
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

const InsertMenu = () => {
    const [isExpanded, setExpanded] = useState(false);

    return (
        <Tooltip description="Insert">
            <MenuContainer>
                <Animated direction="horizontal">
                    {isExpanded ? (
                        <button className="px-2" onClick={() => setExpanded(false)}>
                            <p className="whitespace-nowrap">TODO: Palette</p>
                        </button>
                    ) : (
                        <button
                            className="group hover:bg-gray-100 dark:hover:bg-gray-800 h-[28px] mt-[2px] transition-colors"
                            onClick={() => setExpanded(true)}
                        >
                            <div className="flex flex-row px-1 ml-3">
                                <ColorBlock className="bg-red-500 z-[3] group-hover:-rotate-[5deg] group-hover:-translate-x-0.5" />
                                <ColorBlock className="bg-green-500 z-[2] group-hover:rotate-[5deg]" />
                                <ColorBlock className="bg-blue-500 z-[1] group-hover:-rotate-[10deg] group-hover:translate-x-0.5" />
                            </div>
                        </button>
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
