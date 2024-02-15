import { useEffect, useMemo, useState } from "react";
import {
    Animated,
    Menu,
    MenuContainer,
    Transition,
    defaultAnimationDuration,
} from "../../components";
import { CodeMirror } from "./codemirror";
import { Runner } from "./runner";
import { Turtle } from "../../runtimes";

export const CodeEditor = (props: {
    children: string;
    onChange?: (value: string) => void;
    autofocus?: boolean;
    onFocus?: () => void;
    onBlur?: () => void;
}) => {
    const [isFocused, setFocused] = useState(props.autofocus);

    useEffect(() => {
        if (isFocused) {
            props.onFocus?.();
        } else {
            props.onBlur?.();
        }
    }, [isFocused]);

    const [isHovering, setHovering] = useState(false);

    const runnerOptions = useMemo(
        () => ({
            dependenciesPath: "turtle",
        }),
        [],
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
                    value={isFocused || isHovering ? {} : undefined}
                    exitAnimationDuration={defaultAnimationDuration}
                    inClassName="animate-in fade-in slide-in-from-bottom-[1px] ease-linear"
                    outClassName="animate-out fade-out slide-out-to-bottom-[1px] ease-linear"
                >
                    {() => (
                        <div className="flex flex-row items-center justify-between">
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

                            <InsertMenu />
                        </div>
                    )}
                </Transition>
            </div>

            <div className="flex flex-col bg-gray-50 dark:bg-gray-900 border-2 border-gray-100 dark:border-gray-800 rounded-md overflow-clip">
                <div className="py-[3px] bg-white dark:bg-[#0d1117]">
                    <CodeMirror onChange={props.onChange}>{props.children}</CodeMirror>
                </div>

                <Animated direction="vertical" clip>
                    <Runner options={runnerOptions} runtime={Turtle}>
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
    );
};

const ColorBlock = (props: { className: string }) => (
    <div
        className={`-ml-3 w-5 h-5 rounded-md border-2 border-gray-100 dark:border-gray-800 transition-transform ${props.className}`}
    />
);
