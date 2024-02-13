import { useEffect, useState } from "react";
import { Menu, Transition, defaultAnimationDuration } from "../../components";
import { CodeMirror } from "./codemirror";

export const CodeEditor = (props: {
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
            <div className="ml-auto mr-[14px] h-[14px] z-10">
                <Transition
                    value={isFocused || isHovering ? {} : undefined}
                    exitAnimationDuration={defaultAnimationDuration}
                    inClassName="animate-in fade-in ease-linear"
                    outClassName="animate-out fade-out"
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

            <div className="flex flex-col bg-white dark:bg-gray-900 border-2 border-gray-100 dark:border-gray-800 rounded-md px-3 py-2.5">
                <CodeMirror
                    onChange={() => {
                        // TODO
                    }}
                >
                    Hello, world!
                </CodeMirror>
            </div>
        </div>
    );
};
