import {
    forwardRef,
    useCallback,
    useEffect,
    useId,
    useImperativeHandle,
    useRef,
    useState,
} from "react";
import { useDebounceValue } from "usehooks-ts";
import { Runtime, RuntimeComponent } from "../runtimes";
import { MaterialSymbol } from "react-material-symbols";
import { Help, Output } from "../models";
import { Mutex } from "async-mutex";
import { Markdown, defaultAnimationDuration } from "../components";
import { flushSync } from "react-dom";

export interface RunOptions {
    dependenciesPath: string;
}

export interface RunnerRef {
    help: (position: number, code: string) => Promise<Help | undefined>;
    format: (code: string) => Promise<string>;
}

export interface RunnerProps {
    children: string;
    wipple: typeof import("wipple-wasm");
    runtime?: {
        Component: RuntimeComponent<any>;
        settings: any | undefined;
        onChangeSettings: (settings: any) => void;
    };
    hasFocus: boolean;
    onFocus: () => void;
    onBlur: () => void;
    options: RunOptions;
    onChangeDiagnostics: (diagnostics: any[]) => void;
    onChangeHighlightItems: (highlightItems: Record<string, any>) => void;
}

export const Runner = forwardRef<RunnerRef, RunnerProps>((props, ref) => {
    const [hasWaitedForLayout, setHasWaitedForLayout] = useState(false);

    useEffect(() => {
        setTimeout(() => {
            setHasWaitedForLayout(true);
        }, defaultAnimationDuration);
    }, []);

    const id = useId();
    const [code, setCode] = useDebounceValue(props.children, 500);

    const runtimeMutexRef = useRef(new Mutex());
    const runtimeRef = useRef<Runtime | null>(null);

    useEffect(() => {
        setCode(props.children);
    }, [props.children]);

    const [output, setOutput] = useState<Output[]>([]);

    const appendToOutput = (item: Output) => {
        setOutput((output) => [...output, item]);
        setShowOutput(true);
    };

    const clearOutput = () => {
        setOutput([]);
        setShowOutput(false);
    };

    const [showOutput, setShowOutput] = useState(false);
    const [showRunAgain, setShowRunAgain] = useState(false);

    const cachedBuiltinsHelp = useRef<Record<string, any>>();
    const cachedHighlightItems = useRef<Record<string, any>>();
    const cachedHelp = useRef<Record<string, Help>>({});

    const isCompiling = useRef(false);

    const runMutex = useRef(new Mutex());

    const run = useCallback(async () => {
        if (isCompiling.current || !hasWaitedForLayout) {
            return;
        }

        isCompiling.current = true;

        props.onBlur();
        setShowRunAgain(false);

        let showRunAgain = false;
        try {
            if (!cachedBuiltinsHelp.current) {
                cachedBuiltinsHelp.current = await fetchBuiltinsHelp();
            }

            const dependencies = props.options.dependenciesPath
                ? await fetchDependencies(props.options.dependenciesPath)
                : null;

            if (!cachedHighlightItems.current && dependencies) {
                const highlightsResult = await props.wipple.highlights({
                    interface: dependencies.interface,
                });

                if (highlightsResult) {
                    const { highlights } = highlightsResult;

                    cachedHighlightItems.current = highlights;
                    props.onChangeHighlightItems(highlights);
                }
            }

            const compileResult = await props.wipple.compile({
                id,
                path: "playground",
                code,
                interface: dependencies?.interface,
                libraries: dependencies?.libraries ?? [],
            });

            isCompiling.current = false;

            if (!compileResult) return;

            const { executable, diagnostics } = compileResult;
            props.onChangeDiagnostics(diagnostics);

            if (!executable) return;

            await props.wipple.stop({ id });

            const runResult = await runMutex.current.runExclusive(async () => {
                clearOutput();

                // flushSync is required to initialize runtimeRef
                flushSync(() => {
                    setShowOutput(props.runtime != null);
                });

                if (runtimeRef.current) {
                    const mutex = runtimeMutexRef.current;
                    const runtime = runtimeRef.current;

                    await mutex.runExclusive(async () => {
                        await runtime.initialize();
                    });
                }

                try {
                    return await props.wipple.run({
                        id,
                        executable,
                        handlers: {
                            display: async (text: string) => {
                                appendToOutput({ type: "text", text });
                                await new Promise(requestAnimationFrame);
                            },
                            prompt: async (
                                prompt: string,
                                validate: (input: string) => Promise<boolean>,
                            ) => {
                                appendToOutput({
                                    type: "prompt",
                                    prompt,
                                    onSubmit: validate,
                                });

                                showRunAgain = true;

                                await new Promise(requestAnimationFrame);
                            },
                            choice: (prompt: string, choices: string[]) =>
                                new Promise<number>((resolve) => {
                                    appendToOutput({
                                        type: "choice",
                                        prompt,
                                        choices,
                                        onSubmit: resolve,
                                    });

                                    showRunAgain = true;
                                }),
                            ui: async (message: string, value: any) => {
                                try {
                                    if (!runtimeRef.current) {
                                        throw new Error(
                                            "cannot send a UI message without a runtime",
                                        );
                                    }

                                    const mutex = runtimeMutexRef.current;
                                    const runtime = runtimeRef.current;

                                    const result = await mutex.runExclusive(async () => {
                                        return await runtime.onMessage(message, value);
                                    });

                                    showRunAgain = true;

                                    return result;
                                } catch (error) {
                                    console.warn(error);
                                    throw error;
                                }
                            },
                        },
                    });
                } finally {
                    if (runtimeRef.current) {
                        const mutex = runtimeMutexRef.current;
                        const runtime = runtimeRef.current;

                        await mutex.runExclusive(async () => {
                            await runtime.cleanup();
                        });
                    }
                }
            });

            if (!runResult) return;

            const { error } = runResult;

            if (error) {
                appendToOutput({ type: "error", message: error });
            }
        } catch (error) {
            console.error(error);
        } finally {
            setShowRunAgain(showRunAgain);
            isCompiling.current = false;
        }
    }, [hasWaitedForLayout, code, props.runtime != null, props.runtime?.settings, props.options]);

    useEffect(() => {
        run();
    }, [run]);

    useImperativeHandle(ref, () => ({
        help: async (position: number, code: string): Promise<Help | undefined> => {
            const helpFromDocumentation = (documentation: {
                name: string;
                declaration: string | undefined;
                docs: string;
                example: string | null;
            }): Help => {
                const docString = documentation.docs.split("\n\n");

                return {
                    name: documentation.name,
                    summary: docString[0],
                    declaration: documentation.declaration,
                    doc: docString.slice(1).join("\n\n"),
                    example: documentation.example ?? undefined,
                };
            };

            if (cachedBuiltinsHelp.current != null && cachedBuiltinsHelp.current[code] != null) {
                return helpFromDocumentation({
                    name: code,
                    declaration: undefined,
                    ...cachedBuiltinsHelp.current[code],
                });
            }

            if (cachedHelp.current[code] != null) {
                return cachedHelp.current[code];
            }

            const helpResult = await props.wipple.help({
                id,
                path: "playground",
                position,
            });

            if (!helpResult) {
                return undefined;
            }

            const { help } = helpResult;
            if (!help) {
                return undefined;
            }

            const resolvedHelp = helpFromDocumentation({
                name: help.name ?? code,
                declaration: help.declaration,
                docs: help.docs,
                example: help.example,
            });

            cachedHelp.current[code] = resolvedHelp;

            return resolvedHelp;
        },
        format: async (code) => {
            const formatResult = await props.wipple.format({ code });

            if (!formatResult) {
                return code;
            }

            const { code: formatted } = formatResult;

            return formatted;
        },
    }));

    return showOutput ? (
        <div className="flex flex-col px-4 pb-4 gap-3">
            {props.runtime ? (
                <props.runtime.Component
                    id={id}
                    settings={props.runtime.settings}
                    onChangeSettings={props.runtime.onChangeSettings}
                    stopRunning={async () => {
                        await props.wipple.stop({ id });
                        setShowRunAgain(true);
                    }}
                    ref={runtimeRef}
                />
            ) : null}

            {output.map((item, index) => {
                let content: JSX.Element;
                switch (item.type) {
                    case "text":
                        content = (
                            <div className="bg-gray-50 dark:bg-gray-900 p-3 rounded-lg">
                                <Markdown>{item.text}</Markdown>
                            </div>
                        );
                        break;
                    case "prompt":
                        content = (
                            <Prompt
                                autoFocus={props.hasFocus}
                                prompt={item.prompt}
                                validate={item.onSubmit}
                                onFocus={props.onFocus}
                            />
                        );
                        break;
                    case "choice":
                        content = <div>TODO</div>;
                        break;
                    case "error":
                        content = (
                            <div className="flex flex-row items-center gap-2 bg-red-50 dark:bg-red-950 text-red-500 p-3 rounded-lg">
                                <MaterialSymbol icon="error_circle_rounded" size={20} />
                                {item.message}
                            </div>
                        );
                        break;
                    default:
                        item satisfies never;
                        throw new Error("unreachable");
                }

                return <div key={index}>{content}</div>;
            })}

            {showRunAgain ? (
                <div className="flex flex-col items-start">
                    <button
                        onClick={run}
                        className="flex flex-row items-center gap-2 bg-blue-500 bg-opacity-10 text-blue-500 dark:text-blue-400 hover:bg-opacity-100 hover:text-white rounded-lg px-3 py-1.5 transition-colors"
                    >
                        <MaterialSymbol icon="replay" className="text-lg -scale-x-100" />
                        Run Again
                    </button>
                </div>
            ) : null}
        </div>
    ) : null;
});

const Prompt = (props: {
    autoFocus: boolean;
    prompt: string;
    validate: (value: string) => Promise<boolean>;
    onFocus: () => void;
}) => {
    const [value, setValue] = useState("");
    const [valid, setValid] = useState(true);
    const [disabled, setDisabled] = useState(false);

    const inputRef = useRef<HTMLInputElement>(null);

    useEffect(() => {
        setValid(true);
    }, [value]);

    const onSubmit = async () => {
        setDisabled(true);
        setValid(true);

        const valid = await props.validate(value);

        setValid(valid);
        setDisabled(valid);

        if (!valid) {
            requestAnimationFrame(() => {
                inputRef.current?.focus();
            });
        }
    };

    return (
        <form
            onSubmit={(e) => {
                e.preventDefault();
                onSubmit();
            }}
            className={`relative ${valid ? "" : "shake"} mt-1`}
        >
            <input
                ref={inputRef}
                autoFocus={props.autoFocus}
                onFocus={props.onFocus}
                disabled={disabled}
                placeholder={props.prompt}
                onChange={(e) => setValue(e.target.value)}
                className={`w-full rounded-md px-3 py-2.5 outline ${
                    valid ? "outline-gray-300 dark:outline-gray-700" : "outline-red-500"
                } ${
                    disabled
                        ? "bg-gray-50 dark:bg-gray-900"
                        : valid
                        ? "bg-white dark:bg-gray-950 focus:outline-blue-500"
                        : "bg-red-50 dark:bg-red-950"
                }`}
            />

            <button
                type="submit"
                disabled={disabled}
                className="absolute top-0 bottom-0 right-3 my-auto flex items-center justify-center w-6 h-6 bg-blue-500 disabled:bg-gray-300 disabled:dark:bg-gray-800 rounded-md"
            >
                <MaterialSymbol icon="arrow_forward" className="text-white text-xl" />
            </button>
        </form>
    );
};

interface FetchDependenciesResult {
    interface: any;
    libraries: any[];
}

const fetchDependencies = async (name: string): Promise<FetchDependenciesResult> =>
    fetch(new URL(`/playground/library/${name}.wipplebundle`, window.location.origin)).then(
        (response) => response.json(),
    );

const fetchBuiltinsHelp = async (): Promise<Record<string, any>> =>
    fetch(new URL("/playground/library/help/builtins.json", window.location.origin)).then(
        (response) => response.json(),
    );
