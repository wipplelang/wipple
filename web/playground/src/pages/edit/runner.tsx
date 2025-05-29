import {
    forwardRef,
    useCallback,
    useEffect,
    useId,
    useImperativeHandle,
    useRef,
    useState,
} from "react";
import { Runtime, RuntimeComponent } from "../../runtimes";
import { MaterialSymbol } from "react-material-symbols";
import { Help, Output } from "../../models";
import { Mutex } from "async-mutex";
import { Animated, Markdown, defaultAnimationDuration } from "../../components";
import { flushSync } from "react-dom";
import { decompress } from "fzstd";
import { useUnmountEffect } from "framer-motion";
import { Box } from "../../components/box";

export interface RunOptions {
    bundlePath: string;
}

export interface RunnerRef {
    run: (() => Promise<void>) | undefined;
    stop: (() => Promise<void>) | undefined;
    help: (position: number, code: string) => Promise<Help | undefined>;
    format: (code: string) => Promise<string>;
}

export interface RunnerProps {
    children: string;
    wipple: typeof import("wipple-wasm");
    runtime?: { Component: RuntimeComponent };
    hasFocus: boolean;
    onFocus: () => void;
    onBlur: () => void;
    options: RunOptions;
    onChangeDiagnostics: (diagnostics: any[], driverDiagnostics: any[]) => void;
    onChangeHighlightItems: (highlightItems: Record<string, any>) => void;
    onChangeCompiling?: (isCompiling: boolean) => void;
    onChangeRunning?: (isRunning: boolean) => void;
}

export const Runner = forwardRef<RunnerRef, RunnerProps>((props, ref) => {
    const [hasWaitedForLayout, setHasWaitedForLayout] = useState(false);

    useEffect(() => {
        setTimeout(() => {
            setHasWaitedForLayout(true);
        }, defaultAnimationDuration);
    }, []);

    const id = useId();

    const runtimeRef = useRef<Runtime | null>(null);

    const [output, setOutput] = useState<Output[]>([]);

    const appendToOutput = (item: Output) => {
        setOutput((output) => [...output, item]);
    };

    const clearOutput = () => {
        setOutput([]);
    };

    const cachedHighlightItems = useRef<Record<string, any>>();
    const cachedHelp = useRef<Record<string, Help>>({});

    const [isInitialized, setInitialized] = useState(false);
    const [isCompiling, setCompiling] = useState(false);
    const [isRunning, setRunning] = useState(false);

    const runMutex = useRef(new Mutex());

    const initialize = useCallback(async () => {
        if (isInitialized || !hasWaitedForLayout) {
            return;
        }

        const dependencies = await fetchBundle(props.options.bundlePath);

        await props.wipple.initialize({
            id,
            interfaces: dependencies.interfaces,
            libraries: dependencies.libraries,
        });

        setInitialized(true);

        if (!cachedHighlightItems.current && dependencies) {
            // First, retrieve from cache to speed up highlighting

            const highlightItemsCacheKey = "playground:highlightItems";

            const cachedHighlightItemsString = localStorage.getItem(highlightItemsCacheKey);
            if (cachedHighlightItemsString) {
                try {
                    cachedHighlightItems.current = JSON.parse(cachedHighlightItemsString);
                    props.onChangeHighlightItems(cachedHighlightItems.current!);
                } catch {
                    // ignore
                }
            }

            // Then, fetch the latest highlights

            const highlightsResult = await props.wipple.highlights({ id });

            if (highlightsResult) {
                const { highlights } = highlightsResult;

                cachedHighlightItems.current = highlights;
                props.onChangeHighlightItems(highlights);

                localStorage.setItem(highlightItemsCacheKey, JSON.stringify(highlights));
            }
        }

        try {
            await runtimeRef.current?.initialize();
            await runtimeRef.current?.cleanup();
        } catch (error) {
            console.error(error);
        }
    }, [isInitialized, hasWaitedForLayout, props.options.bundlePath]);

    useEffect(() => {
        initialize();
    }, [initialize]);

    const run = useCallback(async () => {
        if (isCompiling) {
            return;
        }

        runtimeRef.current?.resume?.();

        setCompiling(true);

        props.onBlur();

        try {
            const [compileResult] = await Promise.all([
                props.wipple.compile({
                    id,
                    path: "playground",
                    code: props.children,
                }),

                // Allow animations to finish before proceeding
                new Promise((resolve) => setTimeout(resolve, defaultAnimationDuration * 2)),
            ]);

            flushSync(() => {
                setCompiling(false);
            });

            if (!compileResult) return;

            const { success, diagnostics, driverDiagnostics } = compileResult;
            props.onChangeDiagnostics(diagnostics, driverDiagnostics);

            if (!success) return;

            flushSync(() => {
                setRunning(true);
            });

            await props.wipple.stop({ id });

            const runResult = await runMutex.current.runExclusive(async () => {
                clearOutput();

                await runtimeRef.current?.initialize();

                try {
                    return await props.wipple.run({
                        id,
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
                                }),
                            ui: async (message: string, value: any) => {
                                try {
                                    if (!runtimeRef.current) {
                                        throw new Error(
                                            "cannot send a UI message without a runtime",
                                        );
                                    }

                                    const result = await runtimeRef.current.onMessage(
                                        message,
                                        value,
                                    );

                                    return result;
                                } catch (error) {
                                    console.warn(error);
                                    throw error;
                                }
                            },
                        },
                    });
                } finally {
                    await runtimeRef.current?.cleanup();
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
            requestAnimationFrame(() => {
                flushSync(() => {
                    setCompiling(false);
                    setRunning(false);
                });
            });
        }
    }, [isCompiling, isRunning, props.children, props.runtime != null, props.options]);

    const stop = useCallback(async () => {
        try {
            await props.wipple.stop({ id });
        } finally {
            requestAnimationFrame(() => {
                flushSync(() => {
                    setCompiling(false);
                    setRunning(false);
                });
            });
        }
    }, []);

    useEffect(() => {
        if (!isInitialized) return;

        props.onChangeCompiling?.(isCompiling);
    }, [props.onChangeCompiling, isCompiling]);

    useEffect(() => {
        if (!isInitialized) return;

        props.onChangeRunning?.(isRunning);
    }, [props.onChangeRunning, isRunning]);

    useImperativeHandle(ref, () => ({
        run: isInitialized ? run : undefined,
        stop: isInitialized ? stop : undefined,
        help: async (position: number, code: string): Promise<Help | undefined> => {
            if (cachedHelp.current[code] != null) {
                return cachedHelp.current[code];
            }

            // Ensure the locations of symbols are up to date
            await props.wipple.compile({
                id,
                path: "playground",
                code: props.children,
            });

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

            const docString = help.docs.split("\n\n");

            const resolvedHelp = {
                name: help.name ?? code,
                summary: docString[0],
                declaration: help.declaration,
                doc: docString.slice(1).join("\n\n"),
                example: help.example ?? undefined,
            };

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

    useUnmountEffect(() => {
        props.wipple.cleanup({ id });
    });

    return props.runtime == null && output.length === 0 && !isRunning ? (
        <OutputPlaceholder />
    ) : (
        <Animated direction="vertical" unsized>
            <div className="flex flex-col gap-3">
                {props.runtime ? (
                    <props.runtime.Component
                        id={id}
                        stopRunning={async () => {
                            await props.wipple.stop({ id });
                        }}
                        ref={runtimeRef}
                    />
                ) : null}

                {output.map((item, index) => {
                    let content: JSX.Element;
                    switch (item.type) {
                        case "text":
                            content = (
                                <Box>
                                    <Markdown>{item.text}</Markdown>
                                </Box>
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
            </div>
        </Animated>
    );
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

const OutputPlaceholder = () => (
    <div className="flex-1 flex flex-col items-center justify-center gap-2 rounded-lg mx-4 mb-4  p-4 text-gray-400 dark:text-gray-600">
        <p>No output</p>

        <p className="text-xs">
            Try dragging{" "}
            <code className="border-[1pt] border-gray-200 dark:border-gray-700 rounded-md px-1">
                <MaterialSymbol icon="article" className="mr-px translate-y-0.5" />
                show
            </code>{" "}
            into the space above and then press <strong>Run</strong>.
        </p>
    </div>
);

interface Bundle {
    interfaces: any[];
    libraries: any[];
}

const fetchBundle = async (name: string): Promise<Bundle> => {
    const response = await fetch(
        new URL(`/playground/library/${name}.wipplebundle`, window.location.origin),
    );

    const buffer = await response.arrayBuffer();
    const data = decompress(new Uint8Array(buffer));
    return JSON.parse(new TextDecoder().decode(data));
};
