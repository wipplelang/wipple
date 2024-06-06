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
import type * as compiler from "wipple-compiler";
import { CompilerWorker, RunnerWorker } from "../../helpers";
import { InterpreterError } from "wipple-interpreter";
import { Render, RenderedDiagnostic, RenderedHighlight } from "wipple-render";
import { Runtime, RuntimeComponent } from "../../runtimes";
import { MaterialSymbol } from "react-material-symbols";
import { Help, Output } from "../../models";
import { Mutex } from "async-mutex";
import { defaultAnimationDuration } from "../../components";
import { flushSync } from "react-dom";

export interface RunOptions {
    dependenciesPath: string;
}

export interface RunnerRef {
    help: (position: number, code: string) => Help | undefined;
    format: (code: string) => Promise<string>;
}

export interface RunnerProps {
    children: string;
    runtime?: {
        Component: RuntimeComponent<any>;
        settings: any | undefined;
        onChangeSettings: (settings: any) => void;
    };
    render: Render;
    hasFocus: boolean;
    onFocus: () => void;
    onBlur: () => void;
    options: RunOptions;
    onChangeDiagnostics: (diagnostics: RenderedDiagnostic[]) => void;
    onChangeHighlightItems: (highlightItems: Record<string, RenderedHighlight>) => void;
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

    const runnerWorker = useRef<Worker>();

    const resetRunnerWorker = useCallback(async () => {
        if (runnerWorker.current) {
            runnerWorker.current.onerror?.(new ErrorEvent("terminate"));
            runnerWorker.current.terminate();
        }

        if (runtimeRef.current) {
            const mutex = runtimeMutexRef.current;
            const runtime = runtimeRef.current;

            await mutex.runExclusive(async () => {
                await runtime.cleanup();
            });
        }

        const newWorker = new RunnerWorker({ name: `runner-${id}` });
        runnerWorker.current = newWorker;

        return newWorker;
    }, [id]);

    const [showOutput, setShowOutput] = useState(false);
    const [showRunAgain, setShowRunAgain] = useState(false);

    const cachedBuiltinsHelp = useRef<Record<string, any>>();
    const cachedHighlightItems = useRef<Record<string, RenderedHighlight>>();

    const isCompiling = useRef(false);

    const run = useCallback(async () => {
        if (isCompiling.current || !hasWaitedForLayout) {
            return;
        }

        isCompiling.current = true;

        props.onBlur();
        setShowRunAgain(false);

        try {
            let showRunAgain = false;

            if (!cachedBuiltinsHelp.current) {
                cachedBuiltinsHelp.current = await fetchBuiltinsHelp();
            }

            const dependencies = props.options.dependenciesPath
                ? await fetchDependencies(props.options.dependenciesPath)
                : null;

            if (!cachedHighlightItems.current && dependencies) {
                props.render.update(dependencies.interface, dependencies.libraries, null);
                const highlightItems = getHighlightItems(dependencies.interface, props.render);
                cachedHighlightItems.current = highlightItems;
                props.onChangeHighlightItems(highlightItems);
            }

            const compilerWorker = new CompilerWorker({ name: `compiler-${id}` });

            const { compileResult, executable } = await new Promise<{
                compileResult: compiler.Result;
                executable: compiler.linker_Executable;
            }>((resolve) => {
                compilerWorker.onmessage = (event) => {
                    switch (event.data.type) {
                        case "completion": {
                            resolve(event.data);
                            break;
                        }
                        default: {
                            throw new Error(`unsupported message: ${event.data.type}`);
                        }
                    }
                };

                compilerWorker.postMessage({ type: "compile", code, dependencies });
            });

            compilerWorker.terminate();

            props.render.update(
                compileResult.interface,
                [compileResult.library, ...(dependencies?.libraries ?? [])],
                compileResult.ide,
            );

            if (compileResult.diagnostics.length > 0) {
                const renderedDiagnostics = compileResult.diagnostics.flatMap((diagnostic) => {
                    const rendered = props.render.renderDiagnostic(diagnostic);
                    return rendered ? [rendered] : [];
                });

                props.onChangeDiagnostics(renderedDiagnostics);

                if (renderedDiagnostics.some((diagnostic) => diagnostic.severity === "error")) {
                    return;
                }
            } else {
                props.onChangeDiagnostics([]);
            }

            isCompiling.current = false;

            const runnerWorker = await resetRunnerWorker();
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

            await new Promise<void>((resolve, reject) => {
                runnerWorker.onmessage = async (event) => {
                    const { type } = event.data;

                    switch (type) {
                        case "completion": {
                            resolve();
                            break;
                        }
                        case "display": {
                            const { text } = event.data;

                            appendToOutput({ type: "text", text });

                            runnerWorker.postMessage({
                                type: "completion",
                            });

                            break;
                        }
                        case "prompt": {
                            const { prompt } = event.data;

                            appendToOutput({
                                type: "prompt",
                                prompt,
                                onSubmit: (input) =>
                                    new Promise((resolve) => {
                                        const prevonmessage = runnerWorker.onmessage;
                                        runnerWorker.onmessage = async (event) => {
                                            const { type, valid } = event.data;
                                            if (type !== "validate") {
                                                throw new Error("expected 'validate' event");
                                            }

                                            runnerWorker.onmessage = prevonmessage;
                                            resolve(valid);
                                        };

                                        runnerWorker.postMessage({
                                            type: "validate",
                                            input,
                                        });
                                    }),
                            });

                            showRunAgain = true;

                            break;
                        }
                        case "choice": {
                            const { prompt, choices } = event.data;

                            appendToOutput({
                                type: "choice",
                                prompt,
                                choices,
                                onSubmit: async (index) => {
                                    runnerWorker.postMessage({
                                        type: "completion",
                                        index,
                                    });
                                },
                            });

                            showRunAgain = true;

                            break;
                        }
                        case "ui": {
                            const { message, value } = event.data;

                            if (!runtimeRef.current) {
                                throw new InterpreterError(
                                    "cannot send a UI message without a runtime",
                                );
                            }

                            const mutex = runtimeMutexRef.current;
                            const runtime = runtimeRef.current;

                            const result = await mutex.runExclusive(async () => {
                                return await runtime.onMessage(message, value);
                            });

                            runnerWorker.postMessage({
                                type: "response",
                                value: result,
                            });

                            showRunAgain = true;

                            break;
                        }
                        case "error": {
                            const { message } = event.data;

                            appendToOutput({ type: "error", message });

                            break;
                        }
                        case "runnerError": {
                            reject(event.data.error);
                            break;
                        }
                        default:
                            throw new Error("unsupported message from runner");
                    }
                };

                runnerWorker.onerror = (event) => {
                    if (event.type === "terminate") {
                        resolve();
                    } else {
                        reject(event.error);
                    }
                };

                runnerWorker.postMessage({ type: "run", executable });
            });

            setShowRunAgain(showRunAgain);
        } catch (error) {
            console.error(error);
        } finally {
            isCompiling.current = false;

            if (runtimeRef.current) {
                const mutex = runtimeMutexRef.current;
                const runtime = runtimeRef.current;

                await mutex.runExclusive(async () => {
                    await runtime.cleanup();
                });
            }
        }
    }, [
        hasWaitedForLayout,
        code,
        props.runtime != null,
        props.runtime?.settings,
        props.options,
        resetRunnerWorker,
    ]);

    useEffect(() => {
        run();
    }, [run]);

    useImperativeHandle(ref, () => ({
        help: (position: number, code: string): Help | undefined => {
            if (cachedBuiltinsHelp.current != null && cachedBuiltinsHelp.current[code] != null) {
                return {
                    name: code,
                    summary: cachedBuiltinsHelp.current[code].summary,
                    doc: cachedBuiltinsHelp.current[code].doc,
                };
            }

            const declarationPath = props.render.getPathAtCursor("playground", position);
            if (!declarationPath) {
                return undefined;
            }

            const declaration = props.render.getDeclarationFromPath(declarationPath.item);
            if (!declaration) {
                return undefined;
            }

            const documentation = props.render.renderDocumentation(declaration);
            if (!documentation) {
                return undefined;
            }

            const docString = documentation.docs.split("\n\n");

            const declarationString = props.render.renderDeclaration(declaration) ?? undefined;

            return {
                name: declaration.item.name ?? code,
                summary: docString[0],
                doc: docString.slice(1).join("\n\n"),
                declaration: declarationString,
            };
        },
        format: async (code) => {
            const compilerWorker = new CompilerWorker({ name: `compiler-${id}-format` });

            const formatted = await new Promise<string>((resolve) => {
                compilerWorker.onmessage = async (event) => {
                    const { type } = event.data;
                    switch (type) {
                        case "completion": {
                            resolve(event.data.code);
                            break;
                        }
                        default:
                            throw new Error(`unsupported message: ${type}`);
                    }
                };

                compilerWorker.postMessage({ type: "format", code });
            });

            compilerWorker.terminate();

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
                    call={(func, ...inputs) =>
                        new Promise((resolve) => {
                            if (!runnerWorker.current) {
                                throw new Error("runner not initialized");
                            }

                            const prevonmessage = runnerWorker.current.onmessage;
                            runnerWorker.current.onmessage = async (event) => {
                                if (event.data.type !== "completion") {
                                    return prevonmessage?.call(runnerWorker.current!, event);
                                }

                                runnerWorker.current!.onmessage = prevonmessage;
                                resolve(event.data.output);
                            };

                            runnerWorker.current.postMessage({
                                type: "callFunction",
                                func,
                                inputs,
                            });
                        })
                    }
                    stopRunning={async () => {
                        await resetRunnerWorker();
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
                                {item.text}
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
    interface: compiler.Interface;
    libraries: compiler.linker_UnlinkedLibrary[];
}

const fetchDependencies = async (name: string): Promise<FetchDependenciesResult> =>
    fetch(new URL(`/playground/library/${name}.wipplebundle`, window.location.origin)).then(
        (response) => response.json(),
    );

const fetchBuiltinsHelp = async (): Promise<Record<string, any>> =>
    fetch(new URL("/playground/library/help/builtins.json", window.location.origin)).then(
        (response) => response.json(),
    );

const getHighlightItems = (
    interface_: compiler.Interface,
    render: Render,
): Record<string, RenderedHighlight> => {
    const highlightItems: Record<string, RenderedHighlight> = {};
    for (const [name, declarationPaths] of Object.entries(interface_.topLevel)) {
        if (declarationPaths.length > 1) {
            continue;
        }

        for (const declarationPath of declarationPaths) {
            const declaration = render.getDeclarationFromPath(declarationPath.item);
            if (!declaration) {
                continue;
            }

            const highlight = render.renderHighlight(declaration);
            if (highlight) {
                highlightItems[name] = highlight;
                break;
            }
        }
    }

    return highlightItems;
};
