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
import { compile, link, renderDiagnostics } from "wipple-compiler";
import { RunnerWorker } from "../../helpers";
import { InterpreterError } from "wipple-interpreter";
import { Runtime, RuntimeComponent } from "../../runtimes";
import { MaterialSymbol } from "react-material-symbols";
import { Diagnostic, Help, Output } from "../../models";
import { Mutex } from "async-mutex";

export interface RunOptions {
    dependenciesPath: string;
}

export interface RunnerRef {
    help: (code: string) => Help | undefined;
}

export interface RunnerProps {
    children: string;
    runtime?: RuntimeComponent;
    hasFocus: boolean;
    onFocus: () => void;
    onBlur: () => void;
    options: RunOptions;
    onChangeDiagnostics: (diagnostics: Diagnostic[]) => void;
}

export const Runner = forwardRef<RunnerRef, RunnerProps>((props, ref) => {
    const id = useId();
    const [code, setCode] = useDebounceValue(props.children, 300);

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

    const resetRunnerWorker = useCallback(() => {
        runnerWorker.current?.terminate();

        const newWorker = new RunnerWorker({ name: `runner-${id}` });
        runnerWorker.current = newWorker;

        return newWorker;
    }, [id]);

    const [isRunning, setRunning] = useState(false);
    const [showOutput, setShowOutput] = useState(false);
    const [showRunAgain, setShowRunAgain] = useState(false);

    const [cachedInterface, setCachedInterface] = useState<any>();
    const [cachedBuiltinsHelp, setCachedBuiltinsHelp] = useState<Record<string, any>>();

    const run = useCallback(async () => {
        props.onBlur();
        setRunning(true);
        setShowRunAgain(false);

        try {
            let showRunAgain = false;

            const sources = [
                {
                    path: "playground",
                    visiblePath: "playground",
                    code,
                },
            ];

            if (!cachedBuiltinsHelp) {
                setCachedBuiltinsHelp(await fetchBuiltinsHelp());
            }

            const dependencies = props.options.dependenciesPath
                ? await fetchDependencies(props.options.dependenciesPath)
                : null;

            const sourceFiles = [{ path: "playground", code }];

            if (dependencies != null) {
                sourceFiles.push(...dependencies.sourceFiles);
            }

            const compileResult = compile(sources, dependencies?.interface);
            setCachedInterface(compileResult.interface);

            if (compileResult.diagnostics.length > 0) {
                const sourceCodeForFile = (file: string) => {
                    const sourceFile = sourceFiles.find(({ path }) => path === file);
                    return sourceFile?.code ?? "";
                };

                const renderedDiagnostics = renderDiagnostics(
                    compileResult.diagnostics,
                    compileResult.interface,
                    compileResult.library,
                    sourceCodeForFile,
                );

                props.onChangeDiagnostics(renderedDiagnostics);
                setRunning(false);

                if (renderedDiagnostics.some((diagnostic: any) => diagnostic.error)) {
                    return;
                }
            } else {
                props.onChangeDiagnostics([]);
                setRunning(false);
            }

            const linkResult = link([compileResult.library, ...(dependencies?.libraries ?? [])]);

            if (linkResult.Err) {
                throw new Error(`failed to link libraries: ${linkResult.Err}`);
            }

            const executable = linkResult.Ok;

            const runnerWorker = resetRunnerWorker();
            clearOutput();

            setShowOutput(props.runtime != null);

            if (runtimeRef.current) {
                const mutex = runtimeMutexRef.current;
                const runtime = runtimeRef.current;

                await mutex.runExclusive(async () => {
                    await runtime.initialize();
                });
            }

            await new Promise<void>((resolve) => {
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
                        default:
                            throw new Error("unsupported message from runner");
                    }
                };

                runnerWorker.postMessage({ type: "run", executable });
            });

            setRunning(false);
            setShowRunAgain(showRunAgain);
        } catch (error) {
            console.error(error);
        } finally {
            if (runtimeRef.current) {
                const mutex = runtimeMutexRef.current;
                const runtime = runtimeRef.current;

                await mutex.runExclusive(async () => {
                    await runtime.cleanup();
                });
            }
        }
    }, [code, props.runtime, props.options, resetRunnerWorker, cachedBuiltinsHelp]);

    useEffect(() => {
        run();
    }, [run]);

    useImperativeHandle(ref, () => ({
        help: (code: string): Help | undefined => {
            if (cachedBuiltinsHelp != null && cachedBuiltinsHelp[code] != null) {
                return {
                    name: code,
                    summary: cachedBuiltinsHelp[code].summary,
                    doc: cachedBuiltinsHelp[code].doc,
                };
            }

            if (!cachedInterface) {
                return undefined;
            }

            const comments =
                cachedInterface.topLevel[code]?.[0]?.info.parserInfo.documentation.flatMap(
                    (documentation: any) => (documentation.comment ? [documentation.comment] : []),
                ) ?? [];

            if (comments.length === 0) {
                return undefined;
            }

            return {
                name: code,
                summary: comments[0],
                doc: comments.slice(1).join("\n"),
            };
        },
    }));

    return isRunning || showOutput ? (
        <div className="flex flex-col px-4 pb-4 gap-3">
            {props.runtime ? <props.runtime id={id} ref={runtimeRef} /> : null}

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

            {isRunning ? (
                <div className="bouncing-loader">
                    <div />
                    <div />
                    <div />
                </div>
            ) : null}

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
    libraries: any;
    sourceFiles: { path: string; code: string }[];
}

const fetchDependencies = async (name: string): Promise<FetchDependenciesResult> => {
    const baseUrlString = import.meta.env.VITE_DEPENDENCIES_BASE_URL as string;

    if (!baseUrlString) {
        throw new Error("missing VITE_DEPENDENCIES_BASE_URL");
    }

    return fetch(new URL(`${name}.wipplebundle`, baseUrlString)).then((response) =>
        response.json(),
    );
};

const fetchBuiltinsHelp = async (): Promise<Record<string, string>> => {
    const baseUrlString = import.meta.env.VITE_DEPENDENCIES_BASE_URL as string;

    if (!baseUrlString) {
        throw new Error("missing VITE_DEPENDENCIES_BASE_URL");
    }

    return fetch(new URL("help/builtins.json", baseUrlString)).then((response) => response.json());
};
