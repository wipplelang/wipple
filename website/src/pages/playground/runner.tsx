import { useCallback, useEffect, useId, useRef, useState } from "react";
import { useDebounceValue } from "usehooks-ts";
import { compile, link, renderDiagnostics } from "wipple-compiler";
import { RunnerWorker } from "../../helpers";
import { InterpreterError } from "wipple-interpreter";
import { Runtime, RuntimeComponent } from "../../runtimes";
import { MaterialSymbol } from "react-material-symbols";

export interface RunOptions {
    dependenciesPath: string;
}

type Diagnostic = unknown; // TODO

type Output =
    | { type: "text"; text: string }
    | { type: "prompt"; prompt: string; onSubmit: (value: string) => Promise<boolean> }
    | {
          type: "choice";
          prompt: string;
          choices: string[];
          onSubmit: (choice: number) => Promise<void>;
      };

export const Runner = (props: {
    children: string;
    runtime?: RuntimeComponent;
    options: RunOptions;
}) => {
    const id = useId();
    const [code, setCode] = useDebounceValue(props.children, 300);

    const runtime = useRef<Runtime | null>(null);

    useEffect(() => {
        setCode(props.children);
    }, [props.children]);

    const [diagnostics, setDiagnostics] = useState<Diagnostic[]>([]);

    const [output, setOutput] = useState<Output[]>([]);

    const appendToOutput = (item: Output) => {
        setOutput((output) => [...output, item]);
    };

    const clearOutput = () => {
        setOutput([]);
    };

    const runnerWorker = useRef<Worker>();

    const resetRunnerWorker = useCallback(() => {
        runnerWorker.current?.terminate();

        const newWorker = new RunnerWorker({ name: `runner-${id}` });
        runnerWorker.current = newWorker;

        return newWorker;
    }, [id]);

    useEffect(() => {
        (async () => {
            try {
                const sources = [
                    {
                        path: "playground",
                        visiblePath: "playground",
                        code,
                    },
                ];

                const dependencies = props.options.dependenciesPath
                    ? await fetchDependencies(props.options.dependenciesPath)
                    : null;

                const sourceFiles = [{ path: "playground", code }];

                if (dependencies != null) {
                    sourceFiles.push(...dependencies.sourceFiles);
                }

                const compileResult = compile(sources, dependencies?.interface);

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

                    setDiagnostics(renderedDiagnostics);

                    if (renderedDiagnostics.some((diagnostic: any) => diagnostic.error)) {
                        return;
                    }
                } else {
                    setDiagnostics([]);
                }

                const linkResult = link([
                    compileResult.library,
                    ...(dependencies?.libraries ?? []),
                ]);

                if (linkResult.Err) {
                    throw new Error(`failed to link libraries: ${linkResult.Err}`);
                }

                const executable = linkResult.Ok;

                const runnerWorker = resetRunnerWorker();
                clearOutput();

                runnerWorker.onmessage = async (event) => {
                    const { type } = event.data;

                    switch (type) {
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

                            break;
                        }
                        case "ui": {
                            const { message, value } = event.data;

                            if (!runtime.current) {
                                throw new InterpreterError(
                                    "cannot send a UI message without a runtime",
                                );
                            }

                            const result = await runtime.current.onMessage(message, value);

                            runnerWorker.postMessage({
                                type: "response",
                                value: result,
                            });

                            break;
                        }
                        default:
                            throw new Error("unsupported message from runner");
                    }
                };

                runnerWorker.postMessage({ type: "run", executable });

                // TODO: Run via worker
            } catch (error) {
                console.error(error);
            }
        })();
    }, [code, runtime, props.options, runnerWorker, resetRunnerWorker]);

    return output.length > 0 || diagnostics.length > 0 ? (
        <div className="flex flex-col p-4 gap-2">
            {diagnostics.length > 0 ? (
                <div className="bg-red-100 border border-red-200 p-2 rounded-md">
                    <ul>
                        {diagnostics.map((diagnostic, index) => (
                            <li key={index}>{JSON.stringify(diagnostic, null, 4)}</li>
                        ))}
                    </ul>
                </div>
            ) : null}

            {props.runtime ? <props.runtime id={id} ref={runtime} /> : null}

            {output.map((item, index) => {
                let content: JSX.Element;
                switch (item.type) {
                    case "text":
                        content = <div>{item.text}</div>;
                        break;
                    case "prompt":
                        content = <Prompt prompt={item.prompt} validate={item.onSubmit} />;
                        break;
                    case "choice":
                        content = <div>TODO</div>;
                        break;
                    default:
                        item satisfies never;
                        throw new Error("unreachable");
                }

                return <div key={index}>{content}</div>;
            })}
        </div>
    ) : null;
};

const Prompt = (props: { prompt: string; validate: (value: string) => Promise<boolean> }) => {
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
            className={`relative ${valid ? "" : "shake"}`}
        >
            <input
                ref={inputRef}
                disabled={disabled}
                placeholder={props.prompt}
                onChange={(e) => setValue(e.target.value)}
                className={`w-full rounded-md p-2 outline ${
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
                className="absolute top-0 bottom-0 right-2 my-auto flex items-center justify-center w-6 h-6 bg-blue-500 disabled:bg-gray-300 disabled:dark:bg-gray-800 rounded-md"
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

    return fetch(new URL(`${name}.json`, baseUrlString)).then((response) => response.json());
};
