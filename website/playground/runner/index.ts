import { useEffect, useMemo, useRef, useState } from "react";
import Runner from "../runner/worker?worker";

export interface AnalysisOutput {
    diagnostics: AnalysisOutputDiagnostic[];
    syntaxHighlighting: AnalysisOutputSyntaxHighlightingItem[];
    completions: AnalysisOutputCompletions;
}

export interface AnalysisOutputDiagnostic {
    level: "warning" | "error";
    message: string;
    fix?: AnalysisConsoleDiagnosticFix;
    notes: {
        code: string;
        span: {
            file: string;
            start: number;
            end: number;
        };
        messages: string[];
    }[];
}

export interface AnalysisConsoleDiagnosticFix {
    description: string;
    start: number;
    end: number;
    replacement: string;
}

export interface AnalysisOutputSyntaxHighlightingItem {
    start: number;
    end: number;
    kind: string;
}

export interface HoverOutput {
    code: string;
    help: string;
    url?: string;
}

export interface AnalysisOutputCompletions {
    language: Completion[];
    groupedConstants: [string, Completion[]][];
    ungroupedConstants: Completion[];
    variables: Completion[];
}

export interface Completion {
    kind: string;
    name: string;
    help: string;
    template: string;
}

export type AnalysisConsoleRequest =
    | {
          type: "display";
          text: string;
          callback: () => void;
      }
    | {
          type: "prompt";
          prompt: string;
          sendInput: (input: string) => void;
          recvValid: () => Promise<boolean>;
          callback: () => void;
      }
    | {
          type: "choice";
          prompt: string;
          choices: string[];
          callback: (index: number) => void;
      }
    | {
          type: "loadUi";
          url: string;
          callback: () => void;
      }
    | {
          type: "messageUi";
          message: string;
          value: string;
          callback: (value: any) => void;
      };

export const useRunner = (context: any) => {
    const runner = useRef<Worker | null>(null);

    const newRunnerPromise = () =>
        new Promise<void>((resolve, reject) => {
            runner.current = new Runner();

            runner.current.onmessage = (event) => {
                switch (event.data.type) {
                    case "loaded":
                        console.log("runner loaded");
                        runner.current!.onmessage = null;
                        resolve();
                        setLoaded(true);
                        break;
                    default:
                        runner.current!.onmessage = null;
                        reject(new Error("invalid operation"));
                        break;
                }
            };

            runner.current!.postMessage({ operation: "checkLoading" });
        });

    const runnerPromise = useRef<Promise<void>>();
    const [loaded, setLoaded] = useState(false);

    const reset = () => {
        runner.current?.terminate();
        runner.current = null;

        setLoaded(false);
        runnerPromise.current = newRunnerPromise();
    };

    useEffect(() => {
        reset();
    }, []);

    return {
        isLoaded: loaded,
        waitForLoad: () => runnerPromise.current!,
        analyze: (
            code: string,
            lint: boolean,
            handlePlugin: (path: string, name: string, input: any, api: any) => Promise<any>
        ) =>
            new Promise<AnalysisOutput>(async (resolve, reject) => {
                await runnerPromise.current;

                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = (event) => {
                    try {
                        switch (event.data.type) {
                            case "plugin":
                                const api = {
                                    // TODO
                                };

                                handlePlugin(
                                    event.data.path,
                                    event.data.name,
                                    event.data.input,
                                    api
                                )
                                    .then((output) => {
                                        runner.current!.postMessage({
                                            operation: "pluginSuccessCallback",
                                            id: event.data.id,
                                            output,
                                        });
                                    })
                                    .catch((error) => {
                                        runner.current!.postMessage({
                                            operation: "pluginFailureCallback",
                                            id: event.data.id,
                                            error,
                                        });
                                    });

                                break;
                            case "done":
                                resolve(event.data.analysis);
                                runner.current!.onmessage = prevonmessage;
                                break;
                            default:
                                console.error("received invalid event:", event);
                                throw new Error("invalid operation");
                        }
                    } catch (error) {
                        console.error("[bridge] error:", error);
                    }
                };

                runner.current!.onerror = (event) => {
                    reject(event.error);
                    reset();
                };

                runner.current!.postMessage({ operation: "analyze", code, lint, context });
            }),
        run: (handleConsole: (request: AnalysisConsoleRequest) => void) =>
            new Promise<void>(async (resolve, reject) => {
                await runnerPromise.current;

                let functions: any[] = [];
                let resolveFunctionResult: ((value: any) => void) | undefined;

                const encodeFunction = (value: any) => {
                    if (typeof value === "function") {
                        const length = functions.push(value);
                        return { $function: length - 1 };
                    } else {
                        return value;
                    }
                };

                const decodeFunction = (value: any) => {
                    if (typeof value === "object" && value != null && "$function" in value) {
                        return (input: any) =>
                            new Promise((resolve) => {
                                resolveFunctionResult = resolve;

                                runner.current!.postMessage({
                                    operation: "callFunction",
                                    id: value.$function,
                                    input,
                                });
                            });
                    } else {
                        return value;
                    }
                };

                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = async (event) => {
                    try {
                        switch (event.data.type) {
                            case "display":
                                handleConsole({
                                    type: "display",
                                    text: event.data.text,
                                    callback: () => {
                                        runner.current!.postMessage({
                                            operation: "displayCallback",
                                            id: event.data.id,
                                        });
                                    },
                                });

                                break;
                            case "prompt":
                                handleConsole({
                                    type: "prompt",
                                    prompt: event.data.prompt,
                                    sendInput: (input) => {
                                        runner.current!.postMessage({
                                            operation: "sendPromptInput",
                                            input,
                                            id: event.data.id,
                                        });
                                    },
                                    recvValid: () =>
                                        new Promise((resolve, reject) => {
                                            const prevonmessage = runner.current!.onmessage;
                                            runner.current!.onmessage = (event) => {
                                                resolve(event.data);
                                                runner.current!.onmessage = prevonmessage;
                                            };

                                            runner.current!.onerror = (event) => {
                                                reject(event.error);
                                                reset();
                                            };

                                            runner.current!.postMessage({
                                                operation: "recvPromptValid",
                                                id: event.data.id,
                                            });
                                        }),
                                    callback: () => {
                                        runner.current!.postMessage({
                                            operation: "promptCallback",
                                            id: event.data.id,
                                        });
                                    },
                                });

                                break;
                            case "choice":
                                handleConsole({
                                    type: "choice",
                                    prompt: event.data.prompt,
                                    choices: event.data.choices,
                                    callback: (index) => {
                                        runner.current!.postMessage({
                                            operation: "choiceCallback",
                                            index,
                                            id: event.data.id,
                                        });
                                    },
                                });

                                break;
                            case "loadUi":
                                handleConsole({
                                    type: "loadUi",
                                    url: event.data.url,
                                    callback: () => {
                                        runner.current!.postMessage({
                                            operation: "loadUiCallback",
                                            id: event.data.id,
                                        });
                                    },
                                });

                                break;
                            case "messageUi":
                                handleConsole({
                                    type: "messageUi",
                                    message: event.data.message,
                                    value: decodeFunction(event.data.value),
                                    callback: (value) => {
                                        runner.current!.postMessage({
                                            operation: "messageUiCallback",
                                            value: encodeFunction(value),
                                            id: event.data.id,
                                        });
                                    },
                                });

                                break;
                            case "callFunction":
                                const result = await functions[event.data.id](
                                    decodeFunction(event.data.input)
                                );

                                requestAnimationFrame(() => {
                                    runner.current!.postMessage({
                                        operation: "callFunctionResult",
                                        result: encodeFunction(result),
                                    });
                                });

                                break;
                            case "callFunctionResult":
                                resolveFunctionResult!(event.data.result);
                                resolveFunctionResult = undefined;
                                break;
                            case "done":
                                if (event.data.error) {
                                    reject(event.data.error);
                                } else {
                                    resolve();
                                }

                                runner.current!.onmessage = prevonmessage;
                                break;
                            default:
                                console.error("received invalid event:", event);
                                throw new Error("invalid operation");
                        }
                    } catch (error) {
                        console.error("[bridge] error:", error);
                    }
                };

                runner.current!.onerror = (event) => {
                    reject(event.error);
                    reset();
                };

                runner.current!.postMessage({ operation: "run", context });
            }),
        hover: (start: number, end: number) =>
            new Promise<HoverOutput | null>(async (resolve, reject) => {
                await runnerPromise.current;

                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                    runner.current!.onmessage = prevonmessage;
                };

                runner.current!.onerror = (event) => {
                    reject(event.error);
                    reset();
                };

                runner.current!.postMessage({ operation: "hover", start, end, context });
            }),
        completions: (position: number) =>
            new Promise<AnalysisOutputCompletions>(async (resolve, reject) => {
                await runnerPromise.current;

                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                    runner.current!.onmessage = prevonmessage;
                };

                runner.current!.onerror = (event) => {
                    reject(event.error);
                    reset();
                };

                runner.current!.postMessage({ operation: "completions", position, context });
            }),
        format: (code: string) =>
            new Promise<string | undefined>(async (resolve, reject) => {
                await runnerPromise.current;

                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                    runner.current!.onmessage = prevonmessage;
                };

                runner.current!.onerror = (event) => {
                    reject(event.error);
                    reset();
                };

                runner.current!.postMessage({ operation: "format", code, context });
            }),
    };
};
