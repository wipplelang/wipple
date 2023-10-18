import { useEffect, useRef, useState } from "react";
import Runner from "./worker?worker&inline";

export interface AnalysisOutput {
    diagnostics: AnalysisOutputDiagnostic[];
    syntaxHighlighting: AnalysisOutputSyntaxHighlightingItem[];
    snippets: AnalysisOutputSnippets;
}

export interface AnalysisOutputDiagnostic {
    level: "warning" | "error";
    message: string;
    fix?: AnalysisConsoleDiagnosticFix;
    example?: string;
    notes: {
        code: string;
        span: {
            file?: string;
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

export interface AnalysisOutputSnippets {
    wrapping: Snippet[];
    nonwrapping: Snippet[];
}

export interface Snippet {
    id: number;
    name: string;
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

    const reset = () => {
        runner.current?.terminate();
        runner.current = new Runner();
    };

    useEffect(() => {
        reset();
    }, []);

    return {
        analyze: (code: string, lint: boolean, setup: string | undefined) =>
            new Promise<AnalysisOutput>(async (resolve, reject) => {
                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = async (event) => {
                    try {
                        switch (event.data.type) {
                            case "plugin":
                                const api = {
                                    // TODO
                                };

                                try {
                                    console.log("Received plugin request:", {
                                        path: event.data.path,
                                        name: event.data.name,
                                        input: event.data.input,
                                        api,
                                    });

                                    const plugin = await import(/* @vite-ignore */ event.data.path);

                                    if (!(event.data.name in plugin)) {
                                        throw new Error(
                                            `no such plugin '${event.data.name}' in file`
                                        );
                                    }

                                    const output = plugin[event.data.name](event.data.input, api);

                                    runner.current!.postMessage({
                                        operation: "pluginSuccessCallback",
                                        id: event.data.id,
                                        output,
                                    });
                                } catch (error) {
                                    runner.current!.postMessage({
                                        operation: "pluginFailureCallback",
                                        id: event.data.id,
                                        error,
                                    });
                                }

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

                runner.current!.postMessage({ operation: "analyze", code, lint, setup, context });
            }),
        run: (handleConsole: (request: AnalysisConsoleRequest) => void) =>
            new Promise<void>(async (resolve, reject) => {
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

                                runner.current!.postMessage({
                                    operation: "callFunctionResult",
                                    result: encodeFunction(result),
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
        snippets: (position: number) =>
            new Promise<AnalysisOutputSnippets>(async (resolve, reject) => {
                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                    runner.current!.onmessage = prevonmessage;
                };

                runner.current!.onerror = (event) => {
                    reject(event.error);
                    reset();
                };

                runner.current!.postMessage({ operation: "snippets", position, context });
            }),
        expandSnippet: (snippet: number, wrappedCode: string | null) =>
            new Promise<string | null>(async (resolve, reject) => {
                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                    runner.current!.onmessage = prevonmessage;
                };

                runner.current!.onerror = (event) => {
                    reject(event.error);
                    reset();
                };

                runner.current!.postMessage({ operation: "expandSnippet", snippet, wrappedCode });
            }),
        format: (code: string) =>
            new Promise<string | undefined>(async (resolve, reject) => {
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
