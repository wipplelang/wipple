import { useEffect, useRef } from "react";
import Runner from "../runner/worker?worker";

export interface AnalysisOutput {
    diagnostics: AnalysisOutputDiagnostic[];
    syntaxHighlighting: AnalysisOutputSyntaxHighlightingItem[];
    completions: AnalysisOutputCompletions;
}

export interface AnalysisOutputDiagnostic {
    level: "warning" | "error";
    message: string;
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

export interface AnalysisOutputSyntaxHighlightingItem {
    start: number;
    end: number;
    kind: string;
}

export interface HoverOutput {
    code: string;
    help: string;
}

export interface AnalysisOutputCompletions {
    variables: Completion[];
    groupedConstants: [string, Completion[]][];
    ungroupedConstants: Completion[];
}

export interface Completion {
    kind: string;
    name: string;
    help: string;
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

    useEffect(() => {
        const setup = async () => {
            runner.current = new Runner();
        };

        setup();

        return () => runner.current!.terminate();
    }, []);

    return {
        analyze: (code: string, lint: boolean) =>
            new Promise<AnalysisOutput>((resolve, reject) => {
                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                    runner.current!.onmessage = prevonmessage;
                };

                const prevonerror = runner.current!.onerror;
                runner.current!.onerror = (event) => {
                    reject(event.error);
                    runner.current!.onerror = prevonerror;
                };

                runner.current!.postMessage({ operation: "analyze", code, lint, context });
            }),
        run: (handleConsole: (request: AnalysisConsoleRequest) => void) =>
            new Promise<void>((resolve, reject) => {
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

                                            const prevonerror = runner.current!.onerror;
                                            runner.current!.onerror = (event) => {
                                                reject(event.error);
                                                runner.current!.onerror = prevonerror;
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

                const prevonerror = runner.current!.onerror;
                runner.current!.onerror = (event) => {
                    reject(event.error);
                    runner.current!.onerror = prevonerror;
                };

                runner.current!.postMessage({ operation: "run", context });
            }),
        hover: (start: number, end: number) =>
            new Promise<HoverOutput | null>((resolve, reject) => {
                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                    runner.current!.onmessage = prevonmessage;
                };

                const prevonerror = runner.current!.onerror;
                runner.current!.onerror = (event) => {
                    reject(event.error);
                    runner.current!.onerror = prevonerror;
                };

                runner.current!.postMessage({ operation: "hover", start, end, context });
            }),
        completions: (position: number) =>
            new Promise<AnalysisOutputCompletions>((resolve, reject) => {
                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                    runner.current!.onmessage = prevonmessage;
                };

                const prevonerror = runner.current!.onerror;
                runner.current!.onerror = (event) => {
                    reject(event.error);
                    runner.current!.onerror = prevonerror;
                };

                runner.current!.postMessage({ operation: "completions", position, context });
            }),
        format: (code: string) =>
            new Promise<string | undefined>((resolve, reject) => {
                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                    runner.current!.onmessage = prevonmessage;
                };

                const prevonerror = runner.current!.onerror;
                runner.current!.onerror = (event) => {
                    reject(event.error);
                    runner.current!.onerror = prevonerror;
                };

                runner.current!.postMessage({ operation: "format", code, context });
            }),
    };
};
