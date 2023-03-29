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
          callback: (id: string) => void;
      }
    | {
          type: "messageUi";
          id: string;
          message: string;
          value: any;
          callback: (result: any) => void;
      };

export const useRunner = () => {
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

                runner.current!.postMessage({ operation: "analyze", code, lint });
            }),
        run: (handleConsole: (request: AnalysisConsoleRequest) => void) =>
            new Promise<boolean>((resolve, reject) => {
                const prevonmessage = runner.current!.onmessage;
                runner.current!.onmessage = async (event) => {
                    switch (event.data.type) {
                        case "display":
                            handleConsole({
                                type: "display",
                                text: event.data.text,
                                callback: () => {
                                    runner.current!.postMessage({ operation: "displayCallback" });
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
                                    });
                                },
                                recvValid: () =>
                                    new Promise((resolve) => {
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
                                        });
                                    }),
                                callback: () => {
                                    runner.current!.postMessage({ operation: "promptCallback" });
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
                                    });
                                },
                            });

                            break;
                        case "loadUi":
                            handleConsole({
                                type: "loadUi",
                                url: event.data.url,
                                callback: (id) => {
                                    runner.current!.postMessage({
                                        operation: "loadUiCallback",
                                        id,
                                    });
                                },
                            });

                            break;
                        case "messageUi":
                            handleConsole({
                                type: "messageUi",
                                id: event.data.id,
                                message: event.data.message,
                                value: event.data.value,
                                callback: (result) => {
                                    runner.current!.postMessage({
                                        operation: "messageUiCallback",
                                        result,
                                    });
                                },
                            });

                            break;
                        case "done":
                            resolve(event.data.success);
                            runner.current!.onmessage = prevonmessage;
                            break;
                    }
                };

                const prevonerror = runner.current!.onerror;
                runner.current!.onerror = (event) => {
                    reject(event.error);
                    runner.current!.onerror = prevonerror;
                };

                runner.current!.postMessage({ operation: "run" });
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

                runner.current!.postMessage({ operation: "hover", start, end });
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

                runner.current!.postMessage({ operation: "completions", position });
            }),
    };
};
