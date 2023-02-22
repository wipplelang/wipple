import { useEffect, useRef } from "react";

export interface AnalysisOutput {
    diagnostics: AnalysisOutputDiagnostics;
    syntaxHighlighting: AnalysisOutputSyntaxHighlightingItem[];
}

export type AnalysisOutputDiagnostics =
    | { type: "success" }
    | { type: "warning"; diagnostics: string }
    | { type: "error"; diagnostics: string };

export interface AnalysisOutputSyntaxHighlightingItem {
    start: number;
    end: number;
    kind: string;
}

export interface HoverOutput {
    code: string;
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
      };

export const useRunner = () => {
    const runner = useRef<Worker | null>(null);

    useEffect(() => {
        const setup = async () => {
            runner.current = new Worker(new URL("../runner/worker.js", import.meta.url));
        };

        setup();

        return () => runner.current!.terminate();
    }, []);

    return {
        analyze: async (code: string, lint: boolean) => {
            const analysis: AnalysisOutput = await new Promise(async (resolve, reject) => {
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                };

                runner.current!.onerror = (event) => {
                    reject(event.error);
                };

                runner.current!.postMessage({ operation: "analyze", code, lint });
            });

            return analysis;
        },
        run: async (handleConsole: (request: AnalysisConsoleRequest) => void) =>
            new Promise<boolean>(async (resolve, reject) => {
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
                        case "done":
                            resolve(event.data.success);
                            break;
                    }
                };

                runner.current!.onerror = (event) => {
                    reject(event.error);
                };

                runner.current!.postMessage({ operation: "run" });
            }),
        hover: async (start: number, end: number) =>
            new Promise<HoverOutput>(async (resolve, reject) => {
                runner.current!.onmessage = (event) => {
                    resolve(event.data);
                };

                runner.current!.onerror = (event) => {
                    reject(event.error);
                };

                runner.current!.postMessage({ operation: "hover", start, end });
            }),
    };
};
