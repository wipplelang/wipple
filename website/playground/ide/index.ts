import { useEffect, useRef, useState } from "react";
import Runner from "./worker?worker";
import * as glue from "../../../tools/playground-runner/clientGlue";
import type { AnalysisOutputDiagnostic } from "../common";

export interface AnalysisOutput {
    diagnostics: AnalysisOutputDiagnostic[];
    syntaxHighlighting: AnalysisOutputSyntaxHighlightingItem[];
    completions: AnalysisOutputCompletions;
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
        compile: () =>
            new Promise<any>(async (resolve, reject) => {
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

                runner.current!.postMessage({ operation: "compile", context });
            }),
        run: (handleConsole: (request: glue.ConsoleRequest) => void) =>
            glue.run({
                loadRunner: async () => {
                    await runnerPromise.current;
                    return runner.current!;
                },
                runArgs: { context },
                handleConsole,
                reset,
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
