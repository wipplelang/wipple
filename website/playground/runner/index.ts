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
        run: async (input: (prompt: string) => Promise<string>, output: (string: string) => void) =>
            new Promise<boolean>(async (resolve, reject) => {
                runner.current!.onmessage = async (event) => {
                    switch (event.data.type) {
                        case "input":
                            const text = await input(event.data.prompt);
                            runner.current!.postMessage({ operation: "input", text });
                            break;
                        case "output":
                            output(event.data.text);
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
