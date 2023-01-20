import { Semaphore, useAsyncEffect } from "../helpers";

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

let runner: Worker | undefined;
const semaphore = new Semaphore(1);

export const useRunner = () => {
    useAsyncEffect(async () => {
        if (runner) return;

        await semaphore.acquire();
        runner = new Worker(new URL("../runner/worker.js", import.meta.url));
        semaphore.release();
    }, []);

    return {
        analyze: async (id: string, code: string, lint: boolean) => {
            await semaphore.acquire();

            try {
                const analysis: AnalysisOutput = await new Promise(async (resolve, reject) => {
                    runner!.onmessage = (event) => {
                        resolve(event.data);
                    };

                    runner!.onerror = (event) => {
                        reject(event.error);
                    };

                    runner!.postMessage(
                        JSON.stringify({
                            operation: "analyze",
                            id,
                            code,
                            lint,
                        })
                    );
                });

                return analysis;
            } finally {
                semaphore.release();
            }
        },
        run: async (id: string) => {
            await semaphore.acquire();

            try {
                const output: string = await new Promise(async (resolve, reject) => {
                    runner!.onmessage = (event) => {
                        resolve(event.data);
                    };

                    runner!.onerror = (event) => {
                        reject(event.error);
                    };

                    runner!.postMessage(
                        JSON.stringify({
                            operation: "run",
                            id,
                        })
                    );
                });

                return output;
            } finally {
                semaphore.release();
            }
        },
        hover: async (id: string, start: number, end: number) => {
            await semaphore.acquire();

            try {
                const hover: HoverOutput = await new Promise(async (resolve, reject) => {
                    runner!.onmessage = (event) => {
                        resolve(event.data);
                    };

                    runner!.onerror = (event) => {
                        reject(event.error);
                    };

                    runner!.postMessage(
                        JSON.stringify({
                            operation: "hover",
                            id,
                            start,
                            end,
                        })
                    );
                });

                return hover;
            } finally {
                semaphore.release();
            }
        },
        remove: async (id: string) => {
            await semaphore.acquire();

            try {
                await new Promise<void>(async (resolve, reject) => {
                    runner!.onmessage = (event) => {
                        resolve(event.data);
                    };

                    runner!.onerror = (event) => {
                        reject(event.error);
                    };

                    runner!.postMessage(
                        JSON.stringify({
                            operation: "delete",
                            id,
                        })
                    );
                });
            } finally {
                semaphore.release();
            }
        },
    };
};
