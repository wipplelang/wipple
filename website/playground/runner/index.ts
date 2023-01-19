import { Semaphore, useAsyncEffect } from "../helpers";

export type AnalysisOutput =
    | { type: "success" }
    | { type: "warning"; diagnostics: string }
    | { type: "error"; diagnostics: string };

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

            semaphore.release();

            return analysis;
        },
        run: async (id: string) => {
            await semaphore.acquire();

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

            semaphore.release();

            return output;
        },
        hover: async (id: string, start: number, end: number) => {
            await semaphore.acquire();

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

            semaphore.release();

            return hover;
        },
        remove: async (id: string) => {
            await semaphore.acquire();

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

            semaphore.release();
        },
    };
};
