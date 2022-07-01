import { useEffect } from "react";

export interface Output {
    success: boolean;
    value: string;
}

let runner: Worker | undefined;
let active: Promise<Output> | undefined;

export const useRunner = () => {
    useEffect(() => {
        if (runner) return;
        runner = new Worker(new URL("../runner/worker.js", import.meta.url));
    }, []);

    return {
        run: (code: string): Promise<Output> => {
            active = new Promise(async (resolve, reject) => {
                if (active) {
                    await active;
                }

                runner!.onmessage = (event) => {
                    resolve(event.data);
                    active = undefined;
                };

                runner!.onerror = (event) => {
                    reject(event.error);
                    active = undefined;
                };

                runner!.postMessage(code);
            });

            return active;
        },
    };
};
