import { useEffect } from "react";

export interface Output {
    success: boolean;
    value: string;
}

let runner: Worker | undefined;

export const useRunner = () => {
    useEffect(() => {
        runner = new Worker(new URL("../runner/worker.js", import.meta.url));
    }, []);

    return {
        run: (code: string): Promise<Output> =>
            new Promise((resolve, reject) => {
                runner!.onmessage = (event) => resolve(event.data);
                runner!.onerror = (event) => reject(event.error);
                runner!.postMessage(code);
            }),
    };
};
