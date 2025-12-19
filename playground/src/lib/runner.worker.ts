import * as Comlink from "comlink";

export type RunnerEnv = Record<string, (...args: any[]) => Promise<any>>;

const worker = {
    async run(executable: string, env: RunnerEnv) {
        const module = await import(
            /* @vite-ignore */ `data:text/javascript,${encodeURIComponent(executable)}`
        );

        const proxy = (value: any) => {
            if (typeof value === "function") {
                return Comlink.proxy(value);
            } else {
                return value; // Note: nested functions aren't supported
            }
        };

        await module.default(env, proxy);
    },
};

export type RunnerWorkerType = typeof worker;

Comlink.expose(worker);
