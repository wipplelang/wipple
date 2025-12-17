import * as Comlink from "comlink";

export type RunnerEnv = Record<string, (...args: any[]) => Promise<any>>;

const worker = {
    async run(executable: string, env: RunnerEnv) {
        const { default: entrypoint, buildRuntime } = await import(
            /* @vite-ignore */ `data:text/javascript,${encodeURIComponent(executable)}`
        );

        const proxy = (value: any) => {
            if (typeof value === "function") {
                return Comlink.proxy(value);
            } else {
                return value; // Note: nested functions aren't supported
            }
        };

        await entrypoint(buildRuntime(env, proxy));
    },
};

export type RunnerWorkerType = typeof worker;

Comlink.expose(worker);
