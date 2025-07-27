import * as Comlink from "comlink";
import runtime from "wipple-runtime";

export type RunnerEnv = Record<string, (...args: any[]) => Promise<any>>;

const worker = {
    async run(executable: string, env: RunnerEnv) {
        const { default: entrypoint } = await import(
            /* @vite-ignore */ `data:text/javascript,${encodeURIComponent(executable)}`
        );

        await entrypoint(runtime(env, Comlink.proxy));
    },
};

export type RunnerWorkerType = typeof worker;

Comlink.expose(worker);
