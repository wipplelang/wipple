import * as Comlink from "comlink";

export type RunnerEnvInput = Record<string, (input: any) => Promise<any>>;

export const runnerEnv = (env: RunnerEnvInput) =>
    Object.fromEntries(
        Object.entries(env).map(([name, f]) => [name, async (input: any) => await f(input)]),
    );

const worker = {
    async run(executable: string, env: RunnerEnvInput) {
        const module = await import(
            /* @vite-ignore */ `data:text/javascript,${encodeURIComponent(executable)}`
        );

        try {
            await module.default(env);
        } catch (e) {
            console.error(e);
        }
    },
};

export type RunnerWorkerType = typeof worker;

Comlink.expose(worker);
