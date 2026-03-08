import { makeChannel, readMessage, writeMessage, type Channel } from "sync-message";

export type Env = Record<string, (input: any) => Promise<any>>;

// Message IDs are only used by service workers
const globalMessageId = "";

export const init = (worker: Worker, env: Env) => {
    const channel = makeChannel()!;

    const done = new Promise<void>((resolve) => {
        worker.onmessage = async (e) => {
            switch (e.data.type) {
                case "call": {
                    const { f, input } = e.data;
                    const output = await env[f](input);
                    writeMessage(channel, { output }, globalMessageId);
                    break;
                }
                case "done": {
                    resolve();
                    break;
                }
            }
        };
    });

    return {
        run: async (executable: string) => {
            worker.postMessage({ type: "run", channel, executable });
            await done;
        },
    };
};

const run = async (channel: Channel, executable: string) => {
    const module = await import(
        /* @vite-ignore */ `data:text/javascript,${encodeURIComponent(executable)}`
    );

    const env = new Proxy(
        {},
        {
            get: (_, f) => (input: any) => {
                postMessage({ type: "call", f, input });
                const { output } = readMessage(channel, globalMessageId);
                return output;
            },
        },
    );

    try {
        await module.default(env);
    } catch (e) {
        console.error(e);
    }

    postMessage({ type: "done" });
};

if (typeof WorkerGlobalScope !== "undefined" && self instanceof WorkerGlobalScope) {
    onmessage = (e) => {
        switch (e.data.type) {
            case "run": {
                run(e.data.channel, e.data.executable);
                break;
            }
            default:
                throw new Error(`unsupported message: ${JSON.stringify(e.data)}`);
        }
    };
}
