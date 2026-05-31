import { makeChannel, readMessage, writeMessage, type Channel } from "sync-message";
import * as Sentry from "@sentry/browser";

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
        run: async (module: string) => {
            worker.postMessage({ type: "run", channel, module });
            await done;
        },
    };
};

const run = async (channel: Channel, moduleString: string) => {
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

    const blob = new Blob([moduleString], { type: "application/javascript" });
    const url = URL.createObjectURL(blob);
    const { default: main } = await import(/* @vite-ignore */ url);
    URL.revokeObjectURL(url);

    try {
        main(env);
    } catch (e) {
        console.error(e);
        Sentry.captureException(e);
    }

    postMessage({ type: "done" });
};

if (typeof WorkerGlobalScope !== "undefined" && self instanceof WorkerGlobalScope) {
    onmessage = (e) => {
        switch (e.data.type) {
            case "run": {
                run(e.data.channel, e.data.module);
                break;
            }
            default:
                throw new Error(`unsupported message: ${JSON.stringify(e.data)}`);
        }
    };
}
