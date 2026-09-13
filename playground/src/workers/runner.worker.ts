import { makeAtomicsChannel, readMessage, writeMessage, type Channel } from "sync-message";
import initWipple, * as wipple from "wipple";
import * as Sentry from "@sentry/browser";

await initWipple({ module_or_path: fetch(wipple.modulePath) });

export type Env = Record<string, (input: any) => Promise<any>>;

// Message IDs are only used by service workers
const globalMessageId = "";

export const init = (worker: Worker, env: Env) => {
    const channel = makeAtomicsChannel();

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
        run: async (program: ArrayBufferLike) => {
            worker.postMessage({ type: "run", channel, program }, [program]);
            await done;
        },
    };
};

const run = async (channel: Channel, program: ArrayBufferLike) => {
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
        wipple.run(new Uint8Array(program), env);
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
                run(e.data.channel, e.data.program);
                break;
            }
            default:
                throw new Error(`unsupported message: ${JSON.stringify(e.data)}`);
        }
    };
}
