import { PUBLIC_SERVER_URL } from "$env/static/public";
import { makeChannel, readMessage, writeMessage, type Channel } from "sync-message";
import "core-js/proposals/array-buffer-base64";

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

let runtimeSource: string | undefined;

const run = async (channel: Channel, executable: string) => {
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

    if (runtimeSource == null) {
        runtimeSource = (
            await fetch(`${PUBLIC_SERVER_URL}/runtime`, { method: "POST", body: "{}" }).then(
                (res) => res.json(),
            )
        ).runtime;
    }

    const { default: initRuntime } = await import(
        /* @vite-ignore */ `data:text/javascript;base64,${btoa(runtimeSource!)}`
    );

    const runtime = initRuntime(env);

    // @ts-expect-error polyfilled via core-js above
    const data = Uint8Array.fromBase64(executable);

    const wasm = await WebAssembly.instantiate(data, { runtime });

    const { main, memory } = wasm.instance.exports as any;

    try {
        runtime.init(memory.buffer);
        main(runtime);
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
