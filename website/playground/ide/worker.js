import { v4 as uuid } from "uuid";
import * as Sentry from "@sentry/browser";
import * as glue from "../../../tools/playground-runner/workerGlue";

if (import.meta.env.PROD) {
    Sentry.init({ dsn: import.meta.env.VITE_SENTRY_DSN });
}

let cancel;
const responders = {};

Error.stackTraceLimit = 10000;

onmessage = async (event) => {
    try {
        const runner = await import("./pkg");

        switch (event.data.operation) {
            case "checkLoading":
                postMessage({ type: "loaded" });
                break;
            case "analyze": {
                if (cancel) {
                    cancel();
                    cancel = undefined;
                }

                const { code, lint } = event.data;

                const analysis = await new Promise((resolve, reject) => {
                    runner.analyze(
                        code,
                        lint,
                        (path, name, input, api) =>
                            new Promise((resolve, reject) => {
                                const responderId = uuid();

                                postMessage({
                                    type: "plugin",
                                    path,
                                    name,
                                    input,
                                    api: {
                                        // TODO
                                    },
                                    id: responderId,
                                });

                                responders[responderId] = { resolve, reject };
                            }),
                        (success, result) => {
                            if (success) {
                                resolve(result);
                            } else {
                                reject(result);
                            }
                        }
                    );
                });

                postMessage({ type: "done", analysis });

                break;
            }
            case "compile": {
                const program = runner.compile();
                postMessage(program);
                break;
            }
            case "hover": {
                const { start, end } = event.data;
                const hover = runner.hover(start, end);
                postMessage(hover);
                break;
            }
            case "completions": {
                const { position } = event.data;
                const completions = runner.completions(position);
                postMessage(completions);
                break;
            }
            case "format": {
                const { code } = event.data;
                const formatted = runner.format(code);
                postMessage(formatted);
                break;
            }
            case "pluginSuccessCallback":
                await responders[event.data.id].resolve(event.data.output);
                responders[event.data.id] = undefined;
                break;
            case "pluginFailureCallback":
                await responders[event.data.id].reject(event.data.error);
                responders[event.data.id] = undefined;
                break;
            default:
                await glue.handle(runner, event);
                break;
        }
    } catch (error) {
        console.error("[runner] error:", error);

        Sentry.captureException(error, (ctx) => {
            ctx.setContext("runner-context", event.data);
        });

        reportError(error);
    }
};
