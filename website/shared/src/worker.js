import { v4 as uuid } from "uuid";
import * as Sentry from "@sentry/browser";
import initRunner from "../wasm/Cargo.toml";

const runner = initRunner();

if (import.meta.env.PROD) {
    Sentry.init({ dsn: import.meta.env.VITE_SENTRY_DSN });
}

let functions = [];
let cancel;
const responders = {};
let resolveFunctionResult;

Error.stackTraceLimit = 10000;

onmessage = async (event) => {
    try {
        switch (event.data.operation) {
            case "analyze": {
                if (cancel) {
                    cancel();
                    cancel = undefined;
                }

                const { code, setup, lint } = event.data;

                const analysis = await new Promise((resolve, reject) => {
                    runner.analyze(
                        code,
                        lint,
                        setup,
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
            case "run": {
                functions = [];

                cancel = runner.run(
                    (request) => {
                        const responderId = uuid();

                        switch (request.kind) {
                            case "display":
                                postMessage({
                                    type: "display",
                                    text: request.text,
                                    id: responderId,
                                });

                                responders[responderId] = {
                                    callback: request.callback,
                                };

                                break;
                            case "prompt":
                                postMessage({
                                    type: "prompt",
                                    prompt: request.prompt,
                                    id: responderId,
                                });

                                responders[responderId] = {
                                    sendInput: request.send_input,
                                    recvValid: request.recv_valid,
                                    callback: request.callback,
                                };

                                break;
                            case "choice":
                                postMessage({
                                    type: "choice",
                                    prompt: request.prompt,
                                    choices: request.choices,
                                    id: responderId,
                                });

                                responders[responderId] = {
                                    callback: request.callback,
                                };

                                break;
                            case "loadUi":
                                postMessage({
                                    type: "loadUi",
                                    url: request.url,
                                    id: responderId,
                                });

                                responders[responderId] = {
                                    callback: request.callback,
                                };

                                break;
                            case "finishUi":
                                responders[request.id] = undefined;

                                break;
                            default:
                                throw new Error("unhandled console request");
                        }
                    },
                    (error) => {
                        if (cancel) {
                            cancel();
                            cancel = undefined;
                        }

                        postMessage({ type: "done", error });
                    }
                );

                break;
            }
            case "hover": {
                const { start, end } = event.data;
                const hover = runner.hover(start, end);
                postMessage(hover);
                break;
            }
            case "snippets": {
                const { position } = event.data;
                const snippets = runner.snippets(position);
                postMessage(snippets);
                break;
            }
            case "expandSnippet": {
                const { snippet, wrappedCode } = event.data;
                const code = runner.expandSnippet(snippet, wrappedCode);
                postMessage(code);
                break;
            }
            case "format": {
                const { code } = event.data;
                const formatted = runner.format(code);
                postMessage(formatted);
                break;
            }
            case "completion": {
                const { prefix } = event.data;
                const completion = runner.completion(prefix);
                postMessage(completion);
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
            case "displayCallback":
                await responders[event.data.id].callback();
                responders[event.data.id] = undefined;
                break;
            case "sendPromptInput":
                await responders[event.data.id].sendInput(event.data.input);
                break;
            case "recvPromptValid":
                const valid = await responders[event.data.id].recvValid();
                postMessage(valid);
                break;
            case "promptCallback":
                await responders[event.data.id].callback();
                responders[event.data.id] = undefined;
                break;
            case "choiceCallback":
                await responders[event.data.id].callback(event.data.index);
                responders[event.data.id] = undefined;
                break;
            case "loadUiCallback":
                const responderId = uuid();

                await responders[event.data.id].callback(
                    responderId,
                    (message, value, callback) => {
                        const responderId = uuid();

                        postMessage({
                            type: "messageUi",
                            message,
                            value: encodeFunction(value),
                            id: responderId,
                        });

                        responders[responderId] = { callback };
                    }
                );

                responders[event.data.id] = undefined;

                break;
            case "messageUiCallback":
                await responders[event.data.id].callback(decodeFunction(event.data.value));

                break;
            case "callFunction":
                const result = await functions[event.data.id](decodeFunction(event.data.input));

                postMessage({
                    type: "callFunctionResult",
                    result: encodeFunction(result),
                });

                break;
            case "callFunctionResult":
                resolveFunctionResult(event.data.result);
                resolveFunctionResult = undefined;
                break;
            default:
                console.error("received invalid event:", event);
                throw new Error("invalid operation");
        }
    } catch (error) {
        console.error("[runner] error:", error);

        Sentry.captureException(error, (ctx) => {
            ctx.setContext("runner-context", event.data);
        });

        reportError(error);
    }
};

const encodeFunction = (value) => {
    if (typeof value === "function") {
        const length = functions.push(value);
        return { $function: length - 1 };
    } else {
        return value;
    }
};

const decodeFunction = (value) => {
    if (typeof value === "object" && value != null && "$function" in value) {
        return (input) =>
            new Promise((resolve) => {
                resolveFunctionResult = resolve;
                postMessage({ type: "callFunction", id: value.$function, input });
            });
    } else {
        return value;
    }
};
