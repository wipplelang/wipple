import { v4 as uuid } from "uuid";

let functions = [];
let cancel;
const consoleResponders = {};
let resolveFunctionResult;

Error.stackTraceLimit = 10000;

onmessage = async (event) => {
    try {
        const runner = await import("./pkg");

        switch (event.data.operation) {
            case "analyze":
                if (cancel) {
                    cancel();
                    cancel = undefined;
                }

                const { code, lint } = event.data;

                const analysis = await new Promise((resolve, reject) => {
                    runner.analyze(code, lint, (success, result) => {
                        if (success) {
                            resolve(result);
                        } else {
                            reject(result);
                        }
                    });
                });

                postMessage(analysis);

                break;
            case "run":
                functions = [];

                cancel = runner.run(
                    (request) => {
                        const consoleResponderId = uuid();

                        switch (request.kind) {
                            case "display":
                                postMessage({
                                    type: "display",
                                    text: request.text,
                                    id: consoleResponderId,
                                });

                                consoleResponders[consoleResponderId] = {
                                    callback: request.callback,
                                };

                                break;
                            case "prompt":
                                postMessage({
                                    type: "prompt",
                                    prompt: request.prompt,
                                    id: consoleResponderId,
                                });

                                consoleResponders[consoleResponderId] = {
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
                                    id: consoleResponderId,
                                });

                                consoleResponders[consoleResponderId] = {
                                    callback: request.callback,
                                };

                                break;
                            case "loadUi":
                                postMessage({
                                    type: "loadUi",
                                    url: request.url,
                                    id: consoleResponderId,
                                });

                                consoleResponders[consoleResponderId] = {
                                    callback: request.callback,
                                };

                                break;
                            case "finishUi":
                                consoleResponders[request.id] = undefined;

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
            case "hover":
                const { start, end } = event.data;
                const hover = runner.hover(start, end);
                postMessage(hover);
                break;
            case "completions":
                const { position } = event.data;
                const completions = runner.completions(position);
                postMessage(completions);
                break;
            case "displayCallback":
                await consoleResponders[event.data.id].callback();
                consoleResponders[event.data.id] = undefined;
                break;
            case "sendPromptInput":
                await consoleResponders[event.data.id].sendInput(event.data.input);
                break;
            case "recvPromptValid":
                const valid = await consoleResponders[event.data.id].recvValid();
                postMessage(valid);
                break;
            case "promptCallback":
                await consoleResponders[event.data.id].callback();
                consoleResponders[event.data.id] = undefined;
                break;
            case "choiceCallback":
                await consoleResponders[event.data.id].callback(event.data.index);
                consoleResponders[event.data.id] = undefined;
                break;
            case "loadUiCallback":
                const consoleResponderId = uuid();

                await consoleResponders[event.data.id].callback(
                    consoleResponderId,
                    (message, value, callback) => {
                        const consoleResponderId = uuid();

                        postMessage({
                            type: "messageUi",
                            message,
                            value: encodeFunction(value),
                            id: consoleResponderId,
                        });

                        consoleResponders[consoleResponderId] = { callback };
                    }
                );

                consoleResponders[event.data.id] = undefined;

                break;
            case "messageUiCallback":
                await consoleResponders[event.data.id].callback(decodeFunction(event.data.value));

                break;
            case "callFunction":
                const result = await functions[event.data.id](decodeFunction(event.data.input));

                requestAnimationFrame(() => {
                    postMessage({
                        type: "callFunctionResult",
                        result: encodeFunction(result),
                    });
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

        throw error;
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
