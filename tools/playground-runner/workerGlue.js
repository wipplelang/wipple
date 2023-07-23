import { v4 as uuid } from "uuid";

let functions = [];
let cancel;
const responders = {};
let resolveFunctionResult;

export const handle = async (runner, event) => {
    try {
        switch (event.data.operation) {
            case "checkLoading":
                postMessage({ type: "loaded" });
                break;
            case "run": {
                functions = [];

                const requestHandler = (request) => {
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
                };

                const errorHandler = (error) => {
                    if (cancel) {
                        cancel();
                        cancel = undefined;
                    }

                    postMessage({ type: "done", error });
                };

                if (event.data.program) {
                    runner.runProgram(program, requestHandler, errorHandler);
                } else {
                    cancel = runner.run(requestHandler, errorHandler);
                }

                break;
            }
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
                console.error("received invalid event:", event.data.operation);
                throw new Error("invalid operation");
        }
    } catch (error) {
        console.error("[runner] error:", error);

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
