import { callFunction, evaluate, InterpreterError, IoRequest } from "wipple-interpreter";

const withCallFunction =
    (handler: (event: MessageEvent<any>) => void) => async (event: MessageEvent<any>) => {
        switch (event.data.type) {
            case "callFunction": {
                const { func, inputs } = event.data;

                const output = await callFunction(func, inputs);

                postMessage({
                    type: "completion",
                    output,
                });

                break;
            }
            default: {
                handler(event);
                break;
            }
        }
    };

onmessage = async (event) => {
    try {
        const { type } = event.data;
        switch (type) {
            case "run": {
                try {
                    const prevonmessage = onmessage;
                    onmessage = withCallFunction(async (event) => {
                        throw new Error(`unsupported message: ${event.data.type}`);
                    });

                    await evaluate(event.data.executable, {
                        debug: false,
                        gc: () => {
                            // The browser GCs automatically
                        },
                        io: async (request: IoRequest) => {
                            switch (request.type) {
                                case "display": {
                                    const prevonmessage = onmessage;
                                    onmessage = async (event) => {
                                        const { type } = event.data;
                                        if (type !== "completion") {
                                            throw new Error("expected 'completion' event");
                                        }

                                        onmessage = prevonmessage;
                                        request.completion();
                                    };

                                    postMessage({ type: "display", text: request.message });

                                    break;
                                }
                                case "prompt": {
                                    const prevonmessage = onmessage;
                                    onmessage = withCallFunction(async (event) => {
                                        const { type, input } = event.data;
                                        if (type !== "validate") {
                                            throw new Error("expected 'validate' event");
                                        }

                                        const valid = await request.validate(input);

                                        if (valid) {
                                            onmessage = prevonmessage;
                                        }

                                        postMessage({ type: "validate", valid });
                                    });

                                    postMessage({ type: "prompt", prompt: request.message });

                                    break;
                                }
                                case "choice": {
                                    const prevonmessage = onmessage;
                                    onmessage = async (event) => {
                                        const { type, index } = event.data;
                                        if (type !== "completion") {
                                            throw new Error("expected 'completion' event");
                                        }

                                        onmessage = prevonmessage;
                                        request.completion(index);
                                    };

                                    postMessage({
                                        type: "choice",
                                        prompt: request.message,
                                        choices: request.choices,
                                    });

                                    break;
                                }
                                case "ui": {
                                    const prevonmessage = onmessage;
                                    onmessage = withCallFunction(async (event) => {
                                        const { type, value } = event.data;
                                        if (type !== "response") {
                                            throw new Error("expected 'response' event");
                                        }

                                        onmessage = prevonmessage;
                                        request.completion(value);
                                    });

                                    postMessage({
                                        type: "ui",
                                        message: request.message,
                                        value: request.value,
                                    });

                                    break;
                                }
                                case "sleep": {
                                    setTimeout(request.completion, request.ms);
                                    break;
                                }
                                default: {
                                    throw new Error("unsupported IO request");
                                }
                            }
                        },
                    });

                    onmessage = prevonmessage;
                    postMessage({ type: "completion" });
                } catch (error) {
                    if (error instanceof InterpreterError) {
                        postMessage({ type: "error", message: error.message });
                    } else {
                        throw error;
                    }
                }

                break;
            }
            default:
                throw new Error(`unsupported message: ${type}`);
        }
    } catch (error) {
        postMessage({ type: "runnerError", error });
    }
};
