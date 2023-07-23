export type ConsoleRequest =
    | {
          type: "display";
          text: string;
          callback: () => void;
      }
    | {
          type: "prompt";
          prompt: string;
          sendInput: (input: string) => void;
          recvValid: () => Promise<boolean>;
          callback: () => void;
      }
    | {
          type: "choice";
          prompt: string;
          choices: string[];
          callback: (index: number) => void;
      }
    | {
          type: "loadUi";
          url: string;
          callback: () => void;
      }
    | {
          type: "messageUi";
          message: string;
          value: string;
          callback: (value: any) => void;
      };

export const run = (options: {
    loadRunner: () => Promise<Worker>;
    runArgs: {};
    handleConsole: (request: ConsoleRequest) => void;
    reset: () => void;
}) =>
    new Promise<void>(async (resolve, reject) => {
        const runner = await options.loadRunner();

        let functions: any[] = [];
        let resolveFunctionResult: ((value: any) => void) | undefined;

        const encodeFunction = (value: any) => {
            if (typeof value === "function") {
                const length = functions.push(value);
                return { $function: length - 1 };
            } else {
                return value;
            }
        };

        const decodeFunction = (value: any) => {
            if (typeof value === "object" && value != null && "$function" in value) {
                return (input: any) =>
                    new Promise((resolve) => {
                        resolveFunctionResult = resolve;

                        runner.postMessage({
                            operation: "callFunction",
                            id: value.$function,
                            input,
                        });
                    });
            } else {
                return value;
            }
        };

        const prevonmessage = runner.onmessage;
        runner.onmessage = async (event) => {
            try {
                switch (event.data.type) {
                    case "display":
                        options.handleConsole({
                            type: "display",
                            text: event.data.text,
                            callback: () => {
                                runner.postMessage({
                                    operation: "displayCallback",
                                    id: event.data.id,
                                });
                            },
                        });

                        break;
                    case "prompt":
                        options.handleConsole({
                            type: "prompt",
                            prompt: event.data.prompt,
                            sendInput: (input) => {
                                runner.postMessage({
                                    operation: "sendPromptInput",
                                    input,
                                    id: event.data.id,
                                });
                            },
                            recvValid: () =>
                                new Promise((resolve, reject) => {
                                    const prevonmessage = runner.onmessage;
                                    runner.onmessage = (event) => {
                                        resolve(event.data);
                                        runner.onmessage = prevonmessage;
                                    };

                                    runner.onerror = (event) => {
                                        reject(event.error);
                                        options.reset();
                                    };

                                    runner.postMessage({
                                        operation: "recvPromptValid",
                                        id: event.data.id,
                                    });
                                }),
                            callback: () => {
                                runner.postMessage({
                                    operation: "promptCallback",
                                    id: event.data.id,
                                });
                            },
                        });

                        break;
                    case "choice":
                        options.handleConsole({
                            type: "choice",
                            prompt: event.data.prompt,
                            choices: event.data.choices,
                            callback: (index) => {
                                runner.postMessage({
                                    operation: "choiceCallback",
                                    index,
                                    id: event.data.id,
                                });
                            },
                        });

                        break;
                    case "loadUi":
                        options.handleConsole({
                            type: "loadUi",
                            url: event.data.url,
                            callback: () => {
                                runner.postMessage({
                                    operation: "loadUiCallback",
                                    id: event.data.id,
                                });
                            },
                        });

                        break;
                    case "messageUi":
                        options.handleConsole({
                            type: "messageUi",
                            message: event.data.message,
                            value: decodeFunction(event.data.value),
                            callback: (value) => {
                                runner.postMessage({
                                    operation: "messageUiCallback",
                                    value: encodeFunction(value),
                                    id: event.data.id,
                                });
                            },
                        });

                        break;
                    case "callFunction":
                        const result = await functions[event.data.id](
                            decodeFunction(event.data.input)
                        );

                        requestAnimationFrame(() => {
                            runner.postMessage({
                                operation: "callFunctionResult",
                                result: encodeFunction(result),
                            });
                        });

                        break;
                    case "callFunctionResult":
                        resolveFunctionResult!(event.data.result);
                        resolveFunctionResult = undefined;
                        break;
                    case "done":
                        if (event.data.error) {
                            reject(event.data.error);
                        } else {
                            resolve();
                        }

                        runner.onmessage = prevonmessage;
                        break;
                    default:
                        console.error("received invalid event:", event);
                        throw new Error("invalid operation");
                }
            } catch (error) {
                console.error("[bridge] error:", error);
            }
        };

        runner.onerror = (event) => {
            reject(event.error);
            options.reset();
        };

        runner.postMessage({
            operation: "run",
            ...options.runArgs,
        });
    });
