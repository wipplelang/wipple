let cancel;
const consoleResponders = [];

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
                const analysis = await runner.analyze(code, lint);
                postMessage(analysis);
                break;
            case "run":
                cancel = runner.run(
                    (request) => {
                        switch (request.kind) {
                            case "display":
                                postMessage({
                                    type: "display",
                                    text: request.text,
                                });

                                consoleResponders.push({
                                    callback: request.callback,
                                });

                                break;
                            case "prompt":
                                postMessage({
                                    type: "prompt",
                                    prompt: request.prompt,
                                });

                                consoleResponders.push({
                                    sendInput: request.send_input,
                                    recvValid: request.recv_valid,
                                    callback: request.callback,
                                });

                                break;
                            case "choice":
                                postMessage({
                                    type: "choice",
                                    prompt: request.prompt,
                                    choices: request.choices,
                                });

                                consoleResponders.push({
                                    callback: request.callback,
                                });

                                break;
                            default:
                                throw new Error("unhandled console request");
                        }
                    },
                    (success) => {
                        if (cancel) {
                            cancel();
                            cancel = undefined;
                        }

                        postMessage({ type: "done", success });
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
                await consoleResponders.pop().callback();
                break;
            case "sendPromptInput":
                await consoleResponders[consoleResponders.length - 1].sendInput(event.data.input);
                break;
            case "recvPromptValid":
                const valid = await consoleResponders[consoleResponders.length - 1].recvValid();
                postMessage(valid);
                break;
            case "promptCallback":
                await consoleResponders.pop().callback();
                break;
            case "choiceCallback":
                await consoleResponders.pop().callback(event.data.index);
                break;
            default:
                throw new Error("invalid operation");
        }
    } catch (error) {
        setTimeout(() => {
            throw error;
        });
    }
};
