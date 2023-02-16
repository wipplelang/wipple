let cancel;
let input;

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
                    (prompt) =>
                        new Promise((resolve) => {
                            input = resolve;
                            postMessage({ type: "input", prompt });
                        }),
                    async (text) => {
                        postMessage({ type: "output", text });
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
            case "input":
                const { text } = event.data;

                if (input) {
                    input(text);
                    input = undefined;
                }

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
