onmessage = async (event) => {
    try {
        const runner = await import("./pkg");

        const { id } = event.data;

        switch (event.data.operation) {
            case "analyze":
                const { code, lint } = event.data;
                const analysis = await runner.analyze(id, code, lint);
                postMessage(analysis);
                break;
            case "run":
                const input = (prompt) =>
                    new Promise((resolve) => {
                        // {
                        //     const prevonmessage = onmessage;
                        //     onmessage = async (event) => {};

                        //     postMessage({ type: "input", prompt });
                        // }

                        resolve("Hello, world!");
                    });

                const output = async (text) => {
                    postMessage({ type: "output", text });
                };

                const success = await runner.run(id, input, output);
                postMessage({ type: "done", success });
                break;
            case "hover":
                const { start, end } = event.data;
                const hover = runner.hover(id, start, end);
                postMessage(hover);
                break;
            case "remove":
                runner.remove(id);
                postMessage(null);
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
