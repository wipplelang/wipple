onmessage = async (event) => {
    try {
        const runner = await import("./pkg");

        const data = JSON.parse(event.data);

        const { id } = data;

        switch (data.operation) {
            case "analyze":
                const { code, lint } = data;
                const analysis = await runner.analyze(id, code, lint);
                postMessage(analysis);
                break;
            case "run":
                const output = runner.run(id);
                postMessage(output);
                break;
            case "hover":
                const { start, end } = data;
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
