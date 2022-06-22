onmessage = async (event) => {
    const runner = await import("./pkg");
    postMessage(runner.run(event.data));
};
