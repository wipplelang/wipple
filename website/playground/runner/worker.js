onmessage = async (event) => {
    const runner = await import("./pkg");
    postMessage(await runner.run(event.data));
};
