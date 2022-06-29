onmessage = async (event) => {
    const runner = await import("./pkg");
    postMessage(runner.run(event.data, (url) => `show "Hello from ${url}"`));
};
