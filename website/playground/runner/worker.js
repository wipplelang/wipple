onmessage = async (event) => {
    const runner = await import("./pkg");
    const { code, lint } = JSON.parse(event.data);
    postMessage(await runner.run(code, lint));
};
