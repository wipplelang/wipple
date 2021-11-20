let runner;
import("./pkg").then((r) => (runner = r));

onmessage = (event) => {
    if (!runner) return;

    const result = runner.run(event.data);
    postMessage(result);
};
