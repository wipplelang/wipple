let runner;
import("./pkg").then((r) => (runner = r));

onmessage = (event) => {
    if (!runner) return;

    switch (event.data.type) {
        case "run":
            postMessage(runner.run(event.data.code));
            break;
        case "completions":
            postMessage(runner.get_completions(event.data.position));
    }
};
