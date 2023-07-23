import * as glue from "./workerGlue";

Error.stackTraceLimit = 10000;

onmessage = async (event) => {
    try {
        const runner = await import("./pkg");

        switch (event.data.operation) {
            case "checkLoading":
                postMessage({ type: "loaded" });
                break;
            default:
                await glue.handle(runner, event);
                break;
        }
    } catch (error) {
        console.error("[runner] error:", error);

        reportError(error);
    }
};
