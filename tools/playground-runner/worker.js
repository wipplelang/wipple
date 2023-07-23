import * as glue from "./workerGlue";

Error.stackTraceLimit = 10000;

onmessage = async (event) => {
    try {
        const runner = await import("./pkg");
        await glue.handle(runner, event);
    } catch (error) {
        console.error("[runner] error:", error);

        reportError(error);
    }
};
