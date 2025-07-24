import * as Comlink from "comlink";
import { Interpreter, type Runtime } from "wipple-interpreter";
import type { Executable } from "wipple-interpreter/ir";

const worker = {
    async run(executable: Executable, runtime: Runtime) {
        const interpreter = new Interpreter({
            executable,
            runtime,
            proxy: Comlink.proxy as any,
        });

        await interpreter.run();
    },
};

export type InterpreterWorkerType = typeof worker;

Comlink.expose(worker);
