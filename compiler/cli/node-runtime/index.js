import { join } from "node:path";
import { readFileSync } from "node:fs";
import initRuntime from "./runtime.js";

const runtime = initRuntime({
    display: (message) => console.log(message),
});

const data = readFileSync(join(import.meta.dirname, "./main.wasm"));
const wasm = await WebAssembly.instantiate(data, { runtime });

const { main, memory } = wasm.instance.exports;

runtime.init(memory.buffer);
main(runtime);
