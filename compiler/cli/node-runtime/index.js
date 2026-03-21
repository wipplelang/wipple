import { join } from "node:path";
import { readFileSync } from "node:fs";
import initRuntime from "./runtime.js";

const runtime = initRuntime({
    display: (message) => console.log(JSON.parse(message)),
});

const wasm = await WebAssembly.instantiate(
    readFileSync(join(import.meta.dirname, "./main.wasm")),
    { runtime },
    { builtins: ["js-string"], importedStringConstants: "string_constants" },
);

const { main } = wasm.instance.exports;

main(runtime);
