import init, { compile, format, highlights, help, run, stop } from "./pkg/wipple_wasm.js";

await init();

export { compile, format, highlights, help, run, stop };
