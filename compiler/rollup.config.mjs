import { defineConfig } from "rollup";
import rust from "@wasm-tool/rollup-plugin-rust";

export default defineConfig({
    input: "index.js",
    output: {
        dir: "dist",
        sourcemap: true,
    },
    plugins: [
        rust({
            verbose: true,
            inlineWasm: true,
            debug: process.env.WIPPLE_COMPILER_WASM_OPTIMIZE ? false : true,
            wasmOptArgs: process.env.WIPPLE_COMPILER_WASM_OPTIMIZE ? ["-O"] : [],
            experimental: {
                synchronous: true,
            },
        }),
    ],
});
