import { defineConfig } from "rollup";
import rust from "@wasm-tool/rollup-plugin-rust";

console.error(
    "process.env.WIPPLE_COMPILER_WASM_OPTIMIZE",
    process.env.WIPPLE_COMPILER_WASM_OPTIMIZE,
);

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
            wasmOptArgs: process.env.WIPPLE_COMPILER_WASM_OPTIMIZE ? ["-O"] : [],
            experimental: {
                synchronous: true,
            },
        }),
    ],
});
