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
            debug: !process.env.WIPPLE_COMPILER_WASM_OPTIMIZE,
            verbose: true,
            inlineWasm: true,
            cargoArgs: process.env.WIPPLE_COMPILER_WASM_OPTIMIZE
                ? []
                : ["--config", "profile.dev.debug='line-tables-only'"],
            wasmBindgenArgs: process.env.WIPPLE_COMPILER_WASM_OPTIMIZE ? [] : ["--keep-debug"],
            wasmOptArgs: process.env.WIPPLE_COMPILER_WASM_OPTIMIZE ? ["-O"] : [],
            experimental: {
                synchronous: true,
            },
        }),
    ],
});
