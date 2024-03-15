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
            debug: process.env.CI == null,
            wasmOptArgs: process.env.CI != null ? ["-O"] : [],
            verbose: true,
            inlineWasm: true,
            experimental: {
                synchronous: true,
            },
        }),
    ],
});
