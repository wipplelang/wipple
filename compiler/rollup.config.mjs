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
            experimental: {
                synchronous: true,
            },
        }),
    ],
});
