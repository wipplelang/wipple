import * as esbuild from "esbuild";
import { wasmLoader } from "esbuild-plugin-wasm";

await esbuild.build({
    entryPoints: ["index.ts"],
    bundle: true,
    platform: "node",
    target: ["node20"],
    format: "esm",
    sourcemap: true,
    outfile: "dist/index.js",
    plugins: [wasmLoader({ mode: "embedded" })],
});