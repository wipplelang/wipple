import * as esbuild from "esbuild";
import * as fs from "node:fs";
import metaUrl from "@chialab/esbuild-plugin-meta-url";

const options = {
    bundle: true,
    sourcemap: true,
    loader: { ".wasm": "file" },
    plugins: [metaUrl()],
};

fs.rmSync("dist", { recursive: true, force: true });

esbuild.build({
    entryPoints: ["./index.browser.ts"],
    platform: "browser",
    outdir: "dist/browser",
    format: "esm",
    ...options,
});

esbuild.build({
    entryPoints: ["./index.node.ts"],
    platform: "node",
    outdir: "dist/node",
    format: "cjs",
    ...options,
});
