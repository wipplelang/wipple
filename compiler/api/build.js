import * as esbuild from "esbuild";
import * as fs from "node:fs";
import metaUrl from "@chialab/esbuild-plugin-meta-url";

const outdir = "./dist";

fs.rmSync(outdir, { recursive: true, force: true });

esbuild.build({
    entryPoints: ["./index.ts"],
    outdir,
    platform: "node",
    bundle: true,
    sourcemap: true,
    format: "esm",
    loader: { ".wasm": "file" },
    plugins: [metaUrl()],
});
