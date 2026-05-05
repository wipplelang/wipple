import * as esbuild from "esbuild";
import * as fs from "node:fs";
import inline from "esbuild-plugin-inline-import";
import metaUrl from "@chialab/esbuild-plugin-meta-url";

const outdir = "./dist";

fs.rmSync(outdir, { recursive: true, force: true });

esbuild.build({
    entryPoints: ["./src/index.ts"],
    outdir,
    platform: "node",
    bundle: true,
    sourcemap: true,
    format: "esm",
    outExtension: { ".js": ".mjs" },
    loader: { ".wasm": "file" },
    plugins: [inline(), metaUrl()],
});
