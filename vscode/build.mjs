import * as esbuild from "esbuild";
import * as fs from "node:fs";
import metaUrl from "@chialab/esbuild-plugin-meta-url";

const outdir = "./dist";

fs.rmSync(outdir, { recursive: true, force: true });

esbuild.build({
    entryPoints: ["src/extension.ts"],
    outdir,
    platform: "node",
    bundle: true,
    sourcemap: true,
    format: "cjs",
    loader: { ".wasm": "file" },
    external: ["vscode"],
    plugins: [metaUrl()],
});
