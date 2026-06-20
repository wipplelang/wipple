import * as fs from "node:fs";
import esbuild from "esbuild";
import metaUrl from "@chialab/esbuild-plugin-meta-url";

const outdir = "./dist";

fs.rmSync(outdir, { recursive: true, force: true });

await esbuild.build({
    entryPoints: ["src/index.ts"],
    outdir,
    platform: "node",
    bundle: true,
    sourcemap: true,
    format: "cjs",
    loader: { ".wasm": "file" },
    plugins: [metaUrl()],
});
