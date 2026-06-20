/// <reference types="node" />

import * as fs from "node:fs";
import esbuild from "esbuild";

const outdir = "./dist";

fs.rmSync(outdir, { recursive: true, force: true });

await esbuild.build({
    entryPoints: ["extension.ts"],
    outdir,
    platform: "node",
    bundle: true,
    sourcemap: true,
    format: "cjs",
    loader: { ".wasm": "file" },
    external: ["vscode"],
});

fs.cpSync("node_modules/wipple-lsp/dist", `${outdir}/wipple-lsp`, { recursive: true });
