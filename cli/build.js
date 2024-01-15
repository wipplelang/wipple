import * as esbuild from "esbuild";

await esbuild.build({
    entryPoints: ["src/index.ts"],
    bundle: true,
    platform: "node",
    target: ["node20"],
    format: "esm",
    sourcemap: true,
    outfile: "dist/index.js",
});
