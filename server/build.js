import * as esbuild from "esbuild";

await esbuild.build({
    entryPoints: ["src/index.ts"],
    bundle: true,
    platform: "neutral",
    target: ["es2022"],
    format: "esm",
    sourcemap: true,
    outfile: "dist/index.js",
});
