import * as esbuild from "esbuild";

await esbuild.build({
    entryPoints: ["src/extension.ts", "src/lsp.ts"],
    external: ["vscode"],
    bundle: true,
    platform: "node",
    target: ["es2022"],
    format: "cjs",
    sourcemap: true,
    outdir: "dist",
});
