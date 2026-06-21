import esbuild from "esbuild";

esbuild.build({
    entryPoints: ["./src/custom.js"],
    outdir: "./dist",
    bundle: true,
});
