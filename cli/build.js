import * as esbuild from "esbuild";

await esbuild.build({
    entryPoints: ["src/index.ts"],
    bundle: true,
    platform: "node",
    target: ["node20"],
    external: ["bun"],
    format: "esm",
    banner: {
        // https://github.com/evanw/esbuild/issues/1921#issuecomment-1152991694
        js: 'import { createRequire } from "module"; const require = createRequire(import.meta.url);',
    },
    sourcemap: true,
    outfile: "dist/index.js",
});
