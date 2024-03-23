import { defineConfig } from "rollup";
import typescript from "@rollup/plugin-typescript";
import nodeResolve from "@rollup/plugin-node-resolve";

export default defineConfig({
    input: "src/index.ts",
    output: {
        dir: "dist",
        sourcemap: true,
    },
    plugins: [typescript(), nodeResolve()],
});
