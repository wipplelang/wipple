import { defineConfig } from "rollup";
import typescript from "@rollup/plugin-typescript";
import json from "@rollup/plugin-json";
import nodeResolve from "@rollup/plugin-node-resolve";
import commonjs from "@rollup/plugin-commonjs";

export default defineConfig({
    input: ["src/extension.ts"],
    output: {
        dir: "dist",
        format: "cjs",
        sourcemap: true,
    },
    plugins: [typescript(), json(), commonjs(), nodeResolve()],
});
