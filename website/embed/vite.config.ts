import { defineConfig, loadEnv } from "vite";
import react from "@vitejs/plugin-react-swc";
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";
import path from "path";
import pkg from "./package.json";

export default defineConfig(({ command, mode }) => {
    const env = loadEnv(mode, process.cwd(), "");

    return {
        build: {
            lib: {
                entry: path.resolve(__dirname, "src/index.tsx"),
                name: pkg.name,
                fileName: "index",
            },
        },
        plugins: [react(), wasm(), topLevelAwait()],
        worker: {
            plugins: [wasm(), topLevelAwait()],
            rollupOptions: {
                output: {
                    inlineDynamicImports: true,
                },
            },
        },
        server: {
            port: 3000,
        },
        base:
            mode === "production"
                ? process.env.CI
                    ? "https://wipple.dev/embed/"
                    : "http://localhost:8080/embed/"
                : undefined,
        define: {
            "process.env.NODE_ENV": '"production"',
        },
    };
});
