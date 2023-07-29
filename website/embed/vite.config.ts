import { defineConfig, loadEnv } from "vite";
import react from "@vitejs/plugin-react-swc";
import rust from "@wasm-tool/rollup-plugin-rust";
import { rustOptions } from "../shared/build";
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
                formats: ["es"],
            },
        },
        plugins: [react(), rust(rustOptions(env))],
        worker: {
            plugins: [rust(rustOptions(env))],
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
