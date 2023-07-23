import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";

// https://vitejs.dev/config/
export default defineConfig({
    base: "./",
    plugins: [react(), wasm(), topLevelAwait()],
    resolve: {
        preserveSymlinks: true,
    },
    worker: {
        format: "es",
        plugins: [wasm(), topLevelAwait()],
    },
    define: {
        "process.env.NODE_ENV": '"production"',
    },
});
