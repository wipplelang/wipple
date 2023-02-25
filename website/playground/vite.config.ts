import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";

export default defineConfig({
    plugins: [react(), wasm(), topLevelAwait()],
    worker: {
        format: "es",
        plugins: [wasm(), topLevelAwait()],
    },
    server: {
        port: 3000,
    },
    base: process.env.NODE_ENV === "development" ? undefined : "/playground",
});
