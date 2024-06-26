import path from "path";
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import { lezer } from "@lezer/generator/rollup";
import svgr from "vite-plugin-svgr";
import topLevelAwait from "vite-plugin-top-level-await";

// https://vitejs.dev/config/
export default defineConfig((env) => ({
    base: "/playground",
    plugins: [react(), lezer(), svgr(), topLevelAwait()],
    build: {
        rollupOptions: {
            input: [
                path.join(__dirname, "index.html"),
                path.join(__dirname, "src", "main.tsx"),
                path.join(__dirname, "wasm", "worker_entrypoint.js"),
            ],

            preserveEntrySignatures: "allow-extension",

            // Don't add hashes to the file extensions
            // (needed for wasm/worker_entrypoint.js)
            output: {
                entryFileNames: `assets/[name].js`,
                chunkFileNames: `assets/[name].js`,
                assetFileNames: `assets/[name].[ext]`,
            },
        },
    },
    server: {
        port: 8080,
        headers: {
            "Cross-Origin-Opener-Policy": "same-origin",
            "Cross-Origin-Embedder-Policy": "require-corp",
        },
    },
    define: {
        __DEV__: env.mode === "development",
    },
}));
