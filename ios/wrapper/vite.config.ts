import path from "path";
import { defineConfig, loadEnv } from "vite";
import react from "@vitejs/plugin-react-swc";
import svgr from "vite-plugin-svgr";
import topLevelAwait from "vite-plugin-top-level-await";

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => {
    process.env = { ...process.env, ...loadEnv(mode, process.cwd()) };

    return {
        base: "/playground",
        plugins: [react(), svgr(), topLevelAwait()],
        build: {
            sourcemap: true,
            rollupOptions: {
                input: [
                    path.join(__dirname, "index.html"),
                    path.join(__dirname, "src/main.tsx"),
                    path.join(__dirname, "../../wasm/worker_entrypoint.js"),
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
    };
});
