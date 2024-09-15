import path from "path";
import { defineConfig, loadEnv } from "vite";
import react from "@vitejs/plugin-react-swc";
import svgr from "vite-plugin-svgr";
import topLevelAwait from "vite-plugin-top-level-await";
import { sentryVitePlugin } from "@sentry/vite-plugin";
import { lezer } from "@lezer/generator/rollup";
import * as markdown from "vite-plugin-markdown";

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => {
    process.env = { ...process.env, ...loadEnv(mode, process.cwd()) };

    return {
        base: "/playground",
        plugins: [
            react(),
            svgr(),
            lezer(),
            markdown.plugin({ mode: [markdown.Mode.MARKDOWN] }),
            topLevelAwait(),
            sentryVitePlugin({
                disable: !process.env.CI,
                org: process.env.VITE_SENTRY_ORG,
                project: process.env.VITE_SENTRY_PROJECT,
                authToken: process.env.VITE_SENTRY_AUTH_TOKEN,
            }),
        ],
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
        server: {
            port: 8080,
            fs: {
                allow: ["../.."],
            },
            headers: {
                "Cross-Origin-Opener-Policy": "same-origin",
                "Cross-Origin-Embedder-Policy": "require-corp",
            },
        },
        define: {
            __DEV__: mode === "development",
        },
    };
});
