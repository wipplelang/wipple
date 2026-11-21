import path from "node:path";
import tailwindcss from "@tailwindcss/vite";
import { svelte } from "@sveltejs/vite-plugin-svelte";
import { sentryVitePlugin } from "@sentry/vite-plugin";
import {
    defineConfig,
    loadEnv,
    type Plugin,
    type PreviewServer,
    type ViteDevServer,
    type Rolldown,
} from "vite";

export default defineConfig(({ mode }) => {
    const env = loadEnv(mode, process.cwd(), "");

    return {
        base: "./",
        plugins: [
            tailwindcss(),
            svelte(),
            crossOriginIsolation(),
            sentryVitePlugin({
                org: env.SENTRY_ORG,
                project: env.SENTRY_PROJECT,
                authToken: env.SENTRY_AUTH_TOKEN,
            }),
        ],
        resolve: {
            alias: {
                "@": path.resolve("./src"),
            },
        },
        worker: {
            format: "es",
            rolldownOptions: {
                output: { assetFileNames },
            },
        },
        build: {
            sourcemap: true,
            rolldownOptions: {
                output: { assetFileNames },
            },
        },
        server: {
            port: 8080,
            fs: {
                allow: [".."],
            },
        },
    };
});

const configureServer = (server: ViteDevServer | PreviewServer) => {
    server.middlewares.use((_req, res, next) => {
        res.setHeader("Cross-Origin-Embedder-Policy", "require-corp");
        res.setHeader("Cross-Origin-Opener-Policy", "same-origin");
        next();
    });
};

const crossOriginIsolation = (): Plugin => ({
    name: "cross-origin-isolation",
    configureServer,
    configurePreviewServer: configureServer,
});

const assetFileNames = (chunkInfo: Rolldown.PreRenderedAsset) => {
    const fileName = chunkInfo.names[0] ?? "";
    return fileName.endsWith(".bin") ? "assets/[name][extname]" : "assets/[name]-[hash][extname]";
};
