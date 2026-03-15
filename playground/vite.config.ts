import tailwindcss from "@tailwindcss/vite";
import { sveltekit } from "@sveltejs/kit/vite";
import { sentryVitePlugin } from "@sentry/vite-plugin";
import { defineConfig, loadEnv, type Plugin } from "vite";

export default defineConfig(({ mode }) => {
    const env = loadEnv(mode, process.cwd(), "");

    return {
        plugins: [
            tailwindcss(),
            sveltekit(),
            crossOriginIsolation(),
            sentryVitePlugin({
                org: env.SENTRY_ORG,
                project: env.SENTRY_PROJECT,
                authToken: env.SENTRY_AUTH_TOKEN,
            }),
        ],
        worker: {
            plugins: () => [sveltekit()],
            format: "es",
        },
        build: {
            sourcemap: true,
        },
    };
});

const crossOriginIsolation = (): Plugin => ({
    name: "cross-origin-isolation",
    configureServer: (server) => {
        server.middlewares.use((_req, res, next) => {
            res.setHeader("Cross-Origin-Embedder-Policy", "require-corp");
            res.setHeader("Cross-Origin-Opener-Policy", "same-origin");
            next();
        });
    },
});
