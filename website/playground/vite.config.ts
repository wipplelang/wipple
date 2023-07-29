import { defineConfig, loadEnv } from "vite";
import react from "@vitejs/plugin-react-swc";
import { sentryVitePlugin } from "@sentry/vite-plugin";
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";
import { lezer } from "@lezer/generator/rollup";

export default defineConfig(({ command, mode }) => {
    const env = loadEnv(mode, process.cwd(), "");

    return {
        build: {
            sourcemap: true,
        },
        plugins: [
            react(),
            lezer(),
            wasm(),
            topLevelAwait(),
            env.CI != null
                ? sentryVitePlugin({
                      org: env.SENTRY_ORG,
                      project: env.SENTRY_PROJECT,
                      authToken: env.SENTRY_AUTH_TOKEN,
                      sourcemaps: {
                          assets: "./dist/**",
                      },
                  })
                : null,
        ],
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
                    ? "https://wipple.dev/playground/"
                    : "http://localhost:8080/playground/"
                : "/playground",
        define: {
            "process.env.NODE_ENV": '"production"',
        },
    };
});
