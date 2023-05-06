import { defineConfig, loadEnv } from "vite";
import react from "@vitejs/plugin-react-swc";
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";
import { sentryVitePlugin } from "@sentry/vite-plugin";

export default defineConfig(({ command, mode }) => {
    const env = loadEnv(mode, process.cwd(), "");

    return {
        build: {
            sourcemap: true,
        },
        plugins: [
            react(),
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
            format: "es",
            plugins: [wasm(), topLevelAwait()],
        },
        server: {
            port: 3000,
        },
        base: "/playground",
    };
});
