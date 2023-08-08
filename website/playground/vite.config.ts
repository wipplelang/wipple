import { defineConfig, loadEnv } from "vite";
import react from "@vitejs/plugin-react-swc";
import { sentryVitePlugin } from "@sentry/vite-plugin";
import { lezer } from "@lezer/generator/rollup";
import rust from "@wasm-tool/rollup-plugin-rust";
import { rustOptions } from "../shared/build";

export default defineConfig(({ command, mode }) => {
    const env = { ...process.env, ...loadEnv(mode, process.cwd(), "") };

    return {
        build: {
            sourcemap: true,
        },
        plugins: [
            react(),
            lezer(),
            rust(rustOptions(env)),
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
            plugins: [rust(rustOptions(env))],
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
