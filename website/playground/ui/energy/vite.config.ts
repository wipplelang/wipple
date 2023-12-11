import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import commonjsExternals from "vite-plugin-commonjs-externals";
import cssInjectedByJsPlugin from "vite-plugin-css-injected-by-js";
import path from "path";
import pkg from "./package.json";

const externals = ["electron"];

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [react(), commonjsExternals({ externals }), cssInjectedByJsPlugin()],
    build: {
        lib: {
            entry: path.resolve(__dirname, "src/main.tsx"),
            name: pkg.name,
            fileName: "index",
        },
        rollupOptions: {
            output: {
                manualChunks: undefined,
            },
        },
    },
    optimizeDeps: {
        exclude: externals,
    },
    define: {
        "process.env.NODE_ENV": '"production"',
    },
});
