import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import commonjsExternals from "vite-plugin-commonjs-externals";
import path from "path";
import pkg from "./package.json";

const externals = ["electron"];

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [react(), commonjsExternals({ externals })],
    build: {
        lib: {
            entry: path.resolve(__dirname, "src/main.tsx"),
            name: pkg.name,
            fileName: "index",
        },
    },
    optimizeDeps: {
        exclude: externals,
    },
    define: {
        "process.env.NODE_ENV": '"production"',
    },
});
