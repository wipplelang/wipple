import { defineConfig } from "vite";
import path from "path";
import pkg from "./package.json";

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [],
    build: {
        lib: {
            entry: path.resolve(__dirname, "src/main.ts"),
            name: pkg.name,
            fileName: "index",
        },
    },
    define: {
        "process.env.NODE_ENV": '"production"',
    },
});
