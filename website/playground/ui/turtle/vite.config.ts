import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import path from "path";
import pkg from "./package.json";

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [react()],
    build: {
        lib: {
            entry: path.resolve(__dirname, "src/main.tsx"),
            name: pkg.name,
            fileName: "index",
        },
    },

    define: {
        "process.env": {},
    },
});
