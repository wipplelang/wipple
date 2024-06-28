import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import dts from "vite-plugin-dts";
import { lezer } from "@lezer/generator/rollup";
import * as markdown from "vite-plugin-markdown";

// https://vitejs.dev/config/
export default defineConfig({
    build: {
        lib: {
            entry: "src/index.ts",
            fileName: "index",
            formats: ["es"],
        },
        sourcemap: true,
        rollupOptions: {
            external: ["react", "react-dom"],
        },
    },
    plugins: [
        react(),
        dts({ tsconfigPath: "./tsconfig.app.json", rollupTypes: true }),
        lezer(),
        markdown.plugin({ mode: [markdown.Mode.MARKDOWN] }),
    ],
});
