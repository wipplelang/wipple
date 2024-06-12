import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import { lezer } from "@lezer/generator/rollup";
import svgr from "vite-plugin-svgr";
import topLevelAwait from "vite-plugin-top-level-await";

// https://vitejs.dev/config/
export default defineConfig({
    base: "/playground",
    plugins: [react(), lezer(), svgr(), topLevelAwait()],
    server: {
        port: 8080,
    },
});
