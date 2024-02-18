import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import { lezer } from "@lezer/generator/rollup";
import svgr from "vite-plugin-svgr";

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [react(), lezer(), svgr()],
    server: {
        port: 8080,
    },
});
